{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Eval.Eval (
    initEvalState,
    posEval,
    posExactEval,
    gamePhase,
) where

import Data.Array.Base (unsafeAt)
import Data.Bits
import Data.List (minimumBy)
import Data.Array.Unboxed
import Data.Ord (comparing)
import Data.Int

import Struct.Struct
import Struct.Status
import Struct.Config
import Struct.MidEnd
import Moves.Moves
import Moves.BitBoard
import Moves.Pattern
import Eval.BasicEval (matPiece1)

------------------------------------------------------------------
-- Parameters of this module ------------
granCoarse, granCoarse2, granCoarseM, shift2Cp :: Int
granCoarse    = 4	-- coarse granularity
granCoarse2   = granCoarse `div` 2
granCoarseM   = complement (granCoarse - 1)
shift2Cp      = 3	-- we have 2^shift2Cp units per centipawn
-----------------------------------------------

initEvalState :: [(String, Double)] -> EvalState
initEvalState sds = EvalState {
        esEParams  = npSetParm (colParams sds :: CollectFor EvalParams),
        esEWeights = npSetParm (colParams sds :: CollectFor EvalWeights)
    }

matesc :: Int
matesc = 20000 - 255	-- warning, this is also defined in Base.hs!!

useSpecials :: Bool
useSpecials = True

{-# INLINE posEval #-}
posEval :: MyPos -> EvalState -> Int
posEval p !sti = scc
    where !sce | useSpecials = evalDispatch          p sti
               | otherwise   = normalEval NotMatched p sti
          !scl = min matesc $ max (-matesc) sce
          !scc = if granCoarse > 0 then (scl + granCoarse2) .&. granCoarseM else scl

-- Don't use specials for tuning
{-# INLINE posExactEval #-}
posExactEval :: MyPos -> EvalState -> Int
posExactEval = normalEval NotMatched

data SpecialResult = NotMatched | Score Int | Scale Int

type SpecialEval = MyPos -> SpecialResult

evalDispatch :: MyPos -> EvalState -> Int
evalDispatch p sti
    = case trySpecials p (enp ++ peg) of
          Score s -> s
          sr      -> normalEval sr p sti
    where -- No pawns at all
          enp  | pawns p == 0 = [evalNoPawns]
               | otherwise    = []
          -- Pawns end game
          peg  | kings p .|. pawns p == occup p = [pawnEndGame]
               | otherwise    = []

-- Try a list of special eval functions
-- The first match will be the result of the whole try
-- If matched, a special eval function can return a score or a scale
trySpecials :: MyPos -> [SpecialEval] -> SpecialResult
trySpecials p = go
    where go []       = NotMatched
          go (se:ses) = case se p of
                            NotMatched -> go ses
                            sr         -> sr

-- In the normal evaluation (features & weights) the calculation is in
-- units of 1/8 centipawns, so when we return the normal score (in centipawns)
-- we must divide again by 8 (see shift2Cp)
normalEval :: SpecialResult -> MyPos -> EvalState -> Int
normalEval sr p !sti = ((sc * (100 - get50Moves p)) `div` 100) `unsafeShiftR` (shift2Cp + 8)
    where ep     = esEParams  sti
          ew     = esEWeights sti
          !gph   = gamePhase p
          !nev = foldr ($) (MidEnd 0 0) [
                   materDiff p ew
                 , evalBishops p ew
                 , evalRookPawn p ew
                 , kingSafe p ew
                 , kingPlace ep p ew
                 , mobiLity p ew
                 , centerDiff p ew
                 , spaceDiff p ew
                 , adversDiff p ew
                 , evalRookPlc p ew
                 , enPrise p ew
                 , pawnBl p ew
                 , isolDiff p ew
                 , backDiff p ew
                 , advPawns p ew
                 , passPawns gph ep p ew
              ]
          !eg = (end nev * scaleFactor p (end nev) sr) `unsafeShiftR` 6
          !sc = (mid nev + epMovingMid ep) * gph + (eg + epMovingEnd ep) * (256 - gph)

gamePhase :: MyPos -> Int
gamePhase p = g
    where qs = popCount $ queens p
          rs = popCount $ rooks p
          bs = popCount $ bishops p
          ns = popCount $ knights p
          !g = qs * 39 + rs * 20 + (bs + ns) * 12	-- opening: 254, end: 0

-- For specific material configurations which reduce the chance of winning
-- Not reduced: 64
scaleFactor :: MyPos -> Int -> SpecialResult -> Int
scaleFactor p eg sr
    | eg == 0                                             = 64
    | pawnsWin == 0
      && nonPawnWin - nonPawnLos <= bishopMg              = noPawnsLowMatDiff nonPawnWin nonPawnLos
    | oppoBishops
      && nonPawnWin == bishopMg && nonPawnLos == bishopMg = 18 + 4 * (popCount $ passed p .&. winPart)
    | oppoBishops                                         = 22 + 3 * popCount winPart
    | nonPawnWin == rookMg && nonPawnLos == rookMg
      && pawnCountWin - pawnCountLos <= 1                 = rookEndgame winPawnsOneFlank losKingOnOwnPawn
    | popCount (queens p) == 1                            = oneQueen p winPart losPart
    -- | otherwise                                           = min sf (36 + 7 * pawnCountWin)	-- this seems too small! For example KRPKB
    | otherwise                                           = sf
    where !winPart | eg > 0    = me p
                   | otherwise = yo p
          !pawnsWin = pawns p .&. winPart
          pawnCountWin = popCount pawnsWin
          bishopMg = matPiece1 Bishop
          rookMg   = matPiece1 Rook
          nonPawnWin = npMat winPart
          losPart = occup p `less` winPart
          nonPawnLos = npMat losPart
          pawnsLos = pawns p .&. losPart
          pawnCountLos = popCount pawnsLos
          losKingOnOwnPawn | eg > 0    = yoKAttacs p .&. pawnsLos
                           | otherwise = myKAttacs p .&. pawnsLos
          oppoBishops | popCount (bishops p) /= 2          = False
                      | popCount (bishops p .&. me p) /= 1 = False
                      | otherwise = popCount (bishops p .&. darkSquares)  == 1
                                 && popCount (bishops p .&. lightSquares) == 1
          npMat part = matPiece1 Queen  * popCount (queens  p .&. part)
                     + rookMg           * popCount (rooks   p .&. part)
                     + bishopMg         * popCount (bishops p .&. part)
                     + matPiece1 Knight * popCount (knights p .&. part)
          sf = case sr of
                   Scale s -> s
                   _       -> 64
          queenSide = fileA .|. fileB .|. fileC .|. fileD
          kingSide  = fileE .|. fileF .|. fileG .|. fileH
          -- pawnsOnBothFlanks = pawns p .&. queenSide /= 0 && pawns p .&. kingSide /= 0
          winPawnsOneFlank  = pawnsWin .&. queenSide == 0 || pawnsWin .&. kingSide == 0

noPawnsLowMatDiff :: Int -> Int -> Int
noPawnsLowMatDiff npWin npLos
    | npWin <  matPiece1 Rook   =  0
    | npLos <= matPiece1 Bishop =  4
    | otherwise                 = 14

rookEndgame :: Bool -> BBoard ->Int
rookEndgame winponeflank loskp
    | winponeflank && loskp /= 0 = 36
    | otherwise                  = 64

oneQueen :: MyPos -> BBoard -> BBoard -> Int
oneQueen p winp losp
    | queens p .&. winp /= 0 = 37 + 3 * popCount (losp .&. (bishops p .|. knights p))
    | otherwise              = 37 + 3 * popCount (winp .&. (bishops p .|. knights p))

-- These evaluation function distiguishes between some known finals with no pawns
evalNoPawns :: SpecialEval
evalNoPawns p
    | kings p == occup p = Score 0
    | onealone           = trySpecials p [kingAlone kaloneyo]
    | otherwise          = NotMatched
    where onealone = kaloneme || kaloneyo
          kaloneme = me p .&. kings p == me p
          kaloneyo = yo p .&. kings p == yo p

kingAlone :: Bool -> SpecialEval
kingAlone mywin p
    | majorcnt >  0 = trySpecials p [mateKMajxK mywin]
    | minorcnt == 2 = trySpecials p (knnk ++ kbbk ++ kbnk) -- the order is important here!
    | minorcnt == 1 = Score 0
    | otherwise     = NotMatched	-- here should be maybe same like KBBK
    where knnk | bishops p == 0 = [const (Score 0)]
               | otherwise      = []
          kbbk | knights p == 0 = [mateKBBK mywin]
               | otherwise      = []
          kbnk = [mateKBNK mywin]
          minorcnt = popCount $ bishops p .|. knights p
          majorcnt = popCount $ queens  p .|. rooks   p

winBonus :: Int
winBonus = 1200	-- known win

mateKBBK :: Bool -> SpecialEval
mateKBBK = scoreToMate pushToEdge

-- It seems that with 2 bishops or 1 major it's the same
-- rule to go to mate
mateKMajxK :: Bool -> SpecialEval
mateKMajxK = mateKBBK

mateKBNK :: Bool -> SpecialEval
mateKBNK mywin p
    | bishops p .&. lightSquares == 0 = scoreToMate pushToDarkCorner  mywin p
    | otherwise                       = scoreToMate pushToLightCorner mywin p

-- Make a score to mate the alone king
-- Given: a function to drive the weak king to some edge or corner (higher value means better score)
-- and which king is weak (mywin = True means: yours is weak)
-- The strong king must go near the weak king too
scoreToMate :: (Square -> Int) -> Bool -> SpecialEval
scoreToMate f mywin p = Score msc
    where !kweak | mywin     = ky
                 | otherwise = km
          !km = kingSquare (kings p) (me p)
          !ky = kingSquare (kings p) (yo p)
          !distk = squareDistance km ky	-- distance between kings - drive to low
          !distc = f kweak	-- driving function: drive to high
          !sc  = (winBonus + distc * distc - distk * distk) * 10
          !mtr = if moving p == White then mater p else -(mater p)
          !wsc = if mywin then sc else -sc
          !msc = (mtr `unsafeShiftL` shift2Cp) + wsc	-- here the score is in centipawns

{-
squareDistArr :: UArray Int Int32
squareDistArr = array (0, 64*64-1) [(sqSqIdx s1 s2, squareDist s1 s2) | s1 <- [0..63], s2 <- [0..63]]
    where squareDist f t = max (abs (fr - tr)) (abs (fc - tc))
              where (fr, fc) = rankFile $ fromIntegral f
                    (tr, tc) = rankFile $ fromIntegral t

squareDistance :: Square -> Square -> Int
squareDistance !sq1 !sq2 = fromIntegral $ squareDistArr `unsafeAt` sqSqIdx sq1 sq2

sqSqIdx :: Square -> Square -> Int
sqSqIdx !sq1 !sq2 = (sq1 `unsafeShiftL` 6) + sq2
-}

{-# INLINE squareDistance #-}
squareDistance :: Square -> Square -> Int
squareDistance sq1 sq2 = max (abs $ r1 - r2) (abs $ f1 - f2)
    where (r1, f1) = rankFile $ fromIntegral sq1
          (r2, f2) = rankFile $ fromIntegral sq2

-- Distance to en adge on rank of file (i.e. 0 to 7) - take the nearest edge
edgeDistance :: Int -> Int
edgeDistance r = min r (7 - r)

-- pushTo... - used in special evaluations
-- pushToEdge - drive the weak king to an edge
-- Values: 0 (in center), 5, 8, 9 (on egde)
pushToEdge :: Square -> Int
pushToEdge sq = 9 - md * md
    where (r, f) = rankFile sq
          rd = edgeDistance r
          fd = edgeDistance f
          md = min rd fd

-- pushToLightCorner: for KBNK end games with light color bishop
-- push the weak king to A1 or H8
-- Values: 0 (on diagonale A8H1) to 7 (on A1 or H8)
pushToDarkCorner :: Square -> Int
pushToDarkCorner sq = abs $ r + f - 7
    where (r, f) = rankFile sq

-- pushToLightCorner: for KBNK end games with light color bishop
-- push the weak king to A8 or H1
-- Values: 0 (on diagonale A1H8) to 7 (on A8 or H1)
pushToLightCorner :: Square -> Int
pushToLightCorner sq = abs $ r - f
    where (r, f) = rankFile sq

----------------------------------------------------------------------------
-- Here we have the implementation of the evaluation items
----------------------------------------------------------------------------

------ King Safety ------
kingSafe :: MyPos -> EvalWeights -> MidEnd -> MidEnd
kingSafe p !ew = mad (ewKingSafe ew) ksafe
    where !ksafe = ksSide (yo p) (yoKAttacs p) (myPAttacs p) (myNAttacs p) (myBAttacs p) (myRAttacs p) 
                          (myQAttacs p) (myKAttacs p) (myAttacs p)
                 - ksSide (me p) (myKAttacs p) (yoPAttacs p) (yoNAttacs p) (yoBAttacs p) (yoRAttacs p) 
                          (yoQAttacs p) (yoKAttacs p) (yoAttacs p)

-- To make the sum and count in one pass
data Flc = Flc !Int !Int

fadd :: Flc -> Flc -> Flc
fadd (Flc f1 q1) (Flc f2 q2) = Flc (f1+f2) (q1+q2)

ksSide :: BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> Int
ksSide !yop !yok !myp !myn !myb !myr !myq !myk !mya
    | myq == 0  = 0
    | otherwise = mattacs
    where qual a p
              | yoka == 0 = Flc 0 0
              | y == 1    = Flc 1 p
              | y == 2    = Flc 1 (p `unsafeShiftL` 1)
              | y == 3    = Flc 1 (p `unsafeShiftL` 2)
              | otherwise = Flc 1 (p `unsafeShiftL` 3)
              where !yoka = yok .&. a
                    y = popCount yoka
          -- qualWeights = [1, 3, 3, 5, 7, 3]
          !qp = qual myp 1
          !qn = qual myn 3
          !qb = qual myb 3
          !qr = qual myr 5
          !qq = qual myq 7
          !qk = qual myk 3
          !(Flc c q) = fadd qp $ fadd qn $ fadd qb $ fadd qr $ fadd qq qk
          !mattacs
              | c == 0 = 0
              | otherwise = fromIntegral $ attCoef `unsafeAt` ixt
              -- where !freey = popCount $ yok `less` (mya .|. yop)
              --       !conce = popCount $ yok .&. mya
              -- This is equivalent to:
              where !freco = popCount $ yok `less` (yop `less` mya)
                    !ixm = c * q `unsafeShiftR` 2
                    !ixt = ixm + c + ksShift - freco
                    ksShift = 13

-- We take the maximum of 283 because:
-- Quali max: 8 * (1 + 3 + 3 + 5 + 7 + 3) = 176
-- Flag max: 6
-- 6 * 176 / 4 + 6 + 13 = 283
attCoef :: UArray Int Int32
attCoef = listArray (0, 283) $ take zeros (repeat 0) ++ [ f x | x <- [0..63] ] ++ repeat (f 63)
    where -- Without the scaling, f will take max value of 4000 for 63
          f :: Int -> Int32
          f x = let y = fromIntegral x :: Double
                in round $ maxks * (2.92968750 - 0.03051758*y)*y*y / 4000
          zeros = 8
          maxks = 4500

kingSquare :: BBoard -> BBoard -> Square
kingSquare kingsb colorp = firstOne $ kingsb .&. colorp
{-# INLINE kingSquare #-}

------ Material ------

materDiff :: MyPos -> EvalWeights -> MidEnd -> MidEnd
materDiff p !ew = mad (ewMaterialDiff ew) md
    where !md | moving p == White =   mater p
              | otherwise         = - mater p

------ King placement and opennes ------

-- Depending on which pieces are on the board we have some preferences
-- where the king should be placed. For example, in the opening and middle game it should
-- be in some corner, in endgame it should be near some (passed) pawn(s)
-- We calculate the king opennes here, as we have all we need
-- We also give a bonus for a king beeing near pawn(s)
kingPlace :: EvalParams -> MyPos -> EvalWeights -> MidEnd -> MidEnd
kingPlace ep p !ew = mad (ewKingPawn      ew) kpa .
                     mad (ewKingThreat    ew) ktr .
                     mad (ewKingOpen      ew) ko .
                     mad (ewKingPlaceCent ew) kcd .
                     mad (ewKingPlacePwns ew) kpd
    where !kcd = (mpl - ypl) `unsafeShiftR` epMaterBonusScale ep
          !kpd = (mpi - ypi) `unsafeShiftR` epPawnBonusScale  ep
          !mks = kingSquare (kings p) $ me p
          !yks = kingSquare (kings p) $ yo p
          !mkm = materFun yminor yrooks yqueens
          !ykm = materFun mminor mrooks mqueens
          (!mpl, !ypl, !mpi, !ypi)
              | moving p == White = ( kingMaterBonus yqueens White mpawns mkm mks
                                    , kingMaterBonus mqueens Black ypawns ykm yks
                                    , kingPawnsBonus mks mpassed ypassed
                                    , kingPawnsBonus yks mpassed ypassed
                                    )
              | otherwise         = ( kingMaterBonus yqueens Black mpawns mkm mks
                                    , kingMaterBonus mqueens White ypawns ykm yks
                                    , kingPawnsBonus mks ypassed mpassed
                                    , kingPawnsBonus yks ypassed mpassed
                                    )
          !mrooks  = popCount $ rooks p .&. me p
          !mqueens = popCount $ queens p .&. me p
          !mminor  = popCount $ (bishops p .|. knights p) .&. me p
          !yrooks  = popCount $ rooks p .&. yo p
          !yqueens = popCount $ queens p .&. yo p
          !yminor  = popCount $ (bishops p .|. knights p) .&. yo p
          !mpawns  = pawns p .&. me p
          !ypawns  = pawns p .&. yo p
          !mpassed = passed p .&. me p
          !ypassed = passed p .&. yo p
          materFun m r q = (m * epMaterMinor ep + r * epMaterRook ep + q * epMaterQueen ep)
                               `unsafeShiftR` epMaterScale ep
          !ko = adv - own
          mwb = popCount $ bAttacs (pawns p) mks .&. nopawns
          mwr = popCount $ rAttacs (pawns p) mks .&. nopawns
          ywb = popCount $ bAttacs (pawns p) yks .&. nopawns
          ywr = popCount $ rAttacs (pawns p) yks .&. nopawns
          nopawns = complement $ pawns p
          comb !oR !oQ !wb !wr = let r = oR * wr
                                     q = oQ * (wb + wr)
                                 in r + q*q
          own = comb yrooks yqueens mwb mwr
          adv = comb mrooks mqueens ywb ywr
          -- King on pawns: more is better (now linear)
          pmkpa = popCount (myKAttacs p .&. pawns p)
          pykpa = popCount (yoKAttacs p .&. pawns p)
          !kpa = pmkpa - pykpa
          -- Threat by king only pieces:
          -- pieces attacked by king and not defended by a pawn
          pmktr = popCount (myKAttacs p .&. yo p .&. nopawns `less` yoPAttacs p)
          pyktr = popCount (yoKAttacs p .&. me p .&. nopawns `less` myPAttacs p)
          !ktr = pmktr - pyktr

-- Rank & File of a square
rankOf :: Square -> Int
rankOf s = s `unsafeShiftR` 3

fileOf :: Square -> Int
fileOf s = s .&. 7

rankFile :: Square -> (Int, Int)
rankFile sq = sq `divMod` 8

promoW, promoB :: Square -> Square
promoW s = fileOf s + 56
promoB s = fileOf s

-- We give bonus also for pawn promotion squares, if the pawn is near enough to promote
-- Give as parameter bitboards for all pawns, white pawns and black pawns for performance
kingPawnsBonus :: Square -> BBoard -> BBoard -> Int
kingPawnsBonus !ksq !wpass !bpass = bonus
    where wns = bbToSquares (wpass `unsafeShiftL` 8)
          bns = bbToSquares (bpass `unsafeShiftR` 8)
          !bpsqs = sum $ map (pawnBonus . squareDistance ksq) $ wns ++ bns
          !bqsqs = sum $ map (pawnBonus . squareDistance ksq)
                       $ map promoW (bbToSquares wpass) ++ map promoB (bbToSquares bpass)
          !bonus = bpsqs + bqsqs

-- This is a bonus for the king beeing near one corner
-- It's bigger when the enemy has more material (only pieces)
-- and when that corner has a pawn shelter
kingMaterBonus :: Int -> Color -> BBoard -> Int -> Square -> Int
kingMaterBonus !qs c !myp !mat !ksq
    | qs == 0   = 0
    | otherwise = kMatBonus c myp mat ksq

kMatBonus :: Color -> BBoard -> Int -> Square -> Int
kMatBonus c !myp !mat !ksq
    | c == White = matFactor mat * prxw
    | otherwise  = matFactor mat * prxb
    where !prxw = prxWA + prxWH
          !prxb = prxBA + prxBH
          !prxWA = (unsafeShiftL (opawns shWA2) 1 + opawns shWA3) * (prxBoQ wa + prxBo wb)
          !prxWH = (unsafeShiftL (opawns shWH2) 1 + opawns shWH3) * (prxBoQ wh + prxBo wg)
          !prxBA = (unsafeShiftL (opawns shBA7) 1 + opawns shBA6) * (prxBoQ ba + prxBo bb)
          !prxBH = (unsafeShiftL (opawns shBH7) 1 + opawns shBH6) * (prxBoQ bh + prxBo bg)
          opawns = popCount . (.&. myp)
          prxBo  = proxyBonus . squareDistance ksq
          prxBoQ = flip unsafeShiftR 2 . prxBo
          matFactor = unsafeAt matKCArr
          -- The interesting squares and bitboards about king placement
          wa = 0
          wb = 1
          wg = 6
          wh = 7
          ba = 56
          bb = 57
          bg = 62
          bh = 63
          shWA2 = row2 .&. (fileA .|. fileB .|. fileC)
          shWA3 = row3 .&. (fileA .|. fileB .|. fileC)
          shWH2 = row2 .&. (fileF .|. fileG .|. fileH)
          shWH3 = row3 .&. (fileF .|. fileG .|. fileH)
          shBA6 = row6 .&. (fileA .|. fileB .|. fileC)
          shBA7 = row7 .&. (fileA .|. fileB .|. fileC)
          shBH6 = row6 .&. (fileF .|. fileG .|. fileH)
          shBH7 = row7 .&. (fileF .|. fileG .|. fileH)

-- Make it longer, for artificially increased distances
proxyBonusArr :: UArray Int Int    -- 0   1  2  3  4  5  6  7
proxyBonusArr = listArray (0, 15) $ [55, 20, 8, 4, 3, 2, 1] ++ repeat 0

pawnBonusArr :: UArray Int Int     -- 0    1   2   3   4   5  6  7
pawnBonusArr = listArray (0, 15) $ [220, 120, 70, 35, 23, 14, 7] ++ repeat 0

proxyBonus :: Int -> Int
proxyBonus = unsafeAt proxyBonusArr

pawnBonus :: Int -> Int
pawnBonus = unsafeAt pawnBonusArr

matKCArr :: UArray Int Int   -- 0              5             10
matKCArr = listArray (0, 63) $ [0, 0, 0, 1, 1, 2, 3, 4, 5, 7, 9, 10, 11, 12] ++ repeat 12

------ Rook placement points ------

evalRookPlc :: MyPos -> EvalWeights -> MidEnd -> MidEnd
evalRookPlc p !ew = mad (ewRook7th   ew) r7 .
                    mad (ewRookHOpen ew) ho .
                    mad (ewRookOpen  ew) op .
                    mad (ewRookConn  ew) rc
    where !mRs = rooks p .&. me p
          !mPs = pawns p .&. me p
          (mho, mop) = foldr (perRook (pawns p) mPs) (0, 0) $ bbToSquares mRs
          !yRs = rooks p .&. yo p
          !yPs = pawns p .&. yo p
          (yho, yop) = foldr (perRook (pawns p) yPs) (0, 0) $ bbToSquares yRs
          !ho = mho - yho
          !op = mop - yop
          !mrc | myRAttacs p .&. me p .&. rooks p == 0 = 0
               | otherwise                             = 1
          !yrc | yoRAttacs p .&. yo p .&. rooks p == 0 = 0
               | otherwise                             = 1
          !rc = mrc - yrc
          !r7 = r7m - r7y
          (!my7, !my8, !yo7, !yo8) | moving p == White = (row7, row8, row2, row1)
                                   | otherwise         = (row2, row1, row7, row8)
          !r7m | yo p .&. kings p .&. my8 == 0 = 0
               | otherwise                     = popCount $ me p .&. rooks p .&. my7
          !r7y | me p .&. kings p .&. yo8 == 0 = 0
               | otherwise                     = popCount $ yo p .&. rooks p .&. yo7

perRook :: BBoard -> BBoard -> Square -> (Int, Int) -> (Int, Int)
perRook allp myp rsq (ho, op)
    | rco .&. allp == 0 = (ho,  op')
    | rco .&. myp  == 0 = (ho', op)
    | otherwise         = (ho,  op)
    where !rco = rcolls `unsafeAt` (rsq .&. 0x7)
          ho'  = ho + 1
          op'  = op + 1
          rcolls :: UArray Int BBoard
          rcolls = listArray (0, 7) [ fileA, fileB, fileC, fileD, fileE, fileF, fileG, fileH ]

------ Mobility ------

mobiLity :: MyPos -> EvalWeights -> MidEnd -> MidEnd
mobiLity p ew mide
    | moving p == White = mobDiff p r23 r67 pbw pbb ew mide
    | otherwise         = mobDiff p r67 r23 pbb pbw ew mide
    where r23 = row2 .|. row3
          r67 = row6 .|. row7
          pbw = occup p `unsafeShiftR` 8
          pbb = occup p `unsafeShiftL` 8

-- No pawn mobility (which, calculated as attacks, is useless)
-- Mobility inspired by Stockfish, with mobility aria
mobDiff :: MyPos -> BBoard -> BBoard -> BBoard -> BBoard -> EvalWeights -> MidEnd -> MidEnd
mobDiff p mylr yolr mypb yopb ew = mad (ewMobilityKnight ew) n .
                                   mad (ewMobilityBishop ew) b .
                                   mad (ewMobilityRook   ew) r .
                                   mad (ewMobilityQueen  ew) q
    where !myPMA = me p .&. pawns p .&. (mylr .|. mypb)
          !yoPMA = yo p .&. pawns p .&. (yolr .|. yopb)
          !myMA  = complement $ myPMA .|. (kings p .&. me p) .|. yoPAttacs p
          !yoMA  = complement $ yoPMA .|. (kings p .&. yo p) .|. myPAttacs p
          !myN = popCount $ myNAttacs p .&. myMA
          !myB = popCount $ myBAttacs p .&. myMA
          !myR = popCount $ myRAttacs p .&. myMA
          !myQ = popCount $ myQAttacs p .&. myMA
          !yoN = popCount $ yoNAttacs p .&. yoMA
          !yoB = popCount $ yoBAttacs p .&. yoMA
          !yoR = popCount $ yoRAttacs p .&. yoMA
          !yoQ = popCount $ yoQAttacs p .&. yoMA
          !n = myN - yoN
          !b = myB - yoB
          !r = myR - yoR
          !q = myQ - yoQ

------ Center control ------

-- This function is already optimised
-- King & minors: take extended center
-- For knights: also position on extended center
centerDiff :: MyPos -> EvalWeights -> MidEnd -> MidEnd
centerDiff p !ew = mad (ewCenterPAtts ew) pd .
                   mad (ewCenterNAtts ew) nd .
                   mad (ewCenterNOcc  ew) no .
                   mad (ewCenterBAtts ew) bd .
                   mad (ewCenterRAtts ew) rd .
                   mad (ewCenterQAtts ew) qd .
                   mad (ewCenterKAtts ew) kd
    where !mpa = popCount $ myPAttacs p .&. center
          !ypa = popCount $ yoPAttacs p .&. center
          !pd  = mpa - ypa
          !mna = popCount $ myNAttacs p .&. excent
          !yna = popCount $ yoNAttacs p .&. excent
          !nd  = mna - yna
          !mno = popCount $ me p .&. excent .&. knights p
          !yno = popCount $ yo p .&. excent .&. knights p
          !no  = mno - yno
          !mba = popCount $ myBAttacs p .&. excent
          !yba = popCount $ yoBAttacs p .&. excent
          !bd  = mba - yba
          !mra = popCount $ myRAttacs p .&. center
          !yra = popCount $ yoRAttacs p .&. center
          !rd  = mra - yra
          !mqa = popCount $ myQAttacs p .&. center
          !yqa = popCount $ yoQAttacs p .&. center
          !qd  = mqa - yqa
          !mka = popCount $ myKAttacs p .&. excent
          !yka = popCount $ yoKAttacs p .&. excent
          !kd  = mka - yka
          center = 0x0000003C3C000000
          excent = 0x00003C3C3C3C0000

-------- Space for own pieces in our courtyard -----------

spaceDiff :: MyPos -> EvalWeights -> MidEnd -> MidEnd
spaceDiff p !ew = mad (ewSpace ew) sd
    where !sd = ms - ys
          (ms, ys)
              | moving p == White = (
                  spaceWhite (pawns p .&. me p) (myAttacs p) (yoPAttacs p) (yoAttacs p),
                  spaceBlack (pawns p .&. yo p) (yoAttacs p) (myPAttacs p) (myAttacs p)
                  )
              | otherwise = (
                  spaceBlack (pawns p .&. me p) (myAttacs p) (yoPAttacs p) (yoAttacs p),
                  spaceWhite (pawns p .&. yo p) (yoAttacs p) (myPAttacs p) (myAttacs p)
                  )

{-# INLINE spaceWhite #-}
spaceWhite :: BBoard -> BBoard -> BBoard -> BBoard -> Int
spaceWhite !mpawns !matts !ypatts !yatts = sv
    where yard = (fileC .|. fileD .|. fileE .|. fileF) .&. (row2 .|. row3 .|. row4)
          safe = (yard .&. (matts .|. complement yatts)) `less` (mpawns .|. ypatts)
          behi = shadowDown mpawns
          spa = popCount $ (safe `unsafeShiftL` 32) .|. (behi .&. safe)
          !sv = fromIntegral $ spaceVals `unsafeAt` spa

{-# INLINE spaceBlack #-}
spaceBlack :: BBoard -> BBoard -> BBoard -> BBoard -> Int
spaceBlack !mpawns !matts !ypatts !yatts = sv
    where yard = (fileC .|. fileD .|. fileE .|. fileF) .&. (row7 .|. row6 .|. row5)
          safe = (yard .&. (matts .|. complement yatts)) `less` (mpawns .|. ypatts)
          behi = shadowUp mpawns
          spa = popCount $ (safe `unsafeShiftR` 32) .|. (behi .&. safe)
          !sv = fromIntegral $ spaceVals `unsafeAt` spa

-- Non linear space values:
spaceVals :: UArray Int Int32
spaceVals = listArray (0, 24) $ map f [1..25]
    where f x = round $ spf * (sqrt x - 1)
          spf = 270 :: Double

-------- Attacks to adverse squares ----------

adversDiff :: MyPos -> EvalWeights -> MidEnd -> MidEnd
adversDiff p !ew = mad (ewAdvAtts ew) ad .
                   mad (ewWeakSq  ew) ws .
                   mad (ewWeakSqA ew) wa
    where !ad = md - yd
          !md = popCount $ myAttacs p .&. yoH
          !yd = popCount $ yoAttacs p .&. myH
          !ws = popCount wsm - popCount wsy
          !wsm = myH `less` (myAttacs p .|. ah45)
          !wsy = yoH `less` (yoAttacs p .|. ah45)
          !wa  = wam - way
          !wam = popCount $ myAttacs p .&. wsy
          !way = popCount $ yoAttacs p .&. wsm
          (myH, yoH) | moving p == White = (ah14, ah58)
                     | otherwise         = (ah58, ah14)
          ah14 = 0xFFFFFFFF
          ah58 = 0xFFFFFFFF00000000
          ah45 = row4 .|. row5

-------- Isolated pawns --------

isolDiff :: MyPos -> EvalWeights -> MidEnd -> MidEnd
isolDiff p !ew = mad (ewIsolPawns  ew) nd .
                 mad (ewIsolPassed ew) pd
    where (!myr, !myp) = isol (pawns p .&. me p) (passed p)
          (!yor, !yop) = isol (pawns p .&. yo p) (passed p)
          !nd = myr - yor
          !pd = myp - yop

isol :: BBoard -> BBoard -> (Int, Int)
isol ps pp = (ris, pis)
    where !myp = ps .&. pp
          !myr = ps `less` myp
          !myf = bbLeft ps .|. bbRight ps
          !myu = myf `unsafeShiftL` 8
          !myd = myf `unsafeShiftR` 8
          !myc = myf .|. myu .|. myd
          !nomyc = complement myc
          !ris = popCount $ myr .&. nomyc
          !pis = popCount $ myp .&. nomyc

-------- Backward pawns --------

backDiff :: MyPos -> EvalWeights -> MidEnd -> MidEnd
backDiff p !ew mide
    | moving p == White
    = let wp = pawns p .&. me p
          bp = pawns p .&. yo p
          (bpw, bpow) = backPawns White wp bp (yoPAttacs p)
          (bpb, bpob) = backPawns Black bp wp (myPAttacs p)
          !bpd  = popCount bpw  - popCount bpb
          !bpod = popCount bpow - popCount bpob
      in mad (ewBackPawns ew) bpd $! mad (ewBackPOpen ew) bpod mide
    | otherwise
    = let bp = pawns p .&. me p
          wp = pawns p .&. yo p
          (bpw, bpow) = backPawns White wp bp (myPAttacs p)
          (bpb, bpob) = backPawns Black bp wp (yoPAttacs p)
          !bpd  = popCount bpb  - popCount bpw
          !bpod = popCount bpob - popCount bpow
      in mad (ewBackPawns ew) bpd  $! mad (ewBackPOpen ew) bpod mide

backPawns :: Color -> BBoard -> BBoard -> BBoard -> (BBoard, BBoard)
backPawns White !mp !op !opa = (bp, bpo)
    where fa = frontAttacksWhite mp
          stops = mp `unsafeShiftL` 8
          !bp  = stops .&. opa .&. complement fa;
          !bpo = bp `less` shadowDown op
backPawns Black !mp !op !opa = (bp, bpo)
    where fa = frontAttacksBlack mp
          stops = mp `unsafeShiftR` 8
          !bp = stops .&. opa .&. complement fa;
          !bpo = bp `less` shadowUp op

frontAttacksWhite :: BBoard -> BBoard
frontAttacksWhite !b = fa
    where fal = bbLeft b
          far = bbRight b
          !fa = shadowUp (fal .|. far)	-- shadowUp is exclusive the original!

frontAttacksBlack :: BBoard -> BBoard
frontAttacksBlack !b = fa
    where fal = bbLeft b
          far = bbRight b
          !fa = shadowDown (fal .|. far)	-- shadowUp is exclusive the original!

------ En prise ------
-- enpHanging and enpEnPrise optimised (only mean) with Clop by running 4222
-- games at 15+0.25 sec against pass3v, resulting in a Clop forecast of 62 +- 39 ELO
-- enpAttacked optimised (together with epMovingMid & epMovingEnd), only mean, with Clop
-- by 3712 games at 15+0.25 sec against pass3v, Clop forecast: 82 +- 40 ELO
-- enpHanging and enpEnPrise again optimised (only mean) with Clop by running 16300
-- games at 15+0.25 sec against pass3w, resulting in a Clop forecast of 63 +- 19 ELO

-- Here we should only take at least the opponent attacks! When we evaluate,
-- we are in one on this situations:
-- 1. we have no further capture and evaluate in a leaf
-- 2. we are evaluating for delta cut
-- In 1 we should take the opponent attacks and analyse them:
-- - if he has more than 2 attacks, than our sencond best attacked piece will be lost
-- (but not always, for example when we can check or can defent one with the other)
-- - if he has only one attack, we are somehow restricted to defend or move that piece
-- In 2 we have a more complicated analysis, which maybe is not worth to do
enPrise :: MyPos -> EvalWeights -> MidEnd -> MidEnd
enPrise p !ew = mad (ewEnpHanging  ew) ha .
                mad (ewEnpEnPrise  ew) ep .
                mad (ewEnpAttacked ew) at .
                mad (ewWepTotal    ew) wp .
                mad (ewWepAttacked ew) wa
    where !meP = me p .&. pawns   p	-- my pieces
          !meM = me p .&. (knights p .|. bishops p)
          !meR = me p .&. rooks   p
          !meQ = me p .&. queens  p
          !atP = meP  .&. yoAttacs p	-- my attacked pieces
          !atM = meM  .&. yoAttacs p
          !atR = meR  .&. yoAttacs p
          !atQ = meQ  .&. yoAttacs p
          !noma = complement $ myAttacs p
          !haP = atP .&. noma	-- attacked and not defended (hanging)
          !haM = atM .&. noma
          !haR = atR .&. noma
          !haQ = atQ .&. noma
          !epM = meM .&. yoPAttacs p	-- defended, but attacked by less valuable opponent pieces
          !epR = meR .&. yoA1
          !epQ = meQ .&. yoA2
          !yoA1 = yoPAttacs p .|. yoNAttacs p .|. yoBAttacs p
          !yoA2 = yoA1 .|. yoRAttacs p
          !ha = popCount haP + 3 * popCount haM + 5 * popCount haR + 9 * popCount haQ
          !ep =                3 * popCount epM + 5 * popCount epR + 9 * popCount epQ
          !at = popCount atP + 3 * popCount atM + 5 * popCount atR + 9 * popCount atQ
          -- Weak pawns: total & attacked
          !mwp = meP `less` myPAttacs p	-- my weak pawns
          !ywp = yo p .&. pawns p `less` yoPAttacs p	-- your weak pawns
          !mwa = mwp .&. yoAttacs p	-- my weak attacked pawns
          !ywa = ywp .&. myAttacs p	-- your weak attacked pawns
          !wp = popCount ywp - popCount mwp
          !wa = popCount ywa - popCount mwa

------ Bishops: bishop pair and bad bishops ------

-- This function is optimised
evalBishops :: MyPos -> EvalWeights -> MidEnd -> MidEnd
evalBishops p !ew = mad (ewBishopPawns ew) pa .
                    mad (ewBishopPair  ew) bp
    where !wbl = bishops p .&. me p .&. lightSquares
          !wbd = bishops p .&. me p .&. darkSquares
          !bbl = bishops p .&. yo p .&. lightSquares
          !bbd = bishops p .&. yo p .&. darkSquares
          !bpwl = popCount wbl
          !bpwd = popCount wbd
          !bpw = bpwl .&. bpwd	-- tricky here: exact 1 and 1 is ok
          !bpbl = popCount bbl
          !bpbd = popCount bbd
          !bpb = bpbl .&. bpbd	-- and here
          !bp  = bpw - bpb
          !mpal = bpwl * (popCount (pawns p .&. lightSquares) - pawnEven)
          !mpad = bpwd * (popCount (pawns p .&. darkSquares) - pawnEven)
          !ypal = bpbl * (popCount (pawns p .&. lightSquares) - pawnEven)
          !ypad = bpbd * (popCount (pawns p .&. darkSquares) - pawnEven)
          !pa = mpal + mpad - ypal - ypad
          pawnEven = 6

{--
------ Knight & Rook correction according to own pawns ------
data NRCorrection = NRCorrection

instance EvalItem NRCorrection where
    evalItem _ _ p _ = evalNRCorrection p
    evalItemNDL _  = [("nrCorrection", ((0, 0), (0, 8)))]

-- This function seems to be already optimised
evalNRCorrection :: MyPos -> [Int]
evalNRCorrection p = [md]
    where !wpc = popCount (pawns p .&. me p) - 5
          !bpc = popCount (pawns p .&. yo p) - 5
          !wnp = popCount (knights p .&. me p) * wpc * 6	-- 1/16 for each pawn over 5
          !bnp = popCount (knights p .&. yo p) * bpc * 6	-- 1/16 for each pawn over 5
          !wrp = - popCount (rooks p .&. me p) * wpc * 12	-- 1/8 for each pawn under 5
          !brp = - popCount (rooks p .&. yo p) * bpc * 12	-- 1/8 for each pawn under 5
          !md = wnp + wrp - bnp - brp
--}

------ Rook pawn weakness ------

-- This function is already optimised
evalRookPawn :: MyPos -> EvalWeights -> MidEnd -> MidEnd
evalRookPawn p !ew = mad (ewRookPawn ew) rps
    where !wrp = popCount $ pawns p .&. me p .&. rookFiles
          !brp = popCount $ pawns p .&. yo p .&. rookFiles
          !rps = wrp - brp

------ Blocked pawns ------

pawnBl :: MyPos -> EvalWeights -> MidEnd -> MidEnd
pawnBl p !ew mide
    | moving p == White = let (wp, wo, wa) = pawnBloWhite mer mef yof
                              (bp, bo, ba) = pawnBloBlack yor yof mef
                          in mad (ewPawnBlockP ew) (wp-bp) $!
                             mad (ewPawnBlockO ew) (wo-bo) $!
                             mad (ewPawnBlockA ew) (wa-ba) mide
    | otherwise         = let (wp, wo, wa) = pawnBloWhite yor yof mef
                              (bp, bo, ba) = pawnBloBlack mer mef yof
                          in mad (ewPawnBlockP ew) (bp-wp) $!
                             mad (ewPawnBlockO ew) (bo-wo) $!
                             mad (ewPawnBlockA ew) (ba-wa) mide
    where !mep = pawns p .&. me p	-- my pawns
          !mes = mep .&. passed p	-- my passed pawns
          !mer = mep `less` mes		-- my rest pawns
          !yop = pawns p .&. yo p	-- your pawns
          !yos = yop .&. passed p	-- your passed pawns
          !yor = yop `less` yos		-- your rest pawns
          !mef = me p `less` mep	-- my figures
          !yof = yo p `less` yop	-- your figures

cntPaBlo :: BBoard -> BBoard -> BBoard -> BBoard -> (Int, Int, Int)
cntPaBlo !ps !op !ofi !afi = (f op, f ofi, f afi)
    where f = popCount . (ps .&.)

pawnBloWhite :: BBoard -> BBoard -> BBoard -> (Int, Int, Int)
pawnBloWhite !pa !op !tp = cntPaBlo p1 pa op tp
    where !p1 = pa `unsafeShiftL` 8

pawnBloBlack :: BBoard -> BBoard -> BBoard -> (Int, Int, Int)
pawnBloBlack !pa !op !tp = cntPaBlo p1 pa op tp
    where !p1 = pa `unsafeShiftR` 8

------ Passed pawns ------

-- Every passed pawn will be evaluated separately
passPawns :: Int -> EvalParams -> MyPos -> EvalWeights -> MidEnd -> MidEnd
passPawns !gph ep p !ew = mad (ewPassPawnLev ew) dpp
    where !mppbb = passed p .&. me p
          !yppbb = passed p .&. yo p
          !myc = moving p
          !yoc = other myc
          !mypp = sum $ map (perPassedPawn gph ep p myc) $ bbToSquares mppbb
          !yopp = sum $ map (perPassedPawn gph ep p yoc) $ bbToSquares yppbb
          !dpp  = mypp - yopp

-- The value of the passed pawn depends answers to this questions:
-- - is it defended/attacked? by which pieces?
-- - how many squares ahead are blocked by own/opponent pieces?
-- - how many squares ahead are controlled by own/opponent pieces?
-- - does it has a rook behind?
perPassedPawn :: Int -> EvalParams -> MyPos -> Color -> Square -> Int
perPassedPawn !gph ep p c sq
    | attacked && not defended
        && c /= moving p = epPassMin ep	-- but if we have more than one like that?
    | otherwise          = perPassedPawnOk gph ep p c sq sqbb moi toi moia toia
    where !sqbb = 1 `unsafeShiftL` sq
          (!moi, !toi, !moia, !toia)
               | moving p == c = (me p, yo p, myAttacs p, yoAttacs p)
               | otherwise     = (yo p, me p, yoAttacs p, myAttacs p)
          !defended = moia .&. sqbb /= 0
          !attacked = toia .&. sqbb /= 0

perPassedPawnOk :: Int -> EvalParams -> MyPos -> Color -> Square -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> Int
perPassedPawnOk !gph ep p c sq sqbb moi toi moia toia = val
    where (!way, !behind, !asq)
              | c == White = (shadowUp sqbb, shadowDown sqbb, sq+8)
              | otherwise  = (shadowDown sqbb, shadowUp sqbb, sq-8)
          !mblo = popCount $ moi .&. way
          !yblo = popCount $ toi .&. way
          !rookBehind = behind .&. (rooks p .|. queens p)
          !mebehind = rookBehind .&. moi /= 0
                   && rookBehind .&. toi == 0
          !yobehind = rookBehind .&. moi == 0
                   && rookBehind .&. toi /= 0
          !bbmyctrl | mebehind  = way
                    | otherwise = moia .&. way
          !bbyoctrl | yobehind  = way `less` bbmyctrl
                    | otherwise = toia .&. (way `less` bbmyctrl)
          !myctrl = popCount bbmyctrl
          !yoctrl = popCount bbyoctrl
          !x = 8 - popCount way
          !pmax = (epPassA ep * x + epPassB ep) * x + epPassC ep
          !myking = kingSquare (kings p) moi
          !yoking = kingSquare (kings p) toi
          !mdis = squareDistance myking asq
          !ydis = squareDistance yoking asq
          !kingprx = (kdDist (mdis - ydis) * epPassKingProx ep * (256 - gph)) `unsafeShiftR` 8
          !val1 = (pmax * (128 - kingprx) * (128 - epPassBlockO ep * mblo)) `unsafeShiftR` 14
          !val2 = (val1 * (128 - epPassBlockA ep * yblo)) `unsafeShiftR` 7
          !val  = (val2 * (128 + epPassMyCtrl ep * myctrl) * (128 - epPassYoCtrl ep * yoctrl))
                    `unsafeShiftR` 14

kdDistArr :: UArray Int Int  --  -7 -6 -5 -4 -3 -2 -1  0  1  2  3  4  5  6  7
kdDistArr = listArray (0, 14) $ [-4,-3,-3,-3,-2,-2,-1, 0, 1, 2, 2, 3, 3, 3, 4]

kdDist :: Int -> Int
kdDist = (kdDistArr `unsafeAt`) . (7+)


------ Advanced pawns, on 6th & 7th rows (not passed) ------
 
advPawns :: MyPos -> EvalWeights -> MidEnd -> MidEnd
advPawns p !ew = mad (ewAdvPawn6 ew) ap6 .
                 mad (ewAdvPawn5 ew) ap5
    where !apbb  = pawns p `less` passed p
          !mapbb = apbb .&. me p
          !yapbb = apbb .&. yo p
          (my5, my6, yo5, yo6)
              | moving p == White = (0x000000FF00000000, 0x0000FF0000000000, 0xFF000000, 0xFF0000)
              | otherwise         = (0xFF000000, 0xFF0000, 0x000000FF00000000, 0x0000FF0000000000)
          !map5 = popCount $ mapbb .&. my5
          !map6 = popCount $ mapbb .&. my6
          !yap5 = popCount $ yapbb .&. yo5
          !yap6 = popCount $ yapbb .&. yo6
          !ap5  = map5 - yap5
          !ap6  = map6 - yap6

-- Pawn end games are treated specially
-- We consider escaped passed pawns in 2 situations:
-- pawn race: when both colors have at least one escaped pp
-- winning promotion: when only one side has it
-- Both are only valid in pawn endings
-- The pawn race is tricky when equal:
-- 1. What if the first promoting part gives check (or even mate)? Or what if after both promotions,
-- the first one can check and evntually capture the opposite queen? We just hope this situations are
-- 2. What about the rest of pawns? Here we make a trick: we shift the passed
-- pawns virtually one row back, which gives less points for the possibly remaining
-- passed pawns - now with queens)
pawnEndGame :: SpecialEval
pawnEndGame p
    | not (null mescds) && not (null yescds) = Score dpr
    | not (null mescds)                      = Score myrace
    |                      not (null yescds) = Score yorace
    | otherwise                              = NotMatched
    -- | -- here we will consider what is with 2 passed pawns which are far enough from each other
    -- and even with connected (or defended) passed pawns
    where !mfpbb = passed p .&. me p
          !yfpbb = passed p .&. yo p
          !myking = kingSquare (kings p) (me p)
          !yoking = kingSquare (kings p) (yo p)
          (escMe, escYo, maDiff)
              | moving p == White = (escMeWhite yoking, escYoBlack myking,   mater p)
              | otherwise         = (escMeBlack yoking, escYoWhite myking, - mater p)
          mpsqs  = map escMe $ bbToSquares mfpbb	-- my pp squares & distances to promotion
          mescds = map snd $ filter fst mpsqs		-- my escaped passed pawns
          ypsqs  = map escYo $ bbToSquares yfpbb	-- your pp squares & distances to promotion
          yescds = map snd $ filter fst ypsqs		-- your escaped passed pawns
          dpr | mim < miy     = myrace
              | mim > miy + 1 = yorace
              | otherwise     = withQueens     -- Here: this is more complex, e.g. if check while promoting
                                               -- or direct after promotion + queen capture?
          mim = fst $ minimumBy (comparing snd) mescds      -- who is promoting first?
          miy = fst $ minimumBy (comparing snd) yescds
          myrace =  promoBonus - distMalus mim
          yorace = -promoBonus + distMalus miy
          promoBonus = 1000     -- i.e. almost a queen (here the unit is 1 cp)
          distMalus x = unsafeShiftL x 3        -- to bring at least 8 cp per move until promotion
          -- We try to estimate staticaly what will be after promotions of both queens
          -- This will be another specialized evaluation function...
          -- But now we consider only the material difference (which consists only of pawns)
          withQueens = maDiff
 
escMeWhite :: Square -> Square -> (Bool, (Square, Int))
escMeWhite !ksq !psq = (esc, (psq, dis))
    where !tsq = promoW psq
          !dis = squareDistance psq tsq
          !esc = dis < squareDistance ksq tsq
 
escYoWhite :: Square -> Square -> (Bool, (Square, Int))
escYoWhite !ksq !psq = (esc, (psq, dis))
    where !tsq = promoW psq
          !dis = squareDistance psq tsq
          !esc = dis < squareDistance ksq tsq - 1       -- because we move
 
escMeBlack :: Square -> Square -> (Bool, (Square, Int))
escMeBlack !ksq !psq = (esc, (psq, dis))
    where !tsq = promoB psq
          !dis = squareDistance psq tsq
          !esc = dis < squareDistance ksq tsq
 
escYoBlack :: Square -> Square -> (Bool, (Square, Int))
escYoBlack !ksq !psq = (esc, (psq, dis))
    where !tsq = promoB psq
          !dis = squareDistance psq tsq
          !esc = dis < squareDistance ksq tsq - 1       -- because we move

{--
simplePawnEndGame :: MyPos -> Int
simplePawnEndGame p = d
    where !d = mepv - yopv
          !mepv = simplePvMe $ me p .&. pawns p
          !yopv = simplePvYo $ yo p .&. pawns p
          (simplePvMe, simplePvYo) | moving p == White = (simplePvWhite, simplePvBlack)
                                   | otherwise         = (simplePvBlack, simplePvWhite)

-- Just a simple weighted count
simplePvWhite :: BBoard -> Int
simplePvWhite !bb = pv
    where !pv = 100 * pc
          !pc0 = popCount $ bb  .&. band
          !bb1 = bb  `unsafeShiftL` 16
          !pc1 = popCount $ bb1 .&. band
          !bb2 = bb1 `unsafeShiftL` 16
          !pc2 = popCount $ bb2 .&. band
          !pc  = (pc0 `unsafeShiftL` 2) + (pc1 `unsafeShiftL` 1) + pc2
          band = 0x00FFFF0000000000	-- row 6 and 7

simplePvBlack :: BBoard -> Int
simplePvBlack !bb = pv
    where !pv = 100 * pc
          !pc0 = popCount $ bb  .&. band
          !bb1 = bb  `unsafeShiftR` 16
          !pc1 = popCount $ bb1 .&. band
          !bb2 = bb1 `unsafeShiftR` 16
          !pc2 = popCount $ bb2 .&. band
          !pc  = (pc0 `unsafeShiftL` 2) + (pc1 `unsafeShiftL` 1) + pc2
          band = 0x0000000000FFFF00	-- row 2 and 3

halfPawnMax :: Int -> Int -> Int
halfPawnMax mx d
    | steps > mx = 100 * mx
    | otherwise  = 100 * steps
    where steps = (d + 1) `unsafeShiftR` 1
--}
--------------------------------------
