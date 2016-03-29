{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Eval.Eval (
    initEvalState,
    posEval
) where

import Data.Array.Base (unsafeAt)
import Data.Bits
import Data.List (minimumBy)
import Control.Monad.State.Lazy
import Data.Array.Unboxed
import Data.Ord (comparing)

import Struct.Struct
import Struct.Status
import Struct.Config
import Moves.Moves
import Moves.BitBoard
import Moves.Pattern

class EvalItem a where
    evalItem :: Int -> EvalParams -> EvalWeights -> MyPos -> a -> MidEnd -> MidEnd

data AnyEvalItem = forall a . EvalItem a => EvIt a

-- This is the list of evaluated characteristics of a positions
-- Every item can have one or more parameters which have a name, a default value
-- and a range of values (values are kept for learning purposes as doubles,
-- but for the evaluation itself one copy of integer parameter values is also kept)
evalItems :: [AnyEvalItem]
evalItems = [ EvIt Material,	-- material balance (i.e. white - black material
              EvIt Redundance,	-- bishop pair and rook redundance
              EvIt RookPawn,	-- the rook pawns are about 15% less valuable
              EvIt KingSafe,	-- king safety
              EvIt KingOpen,	-- malus for king openness
              EvIt KingPlace,	-- bonus when king is near some fields
              EvIt LastLine,	-- malus for pieces on last line (except rooks and king)
              EvIt Mobility,	-- pieces mobility
              EvIt Center,	-- attacs of center squares
              EvIt Advers,	-- attacs of adverse squares
              EvIt RookPlc,	-- rooks points for placements
              EvIt EnPrise,	-- when not quiescent - pieces en prise
              EvIt PaBlo,	-- pawn blocking
              EvIt Izolan,	-- isolated pawns
              EvIt Backward,	-- backward pawns
              EvIt AdvPawns,	-- advanced pawns
              EvIt PassPawns	-- pass pawns
            ]

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

posEval :: MyPos -> State EvalState Int
posEval !p = do
    sti <- get
    let sce = evalDispatch p sti
        !scl = min matesc $ max (-matesc) sce
        !scc = if granCoarse > 0 then (scl + granCoarse2) .&. granCoarseM else scl
    return scc

evalDispatch :: MyPos -> EvalState -> Int
evalDispatch p sti
    | pawns p == 0 = evalNoPawns p sti
    | pawns p .&. me p == 0 ||
      pawns p .&. yo p == 0 = evalSideNoPawns p sti
    | kings p .|. pawns p == occup p,
      Just r <- pawnEndGame p = r
    | otherwise    = normalEval p sti

itemEval :: Int -> EvalParams -> EvalWeights -> MyPos -> AnyEvalItem -> MidEnd -> MidEnd
itemEval gph ep ew p (EvIt a) = evalItem gph ep ew p a

normalEval :: MyPos -> EvalState -> Int
normalEval p !sti = sc
    where feat = foldr (itemEval gph ep ew p) (MidEnd 0 0) evalItems
          ep   = esEParams  sti
          ew   = esEWeights sti
          gph  = gamePhase p
          !sc = ((mid feat + epMovingMid ep) * gph + (end feat + epMovingEnd ep) * (256 - gph))
                   `unsafeShiftR` (shift2Cp + 8)

gamePhase :: MyPos -> Int
gamePhase p = g
    where qs = popCount $ queens p
          rs = popCount $ rooks p
          bs = popCount $ bishops p
          ns = popCount $ knights p
          !g = qs * 39 + rs * 20 + (bs + ns) * 12	-- opening: 254, end: 0

evalSideNoPawns :: MyPos -> EvalState -> Int
evalSideNoPawns p sti
    | npwin && insufficient = 0
    | npwin && lessRook p   = nsc `div` 4
    | otherwise             = nsc
    where nsc = normalEval p sti
          npside = if pawns p .&. me p == 0 then me p else yo p
          npwin = npside == me p && nsc > 0 || npside == yo p && nsc < 0
          insufficient = majorcnt == 0 && (minorcnt == 1 || minorcnt == 2 && bishopcnt == 0)
          bishopcnt = popCount $ bishops p .&. npside
          minorcnt  = popCount $ (bishops p .|. knights p) .&. npside
          majorcnt  = popCount $ (queens p .|. rooks p) .&. npside

-- These evaluation function distiguishes between some known finals with no pawns
evalNoPawns :: MyPos -> EvalState -> Int
evalNoPawns p sti = sc
    where !sc | onlykings   = 0
              | kmk || knnk = 0		-- one minor or two knights
              | kbbk        = mateKBBK p kaloneyo	-- 2 bishops
              | kbnk        = mateKBNK p kaloneyo	-- bishop + knight
              | kMxk        = mateKMajxK p kaloneyo	-- simple mate with at least one major
              | lessRook p  = (normalEval p sti) `div` 2
              | otherwise   = normalEval p sti
          kaloneme = me p `less` kings p == 0
          kaloneyo = yo p `less` kings p == 0
          onlykings = kaloneme && kaloneyo
          kmk  = (kaloneme || kaloneyo) && minorcnt == 1 && majorcnt == 0
          knnk = (kaloneme || kaloneyo) && minorcnt == 2 && majorcnt == 0 && bishops p == 0
          kbbk = (kaloneme || kaloneyo) && minorcnt == 2 && majorcnt == 0 && knights p == 0
          kbnk = (kaloneme || kaloneyo) && minorcnt == 2 && not (knnk || kbbk)
          kMxk = (kaloneme || kaloneyo) && majorcnt > 0
          minor   = bishops p .|. knights p
          minorcnt = popCount minor
          major    = queens p .|. rooks p
          majorcnt = popCount major

-- Has one of the players less then one rook advantage (without pawns)?
-- In this case it is drawish (if the winning part has no pawns)
-- This is a primitive first approach
lessRook :: MyPos -> Bool
lessRook p | mq == yq && mr == yr = mb + mn - yb - yn `elem` [-1, 0, 1]
           | otherwise = False
    where !mq = popCount $ queens  p .&. me p
          !yq = popCount $ queens  p .&. yo p
          !mr = popCount $ rooks   p .&. me p
          !yr = popCount $ rooks   p .&. yo p
          !mb = popCount $ bishops p .&. me p
          !yb = popCount $ bishops p .&. yo p
          !mn = popCount $ knights p .&. me p
          !yn = popCount $ knights p .&. yo p

winBonus :: Int
winBonus = 200	-- when it's known win

mateKBBK :: MyPos -> Bool -> Int
mateKBBK = mateScore centerDistance

-- It seems that with 2 bishops or 1 major it's the same
-- rule to go to mate
mateKMajxK :: MyPos -> Bool -> Int
mateKMajxK = mateKBBK

mateKBNK :: MyPos -> Bool -> Int
mateKBNK p = mateScore (bnMateDistance wbish) p
    where wbish = bishops p .&. lightSquares /= 0

{-# INLINE mateScore #-}
mateScore :: (Square -> Int) -> MyPos -> Bool -> Int
mateScore f p mywin = msc
    where !kadv = if mywin then ky else km
          !km = kingSquare (kings p) (me p)
          !ky = kingSquare (kings p) (yo p)
          !distk = squareDistance km ky
          !distc = f kadv
          !sc = winBonus + distc*distc - distk*distk
          !mtr = if moving p == White then mater p else -(mater p)
          !wsc = if mywin then sc else -sc
          !msc = mtr + wsc

squareDistArr :: UArray (Square, Square) Int
squareDistArr = array ((0,0), (63,63)) [((s1, s2), squareDist s1 s2) | s1 <- [0..63], s2 <- [0..63]]
    where squareDist f t = max (abs (fr - tr)) (abs (fc - tc))
              where (fr, fc) = f `divMod` 8
                    (tr, tc) = t `divMod` 8

squareDistance :: Square -> Square -> Int
squareDistance = curry (squareDistArr!)

-- This center distance should be pre calculated
centerDistance :: Int -> Int
centerDistance sq = max (r - 4) (3 - r) + max (c - 4) (3 - c)
    where (r, c) = sq `divMod` 8

-- This distance for knight bishop mate should be pre calculated
-- Here we have to push the adverse king far from center and from the opposite bishop corners
bnMateDistance :: Bool -> Square -> Int
bnMateDistance wbish sq = min (squareDistance sq ocor1) (squareDistance sq ocor2)
    where (ocor1, ocor2) = if wbish then (0, 63) else (7, 56)

----------------------------------------------------------------------------
-- Here we have the implementation of the evaluation items
-- They do not return a score, but a vector of fulfillments of some criteria
-- With version 0.55 we compute everything from white point of view
-- and only at the end we negate the score if black side is asked
----------------------------------------------------------------------------
------ King Safety ------
data KingSafe = KingSafe

instance EvalItem KingSafe where
    evalItem _ _ ew p _ mide = kingSafe p ew mide

-- Rewrite of king safety taking into account number and quality
-- of pieces attacking king neighbour squares
-- This function is almost optimised, it could perhaps be faster
-- if we eliminate the lists
kingSafe :: MyPos -> EvalWeights -> MidEnd -> MidEnd
kingSafe !p !ew !mide = madm mide (ewKingSafe ew) ksafe
    where !ksafe = ksSide (yo p) (yoKAttacs p) (myPAttacs p) (myNAttacs p) (myBAttacs p) (myRAttacs p) (myQAttacs p) (myKAttacs p) (myAttacs p)
                 - ksSide (me p) (myKAttacs p) (yoPAttacs p) (yoNAttacs p) (yoBAttacs p) (yoRAttacs p) (yoQAttacs p) (yoKAttacs p) (yoAttacs p)

-- To make the sum and count in one pass
data Flc = Flc !Int !Int

fadd :: Flc -> Flc -> Flc
fadd (Flc f1 q1) (Flc f2 q2) = Flc (f1+f2) (q1+q2)

fmul :: Flc -> Int
fmul (Flc f q) = f * q

ksSide :: BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> Int
ksSide !yop !yok !myp !myn !myb !myr !myq !myk !mya
    | myq == 0  = 0
    | otherwise = mattacs
    where !freey = popCount $ yok `less` (mya .|. yop)
          qual a p = let c = popCount $ yok .&. a
                     in Flc (flaCoef `unsafeAt` c) (c * p)
          -- qualWeights = [1, 2, 2, 4, 8, 2]
          !qp = qual myp 1
          !qn = qual myn 2
          !qb = qual myb 2
          !qr = qual myr 4
          !qq = qual myq 8
          !qk = qual myk 2
          !ixm = fmul (fadd qp $ fadd qn $ fadd qb $ fadd qr $ fadd qq qk) `unsafeShiftR` 2
               + 8 + ksShift - freey
          !mattacs = attCoef `unsafeAt` ixm
          ksShift = 5

-- We want to eliminate "if yok .&. a /= 0 ..."
-- by using an array
flaCoef :: UArray Int Int
flaCoef = listArray (0, 8) [ 0, 1, 1, 1, 1, 1, 1, 1, 1 ]

-- We take the maximum of 240 because:
-- Quali max: 8 * (1 + 2 + 2 + 4 + 8 + 2) < 160
-- Flag max: 6
-- 6 * 160 / 4 = 240
-- Here the beginning of -8 is actually wrong, which comes to the same
-- as increasing the importance of king safety
attCoef :: UArray Int Int
attCoef = listArray (0, 248) $ take 8 (repeat 0) ++ [ f x | x <- [0..63] ] ++ repeat (f 63)
    where f :: Int -> Int
          f x = let y = fromIntegral x :: Double in round $ (2.92968750 - 0.03051758*y)*y*y

kingSquare :: BBoard -> BBoard -> Square
kingSquare kingsb colorp = head $ bbToSquares $ kingsb .&. colorp
{-# INLINE kingSquare #-}

------ Material ------
data Material = Material

instance EvalItem Material where
    evalItem _ _ ew p _ mide = materDiff p ew mide

materDiff :: MyPos -> EvalWeights -> MidEnd -> MidEnd
materDiff p ew mide = mad mide (ewMaterialDiff ew) md
    where !md | moving p == White =   mater p
              | otherwise         = - mater p

------ King openness ------
data KingOpen = KingOpen

instance EvalItem KingOpen where
    evalItem _ _ ew p _ mide = kingOpen p ew mide

-- Openness can be tought only with pawns (like we take) or all pieces
kingOpen :: MyPos -> EvalWeights -> MidEnd -> MidEnd
kingOpen p ew mide = mad mide (ewKingOpen ew) ko
    where !ko = adv - own
          moprooks   = popCount $ rooks p .&. yo p
          mopqueens  = popCount $ queens p .&. yo p
          mwb = popCount $ bAttacs paw msq `less` paw
          mwr = popCount $ rAttacs paw msq `less` (paw .|. lastrs)
          yoprooks   = popCount $ rooks p .&. me p
          yopqueens  = popCount $ queens p .&. me p
          ywb = popCount $ bAttacs paw ysq `less` paw
          ywr = popCount $ rAttacs paw ysq `less` (paw .|. lastrs)
          paw = pawns p
          msq = kingSquare (kings p) $ me p
          ysq = kingSquare (kings p) $ yo p
          comb !oR !oQ !wb !wr = let !v = oR * wr + 2 * oQ * (wb + wr) in v
          own = comb moprooks mopqueens mwb mwr
          adv = comb yoprooks yopqueens ywb ywr
          lastrs = 0xFF000000000000FF	-- approx: take out last row which cant be covered by pawns

------ King placement ------
data KingPlace = KingPlace

instance EvalItem KingPlace where
    evalItem _ ep ew p _ mide = kingPlace ep p ew mide

-- Depending on which pieces are on the board we have some preferences
-- where the king should be placed. For example, in the opening and middle game it should
-- be in some corner, in endgame it should be near some (passed) pawn(s)
kingPlace :: EvalParams -> MyPos -> EvalWeights -> MidEnd -> MidEnd
kingPlace ep p ew mide = made (madm mide (ewKingPlaceCent ew) kcd) (ewKingPlacePwns ew) kpd
    where !kcd = (mpl - ypl) `unsafeShiftR` epMaterBonusScale ep
          !kpd = (mpi - ypi) `unsafeShiftR` epPawnBonusScale  ep
          !mks = kingSquare (kings p) $ me p
          !yks = kingSquare (kings p) $ yo p
          !mkm = materFun yminor yrooks yqueens
          !ykm = materFun mminor mrooks mqueens
          (!mpl, !ypl, !mpi, !ypi)
              | moving p == White = ( kingMaterBonus White mpawns mkm mks
                                    , kingMaterBonus Black ypawns ykm yks
                                    , kingPawnsBonus mks (passed p) mpassed ypassed
                                    , kingPawnsBonus yks (passed p) mpassed ypassed
                                    )
              | otherwise         = ( kingMaterBonus Black mpawns mkm mks
                                    , kingMaterBonus White ypawns ykm yks
                                    , kingPawnsBonus mks (passed p) ypassed mpassed
                                    , kingPawnsBonus yks (passed p) ypassed mpassed
                                    )
          !mro     = rooks p .&. me p
          !mrooks  = popCount mro
          !mqueens = popCount $ queens p .&. me p
          !mminor  = popCount $ (bishops p .|. knights p) .&. me p
          !yro     = rooks p .&. yo p
          !yrooks  = popCount yro
          !yqueens = popCount $ queens p .&. yo p
          !yminor  = popCount $ (bishops p .|. knights p) .&. yo p
          !mpawns  = pawns p .&. me p
          !ypawns  = pawns p .&. yo p
          !mpassed = passed p .&. me p
          !ypassed = passed p .&. yo p
          materFun m r q = (m * epMaterMinor ep + r * epMaterRook ep + q * epMaterQueen ep)
                               `unsafeShiftR` epMaterScale ep

promoW, promoB :: Square -> Square
promoW s = 56 + (s .&. 7)
promoB s =       s .&. 7

-- We give bonus also for pawn promotion squares, if the pawn is near enough to promote
-- Give as parameter bitboards for all pawns, white pawns and black pawns for performance
kingPawnsBonus :: Square -> BBoard -> BBoard -> BBoard -> Int
kingPawnsBonus !ksq !alp !wpass !bpass = bonus
    where !bpsqs = sum $ map (pawnBonus . squareDistance ksq) $ bbToSquares alp
          !bqsqs = sum $ map (pawnBonus . squareDistance ksq)
                       $ map promoW (bbToSquares wpass) ++ map promoB (bbToSquares bpass)
          !bonus = bpsqs + bqsqs

-- This is a bonus for the king beeing near one corner
-- It's bigger when the enemy has more material (only pieces)
-- and when that corner has a pawn shelter
kingMaterBonus :: Color -> BBoard -> Int -> Square -> Int
kingMaterBonus c !myp !mat !ksq
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
          -- roWA = row1 .&. (fileA .|. fileB .|. fileC)
          -- roWH = row1 .&. (fileG .|. fileH)
          -- roBA = row8 .&. (fileA .|. fileB .|. fileC)
          -- roBH = row8 .&. (fileG .|. fileH)
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

------ Rookm placement points ------
data RookPlc = RookPlc

instance EvalItem RookPlc where
    evalItem _ _ ew p _ mide = evalRookPlc p ew mide

evalRookPlc :: MyPos -> EvalWeights -> MidEnd -> MidEnd
evalRookPlc p ew mide = mad (mad (mad mide (ewRookHOpen ew) ho) (ewRookOpen ew) op) (ewRookConn ew) rc
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
data Mobility = Mobility	-- "safe" moves

instance EvalItem Mobility where
    evalItem _ _ ew p _ mide = mobDiff p ew mide

-- Here we do not calculate pawn mobility (which, calculated as attacs, is useless)
mobDiff :: MyPos -> EvalWeights -> MidEnd -> MidEnd
mobDiff p ew mide = mad (mad (mad (mad mide (ewMobilityKnight ew) n) (ewMobilityBishop ew) b) (ewMobilityRook ew) r) (ewMobilityQueen ew) q
    where !myN = popCount $ myNAttacs p `less` (me p .|. yoPAttacs p)
          !myB = popCount $ myBAttacs p `less` (me p .|. yoPAttacs p)
          !myR = popCount $ myRAttacs p `less` (me p .|. yoA1)
          !myQ = popCount $ myQAttacs p `less` (me p .|. yoA2)
          !yoA1 = yoPAttacs p .|. yoNAttacs p .|. yoBAttacs p
          !yoA2 = yoA1 .|. yoRAttacs p
          !yoN = popCount $ yoNAttacs p `less` (yo p .|. myPAttacs p)
          !yoB = popCount $ yoBAttacs p `less` (yo p .|. myPAttacs p)
          !yoR = popCount $ yoRAttacs p `less` (yo p .|. myA1)
          !yoQ = popCount $ yoQAttacs p `less` (yo p .|. myA2)
          !myA1 = myPAttacs p .|. myNAttacs p .|. myBAttacs p
          !myA2 = myA1 .|. myRAttacs p
          !n = myN - yoN
          !b = myB - yoB
          !r = myR - yoR
          !q = myQ - yoQ

------ Center control ------
data Center = Center

instance EvalItem Center where
    evalItem _ _ ew p _ mide = centerDiff p ew mide

-- This function is already optimised
centerDiff :: MyPos -> EvalWeights -> MidEnd -> MidEnd
centerDiff p ew mide = mad (mad (mad (mad (mad (mad mide (ewCenterPAtts ew) pd) (ewCenterNAtts ew) nd) (ewCenterBAtts ew) bd) (ewCenterRAtts ew) rd) (ewCenterQAtts ew) qd) (ewCenterKAtts ew) kd
    where !mpa = popCount $ myPAttacs p .&. center
          !ypa = popCount $ yoPAttacs p .&. center
          !pd  = mpa - ypa
          !mna = popCount $ myNAttacs p .&. center
          !yna = popCount $ yoNAttacs p .&. center
          !nd  = mna - yna
          !mba = popCount $ myBAttacs p .&. center
          !yba = popCount $ yoBAttacs p .&. center
          !bd  = mba - yba
          !mra = popCount $ myRAttacs p .&. center
          !yra = popCount $ yoRAttacs p .&. center
          !rd  = mra - yra
          !mqa = popCount $ myQAttacs p .&. center
          !yqa = popCount $ yoQAttacs p .&. center
          !qd  = mqa - yqa
          !mka = popCount $ myKAttacs p .&. center
          !yka = popCount $ yoKAttacs p .&. center
          !kd  = mka - yka
          center = 0x0000001818000000

-------- Attacks to adverse squares ----------
data Advers = Advers

instance EvalItem Advers where
    evalItem _ _ ew p _ mide = adversDiff p ew mide

adversDiff :: MyPos -> EvalWeights -> MidEnd -> MidEnd
adversDiff p ew mide = mad (mad (mad (mad (mad (mad mide (ewAdvPAtts ew) pd) (ewAdvNAtts ew) nd) (ewAdvBAtts ew) bd) (ewAdvRAtts ew) rd) (ewAdvQAtts ew) qd) (ewAdvKAtts ew) kd
    where !mpa = popCount $ myPAttacs p .&. yoH
          !ypa = popCount $ yoPAttacs p .&. myH
          !pd  = mpa - ypa
          !mna = popCount $ myNAttacs p .&. yoH
          !yna = popCount $ yoNAttacs p .&. myH
          !nd  = mna - yna
          !mba = popCount $ myBAttacs p .&. yoH
          !yba = popCount $ yoBAttacs p .&. myH
          !bd  = mba - yba
          !mra = popCount $ myRAttacs p .&. yoH
          !yra = popCount $ yoRAttacs p .&. myH
          !rd  = mra - yra
          !mqa = popCount $ myQAttacs p .&. yoH
          !yqa = popCount $ yoQAttacs p .&. myH
          !qd  = mqa - yqa
          !mka = popCount $ myKAttacs p .&. yoH
          !yka = popCount $ yoKAttacs p .&. myH
          !kd  = mka - yka
          (myH, yoH) | moving p == White = (ah14, ah58)
                     | otherwise         = (ah58, ah14)
          ah14 = 0xFFFFFFFF
          ah58 = 0xFFFFFFFF00000000

-------- Isolated pawns --------
data Izolan = Izolan

instance EvalItem Izolan where
    evalItem _ _ ew p _ mide = isolDiff p ew mide

isolDiff :: MyPos -> EvalWeights -> MidEnd -> MidEnd
isolDiff p ew mide = mad (mad mide (ewIsolPawns ew) nd) (ewIsolPassed ew) pd
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
          !ris = popCount $ myr `less` myc
          !pis = popCount $ myp `less` myc

-------- Backward pawns --------
data Backward = Backward

instance EvalItem Backward where
    evalItem _ _ ew p _ mide = backDiff p ew mide

backDiff :: MyPos -> EvalWeights -> MidEnd -> MidEnd
backDiff p ew mide
    | moving p == White
    = let wp = pawns p .&. me p
          bp = pawns p .&. yo p
          (bpw, bpow) = backPawns White wp bp (yoPAttacs p)
          (bpb, bpob) = backPawns Black bp wp (myPAttacs p)
          !bpd  = popCount bpw  - popCount bpb
          !bpod = popCount bpow - popCount bpob
      in mad (mad mide (ewBackPawns ew) bpd) (ewBackPOpen ew) bpod
    | otherwise
    = let bp = pawns p .&. me p
          wp = pawns p .&. yo p
          (bpw, bpow) = backPawns White wp bp (myPAttacs p)
          (bpb, bpob) = backPawns Black bp wp (yoPAttacs p)
          !bpd  = popCount bpb  - popCount bpw
          !bpod = popCount bpob - popCount bpow
      in mad (mad mide (ewBackPawns ew) bpd) (ewBackPOpen ew) bpod

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
data EnPrise = EnPrise

instance EvalItem EnPrise where
    evalItem _ _ ew p _ mide = enPrise p ew mide

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
enPrise p ew mide = mad (mad (mad mide (ewEnpHanging ew) ha) (ewEnpEnPrise ew) ep) (ewEnpAttacked ew) at
    where !meP = me p .&. pawns   p	-- my pieces
          !meN = me p .&. knights p
          !meB = me p .&. bishops p
          !meR = me p .&. rooks   p
          !meQ = me p .&. queens  p
          !atP = meP  .&. yoAttacs p	-- my attacked pieces
          !atN = meN  .&. yoAttacs p
          !atB = meB  .&. yoAttacs p
          !atR = meR  .&. yoAttacs p
          !atQ = meQ  .&. yoAttacs p
          !haP = atP `less` myAttacs p	-- attacked and not defended
          !haN = atN `less` myAttacs p
          !haB = atB `less` myAttacs p
          !haR = atR `less` myAttacs p
          !haQ = atQ `less` myAttacs p
          !epN = (atP `less` haP) .&. yoPAttacs p	-- defended, but attacked by less
          !epB = (atB `less` haN) .&. yoPAttacs p	-- valuable opponent pieces
          !epR = (atR `less` haR) .&. yoA1
          !epQ = (atQ `less` haQ) .&. yoA2
          !yoA1 = yoPAttacs p .|. yoNAttacs p .|. yoBAttacs p
          !yoA2 = yoA1 .|. yoRAttacs p
          !ha = popCount haP + 3 * (popCount haN + popCount haB)
              + 5 * popCount haR + 9 * popCount haQ
          !ep =                 3 * (popCount epN + popCount epB)
              + 5 * popCount epR + 9 * popCount epQ
          !at = popCount atP + 3 * (popCount atN + popCount atB)
              + 5 * popCount atR + 9 * popCount atQ

------ Last Line ------
data LastLine = LastLine

instance EvalItem LastLine where
    evalItem _ _ ew p _ mide = lastline p ew mide

-- Only for minor figures (queen is free to stay where it wants)
-- Negative at the end: so that it falls stronger
lastline :: MyPos -> EvalWeights -> MidEnd -> MidEnd
lastline p ew mide = madm mide (ewLastLinePenalty ew) cdiff
    where !whl = popCount $ me p .&. cb
          !bll = popCount $ yo p .&. cb
          !cb = (knights p .|. bishops p) .&. lali
          lali = 0xFF000000000000FF	-- approximation!
          !cdiff = bll - whl

------ Redundance: bishop pair and rook redundance ------
data Redundance = Redundance

instance EvalItem Redundance where
    evalItem _ _ ew p _ mide = evalRedundance p ew mide

-- This function is optimised
evalRedundance :: MyPos -> EvalWeights -> MidEnd -> MidEnd
evalRedundance p ew mide = mad (mad mide (ewBishopPair ew) bp) (ewRedundanceRook ew) rr
    where !wbl = bishops p .&. me p .&. lightSquares
          !wbd = bishops p .&. me p .&. darkSquares
          !bbl = bishops p .&. yo p .&. lightSquares
          !bbd = bishops p .&. yo p .&. darkSquares
          !bpw = popCount wbl .&. popCount wbd	-- tricky here: exact 1 and 1 is ok
          !bpb = popCount bbl .&. popCount bbd	-- and here
          !bp  = bpw - bpb
          !wro = rooks p .&. me p
          !bro = rooks p .&. yo p
          !wrr = popCount wro `unsafeShiftR` 1	-- tricky here: 2, 3 are the same...
          !brr = popCount bro `unsafeShiftR` 1	-- and here
          !rr  = wrr - brr

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
data RookPawn = RookPawn

instance EvalItem RookPawn where
    evalItem _ _ ew p _ mide = evalRookPawn p ew mide

-- This function is already optimised
evalRookPawn :: MyPos -> EvalWeights -> MidEnd -> MidEnd
evalRookPawn p ew mide = mad mide (ewRookPawn ew) rps
    where !wrp = popCount $ pawns p .&. me p .&. rookFiles
          !brp = popCount $ pawns p .&. yo p .&. rookFiles
          !rps = wrp - brp

------ Blocked pawns ------
data PaBlo = PaBlo

instance EvalItem PaBlo where
    evalItem _ _ ew p _ mide = pawnBl p ew mide

pawnBl :: MyPos -> EvalWeights -> MidEnd -> MidEnd
pawnBl p ew mide
    | moving p == White = let (wp, wo, wa) = pawnBloWhite mer mef yof
                              (bp, bo, ba) = pawnBloBlack yor yof mef
                          in mad (mad (mad mide (ewPawnBlockP ew) (wp-bp)) (ewPawnBlockO ew) (wo-bo)) (ewPawnBlockA ew) (wa-ba)
    | otherwise         = let (wp, wo, wa) = pawnBloWhite yor yof mef
                              (bp, bo, ba) = pawnBloBlack mer mef yof
                          in mad (mad (mad mide (ewPawnBlockP ew) (bp-wp)) (ewPawnBlockO ew) (bo-wo)) (ewPawnBlockA ew) (ba-wa)
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

------ Pass pawns ------
data PassPawns = PassPawns

-- passPawnLev after optimization with Clop
-- 4686 games at 15+0.25 games pass3o against itself (mean)
-- Clop forecast: 60+-25 elo
instance EvalItem PassPawns where
    evalItem gph ep ew p _ mide = passPawns gph ep p ew mide
 
-- Every passed pawn will be evaluated separately
passPawns :: Int -> EvalParams -> MyPos -> EvalWeights -> MidEnd -> MidEnd
passPawns gph ep p ew mide = mad mide (ewPassPawnLev ew) dpp
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
perPassedPawn gph ep p c sq
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
perPassedPawnOk gph ep p c sq sqbb moi toi moia toia = val
    where (!way, !behind) | c == White = (shadowUp sqbb, shadowDown sqbb)
                          | otherwise  = (shadowDown sqbb, shadowUp sqbb)
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
          !bbfree   = way `less` (bbmyctrl .|. bbyoctrl)
          !myctrl = popCount bbmyctrl
          !yoctrl = popCount bbyoctrl
          !free   = popCount bbfree
          !x = myctrl + yoctrl + free
          a0 = 10
          b0 = -120
          c0 = 410
          !pmax = (a0 * x + b0) * x + c0
          !myking = kingSquare (kings p) moi
          !yoking = kingSquare (kings p) toi
          !mdis = squareDistance sq myking
          !ydis = squareDistance sq yoking
          !kingprx = ((mdis - ydis) * epPassKingProx ep * (256 - gph)) `unsafeShiftR` 8
          !val1 = (pmax * (128 - kingprx) * (128 - epPassBlockO ep * mblo)) `unsafeShiftR` 14
          !val2 = (val1 * (128 - epPassBlockA ep * yblo)) `unsafeShiftR` 7
          !val  = (val2 * (128 + epPassMyCtrl ep * myctrl) * (128 - epPassYoCtrl ep * yoctrl))
                    `unsafeShiftR` 14

------ Advanced pawns, on 6th & 7th rows (not passed) ------
data AdvPawns = AdvPawns

instance EvalItem AdvPawns where
    evalItem _ _ ew p _ mide = advPawns p ew mide
 
advPawns :: MyPos -> EvalWeights -> MidEnd -> MidEnd
advPawns p ew mide = mad (mad mide (ewAdvPawn5 ew) ap5) (ewAdvPawn6 ew) ap6
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
pawnEndGame :: MyPos -> Maybe Int
pawnEndGame p
    -- | null mescds && null yescds             = Nothing
    | not (null mescds) && not (null yescds) = Just dpr
    | not (null mescds)                      = Just myrace
    |                      not (null yescds) = Just yorace
    | otherwise                              = Nothing
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
          !mescds = map snd $ filter fst mpsqs		-- my escaped passed pawns
          ypsqs  = map escYo $ bbToSquares yfpbb	-- your pp squares & distances to promotion
          !yescds = map snd $ filter fst ypsqs		-- your escaped passed pawns
          -- mesc = not . null $ mescds
          -- yesc = not . null $ yescds
          dpr | mim < miy     =  promoBonus - distMalus mim
              | mim > miy + 1 = -promoBonus + distMalus miy
              | otherwise     =  withQueens     -- Here: this is more complex, e.g. if check while promoting
                                       -- or direct after promotion + queen capture?
          -- (mim, msq) = minimumBy (comparing snd) mescds      -- who is promoting first?
          -- (miy, ysq) = minimumBy (comparing snd) yescds
          mim = fst $ minimumBy (comparing snd) mescds      -- who is promoting first?
          miy = fst $ minimumBy (comparing snd) yescds
          myrace =  promoBonus - distMalus mim
          yorace = -promoBonus + distMalus miy
          promoBonus = 1000     -- i.e. almost a queen (here the unit is 1 cp)
          distMalus x = unsafeShiftL x 3        -- to bring at least 8 cp per move until promotion
          -- We try to estimate static what will be after promotions of both queens
          -- This will be another specialized evaluation function...
          -- But now we consider only the material difference (which consists only of pawns)
          withQueens = maDiff
          -- This one is for prunning: so match we can win at most
          -- yoPawnCount = popCount $ pawns p .&. yo p
          -- speg = simplePawnEndGame p	-- just to see how it works...
 
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
