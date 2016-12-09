{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Eval.Eval (
    initEvalState,
    posEval, featsEval, markerEval
) where

import Data.Array.Base (unsafeAt)
import Data.Bits
import Data.List (minimumBy, sortBy)
import Control.Monad.State.Lazy
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed         as U

import Struct.Struct
import Struct.Status
import Struct.Config
import Moves.Moves
import Moves.BitBoard
import Moves.Pattern

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
        -- esEParams  = npSetParm (colParams sds :: CollectFor EvalParams),
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

muSum :: U.Vector Int -> U.Vector Int -> Int
muSum a b = U.sum $ U.zipWith (*) a b

normalEval :: MyPos -> EvalState -> Int
normalEval p !sti = sc
    where (featMe, featYo)
              | moving p == White = (
                  sideEvalWhite p (me p) (yo p) (myAttBBs p) (yoAttBBs p),
                  sideEvalBlack p (yo p) (me p) (yoAttBBs p) (myAttBBs p)
                  )
              | otherwise = (
                  sideEvalBlack p (me p) (yo p) (myAttBBs p) (yoAttBBs p),
                  sideEvalWhite p (yo p) (me p) (yoAttBBs p) (myAttBBs p)
                  )
          -- Eval weights: moving/mid game, moving/end game, other/mid game, other/end game
          EvalWeights ewmm ewme ewom ewoe = esEWeights sti
          gph = gamePhase p
          !fmm = muSum ewmm featMe
          !fom = muSum ewom featYo
          !fme = muSum ewme featMe
          !foe = muSum ewoe featYo
          !sc = ((fmm + fom) * gph + (fme + foe) * (256 - gph)) `unsafeShiftR` (shift2Cp + 8)

featsEval :: MyPos -> (Int, [Int])
featsEval p = (gamePhase p, U.toList featMe ++ U.toList featYo)
    where (featMe, featYo)
              | moving p == White = (
                  sideEvalWhite p (me p) (yo p) (myAttBBs p) (yoAttBBs p),
                  sideEvalBlack p (yo p) (me p) (yoAttBBs p) (myAttBBs p)
                  )
              | otherwise = (
                  sideEvalBlack p (me p) (me p) (myAttBBs p) (yoAttBBs p),
                  sideEvalWhite p (yo p) (yo p) (yoAttBBs p) (myAttBBs p)
                  )

markerEval :: [Int]
markerEval = li1 ++ li2
    where li1 = map fromEnum [MatPawns .. PapAKDist3]
          li2 = map ((fromEnum LenEvalIdx +) . fromEnum) li1

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

kingSquare :: BBoard -> BBoard -> Square
kingSquare kingsb colorp = head $ bbToSquares $ kingsb .&. colorp
{-# INLINE kingSquare #-}

promoW, promoB :: Square -> Square
promoW s = 56 + (s .&. 7)
promoB s =       s .&. 7

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


{-
{-# INLINE spaceBlack #-}
spaceBlack :: BBoard -> BBoard -> BBoard -> BBoard -> Int
spaceBlack !mpawns !matts !ypatts !yatts = sv
    where yard = (fileC .|. fileD .|. fileE .|. fileF) .&. (row7 .|. row6 .|. row5)
          safe = (yard .&. (matts .|. complement yatts)) `less` (mpawns .|. ypatts)
          behi = shadowUp mpawns
          spa = popCount $ (safe `unsafeShiftR` 32) .|. (behi .&. safe)
          !sv = spaceVals `unsafeAt` spa
-}

{-# INLINE addBehindWhite #-}
addBehindWhite :: Square -> (Int, Square)
addBehindWhite sq = (be, sq)
    where be = sq `unsafeShiftR` 3

{-# INLINE addBehindBlack #-}
addBehindBlack :: Square -> (Int, Square)
addBehindBlack sq = (be, sq)
    where be = 7 `xor` (sq `unsafeShiftR` 3)

-- A data type to manage the indexes for the side eval
data EvalIdx = MatPawns | MatKnights | MatBishops | MatRooks | MatQueens
             | KopBAtts | KopRAtts
             | KipRank | KipFilc | KipMissSh | KipMissPs
             | RopHOpen | RopOpen | RopConn
             | IsoPawnRow | IsoPPDefdd | IsoSupPwns
             | PamAttacks | PamMoves
             | AdvPawns5 | AdvPawns6
             | BapClosed | BapOpen
             | AdvAtts
             | MarKnights | MarBishops
             | MobKnights | MobBishops | MobRooks | MobQueens
             | SpaSafe | SpaBehind
             | CenPawns | CenKnights | CenBishops | CenRooks | CenQueens | CenKing
             | EnpQP | EnpRP | EnpMP | EnpQM | EnpRM
             | KisZPAtts | KisZNAtts | KisZBAtts | KisZRAtts | KisZQAtts | KisFree
             | KisPPAtts | KisPNAtts | KisPBAtts | KisPRAtts | KisPQAtts | KisPStorm
             | PapBehind1 | PapBlocked1 | PapCtrl1 | PapRBehind1 | PapAKDist1
             | PapBehind2 | PapBlocked2 | PapCtrl2 | PapRBehind2 | PapAKDist2
             | PapBehind3 | PapBlocked3 | PapCtrl3 | PapRBehind3 | PapAKDist3
             | LenEvalIdx	-- this must be the last one to catch the length of the vector
             deriving Enum

{-# INLINE setEval #-}
setEval :: V.STVector s Int -> EvalIdx -> Int -> ST s ()
setEval v ei = V.unsafeWrite v (fromEnum ei)

-- The big evaluation function for white
sideEvalWhite :: MyPos -> BBoard -> BBoard -> AttBBs -> AttBBs -> U.Vector Int
sideEvalWhite !p !us !them !usAtts !themAtts = U.create $ do
    v <- V.new $ fromEnum LenEvalIdx
    setEval v MatPawns matPawns
    setEval v MatKnights matKnights
    setEval v MatBishops matBishops
    setEval v MatRooks matRooks
    setEval v MatQueens matQueens
    setEval v KopBAtts kopBAtts
    setEval v KopRAtts kopRAtts
    setEval v KipRank kipRank
    setEval v KipFilc kipFilc
    setEval v KipMissSh kipMissSh
    setEval v KipMissPs kipMissPs
    setEval v RopHOpen ropHOpen
    setEval v RopOpen ropOpen
    setEval v RopConn ropConn
    setEval v IsoPawnRow isoPawnRow
    setEval v IsoPPDefdd isoPPDefdd
    setEval v IsoSupPwns isoSupPwns
    setEval v PamAttacks pamAttacks
    setEval v PamMoves pamMoves
    setEval v AdvPawns5 advPawns5
    setEval v AdvPawns6 advPawns6
    setEval v BapClosed bapClosed
    setEval v BapOpen bapOpen
    setEval v AdvAtts advAtts
    setEval v MarKnights marKnights
    setEval v MarBishops marBishops
    setEval v MobKnights mobKnights
    setEval v MobBishops mobBishops
    setEval v MobRooks mobRooks
    setEval v MobQueens mobQueens
    setEval v SpaSafe spaSafe
    setEval v SpaBehind spaBehind
    setEval v CenPawns cenPawns
    setEval v CenKnights cenKnights
    setEval v CenBishops cenBishops
    setEval v CenRooks cenRooks
    setEval v CenQueens cenQueens
    setEval v CenKing cenKing
    setEval v EnpQP enpQP
    setEval v EnpRP enpRP
    setEval v EnpMP enpMP
    setEval v EnpQM enpQM
    setEval v EnpRM enpRM
    setEval v KisZPAtts kisZPAtts
    setEval v KisZNAtts kisZNAtts
    setEval v KisZBAtts kisZBAtts
    setEval v KisZRAtts kisZRAtts
    setEval v KisZQAtts kisZQAtts
    setEval v KisFree kisFree
    setEval v KisPPAtts kisPPAtts
    setEval v KisPNAtts kisPNAtts
    setEval v KisPBAtts kisPBAtts
    setEval v KisPRAtts kisPRAtts
    setEval v KisPQAtts kisPQAtts
    setEval v KisPStorm kisPStorm
    setEval v PapBehind1 papBehind1
    setEval v PapBlocked1 papBlocked1
    setEval v PapCtrl1 papCtrl1
    setEval v PapRBehind1 papRBehind1
    setEval v PapAKDist1 papAKDist1
    setEval v PapBehind2 papBehind2
    setEval v PapBlocked2 papBlocked2
    setEval v PapCtrl2 papCtrl2
    setEval v PapRBehind2 papRBehind2
    setEval v PapAKDist2 papAKDist2
    setEval v PapBehind3 papBehind3
    setEval v PapBlocked3 papBlocked3
    setEval v PapCtrl3 papCtrl3
    setEval v PapRBehind3 papRBehind3
    setEval v PapAKDist3 papAKDist3
    return v
    where -- we keep the features very basic and return fulfillment values
          -- which will be combined (mostly by weights multiplications) later
          ------ Material ------
          !myPawns    = pawns p .&. us
          !myKnights  = knights p .&. us
          !myBishops  = bishops p .&. us
          !myRooks    = rooks p .&. us
          !myQueens   = queens p .&. us
          !matPawns   = popCount myPawns
          !matKnights = popCount myKnights
          !matBishops = popCount myBishops
          !matRooks   = popCount myRooks
          !matQueens  = popCount myQueens
          ------ (Your) King openness ------
          !yksq     = kingSquare (kings p) them
          !notPawns = complement $ pawns p
          !kopBAtts = popCount $ bAttacs (pawns p) yksq .&. notPawns
          !kopRAtts = popCount $ rAttacs (pawns p) yksq .&. notPawns
          ------ (Your) King placement ------
          !kir = yksq `unsafeShiftR` 3	-- rank of the king
          !kif = yksq .&. 7		-- file of the king
          !kipRank   = 7 - kir		-- we want to be far from center
          !kipFilc   = kif * (7 - kif)
          -- For pawns shelter quality:
          !ykZone    = _kAttacs themAtts
          !ykPreZone = (ykZone `unsafeShiftR` 8) `less` ykZone
          !ykShelter = ykPreZone `unsafeShiftL` 8
          !notYPawns = complement $ pawns p .&. them
          !kipMissSh = popCount $ ykShelter .&. notYPawns
          !kipMissPs = popCount $ ykPreZone .&. notYPawns
          ------ Rook placement ------
          (ropHOpen, ropOpen)
              = foldr (perRook (pawns p) myPawns) (0, 0) $ bbToSquares myRooks
          !ropConn | _rAttacs usAtts .&. myRooks == 0 = 0
                   | otherwise                        = 1
          ------ Pawn connectivity --------
          !is0  = bbLeft myPawns .|. bbRight myPawns
          !isoPawnRow = popCount $ is0 .&. myPawns
          !isoPPDefdd = popCount $ _pAttacs usAtts .&. myPawns
          !is1  = (myPawns `unsafeShiftL` 8) .&. notPawns
          !is1d = bbLeft is1 .|. bbRight is1
          !is2  = (is1     `unsafeShiftL` 8) .&. notPawns
          !is2d = bbLeft is2 .|. bbRight is2
          !is3  = (is2     `unsafeShiftL` 8) .&. notPawns
          !is3d = bbLeft is3 .|. bbRight is3
          !isFuPAtts  = is1d .|. is2d .|. is3d	-- future pawns attacks (only first 3)
          !isoSupPwns = popCount $ myPawns .&. isFuPAtts
          ------ Pawn attacks & moves ------
          !pamAttacks = popCount $ _pAttacs usAtts
          !pamMoves   = popCount $ (myPawns `unsafeShiftL` 8) `less` occup p
          ------ Advanced pawns (not passed) ------
          !pnp = myPawns `less` passed p
          !advPawns5  = popCount $ pnp .&. row5
          !advPawns6  = popCount $ pnp .&. row6
          ------ Backward pawns --------
          (bpw, bpow) = backPawns White myPawns (pawns p .&. them) (_pAttacs themAtts)
          !bapClosed  = popCount bpw
          !bapOpen    = popCount bpow
          ------ (My) Attacks to adverse squares - we consider only last 2 ranks --------
          !advAtts = popCount $ _allAttacs usAtts .&. yoH
          yoH = row7 .|. row8
          ------ Margin penalty for minors ------
          !marKnights = popCount $ myKnights .&. margin
          !marBishops = popCount $ myBishops .&. margin
          margin = row1 .|. row8 .|. fileA .|. fileH
          ------ Mobility ------
          !yoA1       = _pAttacs themAtts .|. _nAttacs themAtts .|. _bAttacs themAtts
          !yoA2       = yoA1 .|. _rAttacs themAtts
          !mobKnights = popCount $ _nAttacs usAtts `less` (us .|. _pAttacs themAtts)
          !mobBishops = popCount $ _bAttacs usAtts `less` (us .|. _pAttacs themAtts)
          !mobRooks   = popCount $ _rAttacs usAtts `less` (us .|. yoA1)
          !mobQueens  = popCount $ _qAttacs usAtts `less` (us .|. yoA2)
          ------ Space for own pieces in our courtyard ------
          -- Inspired from Stockfish
          yard       = (fileC .|. fileD .|. fileE .|. fileF) .&. (row2 .|. row3 .|. row4)
          !safe      = (yard .&. (_allAttacs usAtts .|. complement (_allAttacs themAtts)))
                           `less` (myPawns .|. _pAttacs themAtts)
          !spaSafe   = popCount safe
          !behi      = shadowDown myPawns .&. safe
          !spaBehind = popCount behi
          ------ Center control ------
          center = 0x0000003C3C000000
          !cenPawns   = popCount $ _pAttacs usAtts .&. center
          !cenKnights = popCount $ _nAttacs usAtts .&. center
          !cenBishops = popCount $ _bAttacs usAtts .&. center
          !cenRooks   = popCount $ _rAttacs usAtts .&. center
          !cenQueens  = popCount $ _qAttacs usAtts .&. center
          !cenKing    = popCount $ _kAttacs usAtts .&. center
          ------ En prise: only queens, rooks & minors attacked by pawns ------
          ------ and queens & rooks attacked by minors
          !enpQP    = popCount $ myQueens .&. _pAttacs themAtts
          !enpRP    = popCount $ myRooks  .&. _pAttacs themAtts
          !myMinors = myKnights .|. myBishops
          !enpMP    = popCount $ myMinors .&. _pAttacs themAtts
          !yoMAtts  = _nAttacs themAtts .|. _bAttacs themAtts
          !enpQM    = popCount $ myQueens .&. yoMAtts
          !enpRM    = popCount $ myRooks .&. yoMAtts
          ------ (Your) King Safety ------
          !kisZPAtts = popCount $ ykZone .&. _pAttacs usAtts
          !kisZNAtts = popCount $ ykZone .&. _nAttacs usAtts
          !kisZBAtts = popCount $ ykZone .&. _bAttacs usAtts
          !kisZRAtts = popCount $ ykZone .&. _rAttacs usAtts
          !kisZQAtts = popCount $ ykZone .&. _qAttacs usAtts
          !kisFree   = popCount $ ykZone `less` (_allAttacs usAtts .|. them)
          !kpzExt1   = bbLeft ykPreZone .|. bbRight ykPreZone	-- pre attacks zone: +1 left/right
          !kpzExt    = kpzExt1 .|. (kpzExt1 `unsafeShiftR` 8)	-- and +1 down
          !kisPPAtts = popCount $ kpzExt .&. _pAttacs usAtts
          !kisPNAtts = popCount $ kpzExt .&. _nAttacs usAtts
          !kisPBAtts = popCount $ kpzExt .&. _bAttacs usAtts
          !kisPRAtts = popCount $ kpzExt .&. _rAttacs usAtts
          !kisPQAtts = popCount $ kpzExt .&. _qAttacs usAtts
          !kisPStorm = popCount $ kpzExt .&. myPawns
          ------ Passed pawns ------
          -- Every passed pawn will be evaluated separately and only the
          -- 3 pawns nearest to theyr promotion square will be kept
          pass3 = map (perPassedPawnWhite yksq (rooks p .|. queens p) us them
                           (_allAttacs usAtts) (_allAttacs themAtts))
              $ take 3 . reverse . sortBy (comparing fst) $ map addBehindWhite . bbToSquares
              $ passed p .&. us
          [ (papBehind1, papBlocked1, papCtrl1, papRBehind1, papAKDist1),
            (papBehind2, papBlocked2, papCtrl2, papRBehind2, papAKDist2),
            (papBehind3, papBlocked3, papCtrl3, papRBehind3, papAKDist3)]
              = take 3 $ pass3 ++ repeat (0, 0, 0, 0, 0)

-- The big evaluation function for black
sideEvalBlack :: MyPos -> BBoard -> BBoard -> AttBBs -> AttBBs -> U.Vector Int
sideEvalBlack !p !us !them !usAtts !themAtts = U.create $ do
    v <- V.new $ fromEnum LenEvalIdx
    setEval v MatPawns matPawns
    setEval v MatKnights matKnights
    setEval v MatBishops matBishops
    setEval v MatRooks matRooks
    setEval v MatQueens matQueens
    setEval v KopBAtts kopBAtts
    setEval v KopRAtts kopRAtts
    setEval v KipRank kipRank
    setEval v KipFilc kipFilc
    setEval v KipMissSh kipMissSh
    setEval v KipMissPs kipMissPs
    setEval v RopHOpen ropHOpen
    setEval v RopOpen ropOpen
    setEval v RopConn ropConn
    setEval v IsoPawnRow isoPawnRow
    setEval v IsoPPDefdd isoPPDefdd
    setEval v IsoSupPwns isoSupPwns
    setEval v PamAttacks pamAttacks
    setEval v PamMoves pamMoves
    setEval v AdvPawns5 advPawns5
    setEval v AdvPawns6 advPawns6
    setEval v BapClosed bapClosed
    setEval v BapOpen bapOpen
    setEval v AdvAtts advAtts
    setEval v MarKnights marKnights
    setEval v MarBishops marBishops
    setEval v MobKnights mobKnights
    setEval v MobBishops mobBishops
    setEval v MobRooks mobRooks
    setEval v MobQueens mobQueens
    setEval v SpaSafe spaSafe
    setEval v SpaBehind spaBehind
    setEval v CenPawns cenPawns
    setEval v CenKnights cenKnights
    setEval v CenBishops cenBishops
    setEval v CenRooks cenRooks
    setEval v CenQueens cenQueens
    setEval v CenKing cenKing
    setEval v EnpQP enpQP
    setEval v EnpRP enpRP
    setEval v EnpMP enpMP
    setEval v EnpQM enpQM
    setEval v EnpRM enpRM
    setEval v KisZPAtts kisZPAtts
    setEval v KisZNAtts kisZNAtts
    setEval v KisZBAtts kisZBAtts
    setEval v KisZRAtts kisZRAtts
    setEval v KisZQAtts kisZQAtts
    setEval v KisFree kisFree
    setEval v KisPPAtts kisPPAtts
    setEval v KisPNAtts kisPNAtts
    setEval v KisPBAtts kisPBAtts
    setEval v KisPRAtts kisPRAtts
    setEval v KisPQAtts kisPQAtts
    setEval v KisPStorm kisPStorm
    setEval v PapBehind1 papBehind1
    setEval v PapBlocked1 papBlocked1
    setEval v PapCtrl1 papCtrl1
    setEval v PapRBehind1 papRBehind1
    setEval v PapAKDist1 papAKDist1
    setEval v PapBehind2 papBehind2
    setEval v PapBlocked2 papBlocked2
    setEval v PapCtrl2 papCtrl2
    setEval v PapRBehind2 papRBehind2
    setEval v PapAKDist2 papAKDist2
    setEval v PapBehind3 papBehind3
    setEval v PapBlocked3 papBlocked3
    setEval v PapCtrl3 papCtrl3
    setEval v PapRBehind3 papRBehind3
    setEval v PapAKDist3 papAKDist3
    return v
    where -- we keep the features very basic and return fulfillment values
          -- which will be combined (mostly by weights multiplications) later
          ------ Material ------
          !myPawns    = pawns p .&. us
          !myKnights  = knights p .&. us
          !myBishops  = bishops p .&. us
          !myRooks    = rooks p .&. us
          !myQueens   = queens p .&. us
          !matPawns   = popCount myPawns
          !matKnights = popCount myKnights
          !matBishops = popCount myBishops
          !matRooks   = popCount myRooks
          !matQueens  = popCount myQueens
          ------ (Your) King openness ------
          !yksq     = kingSquare (kings p) them
          !notPawns = complement $ pawns p
          !kopBAtts = popCount $ bAttacs (pawns p) yksq .&. notPawns
          !kopRAtts = popCount $ rAttacs (pawns p) yksq .&. notPawns
          ------ (Your) King placement ------
          !kipRank = yksq `unsafeShiftR` 3	-- rank of the king
          !kif = yksq .&. 7		-- file of the king
          !kipFilc   = kif * (7 - kif)
          -- For pawns shelter quality:
          !ykZone    = _kAttacs themAtts
          !ykPreZone = (ykZone `unsafeShiftL` 8) `less` ykZone
          !ykShelter = ykPreZone `unsafeShiftR` 8
          !notYPawns = complement $ pawns p .&. them
          !kipMissSh = popCount $ ykShelter .&. notYPawns
          !kipMissPs = popCount $ ykPreZone .&. notYPawns
          ------ Rook placement ------
          (ropHOpen, ropOpen)
              = foldr (perRook (pawns p) myPawns) (0, 0) $ bbToSquares myRooks
          !ropConn | _rAttacs usAtts .&. myRooks == 0 = 0
                   | otherwise                        = 1
          ------ Pawn connectivity --------
          !is0  = bbLeft myPawns .|. bbRight myPawns
          !isoPawnRow = popCount $ is0 .&. myPawns
          !isoPPDefdd = popCount $ _pAttacs usAtts .&. myPawns
          !is1  = (myPawns `unsafeShiftR` 8) .&. notPawns
          !is1d = bbLeft is1 .|. bbRight is1
          !is2  = (is1     `unsafeShiftR` 8) .&. notPawns
          !is2d = bbLeft is2 .|. bbRight is2
          !is3  = (is2     `unsafeShiftR` 8) .&. notPawns
          !is3d = bbLeft is3 .|. bbRight is3
          !isFuPAtts  = is1d .|. is2d .|. is3d	-- future pawns attacks (only first 3)
          !isoSupPwns = popCount $ myPawns .&. isFuPAtts
          ------ Pawn attacks & moves ------
          !pamAttacks = popCount $ _pAttacs usAtts
          !pamMoves   = popCount $ (myPawns `unsafeShiftR` 8) `less` occup p
          ------ Advanced pawns (not passed) ------
          !pnp = myPawns `less` passed p
          !advPawns5  = popCount $ pnp .&. row4
          !advPawns6  = popCount $ pnp .&. row3
          ------ Backward pawns --------
          (bpw, bpow) = backPawns Black myPawns (pawns p .&. them) (_pAttacs themAtts)
          !bapClosed  = popCount bpw
          !bapOpen    = popCount bpow
          ------ (My) Attacks to adverse squares - we consider only last 2 ranks --------
          !advAtts = popCount $ _allAttacs usAtts .&. yoH
          yoH = row2 .|. row1
          ------ Margin penalty for minors ------
          !marKnights = popCount $ myKnights .&. margin
          !marBishops = popCount $ myBishops .&. margin
          margin = row1 .|. row8 .|. fileA .|. fileH
          ------ Mobility ------
          !yoA1       = _pAttacs themAtts .|. _nAttacs themAtts .|. _bAttacs themAtts
          !yoA2       = yoA1 .|. _rAttacs themAtts
          !mobKnights = popCount $ _nAttacs usAtts `less` (us .|. _pAttacs themAtts)
          !mobBishops = popCount $ _bAttacs usAtts `less` (us .|. _pAttacs themAtts)
          !mobRooks   = popCount $ _rAttacs usAtts `less` (us .|. yoA1)
          !mobQueens  = popCount $ _qAttacs usAtts `less` (us .|. yoA2)
          ------ Space for own pieces in our courtyard ------
          -- Inspired from Stockfish
          yard       = (fileC .|. fileD .|. fileE .|. fileF) .&. (row7 .|. row6 .|. row5)
          !safe      = (yard .&. (_allAttacs usAtts .|. complement (_allAttacs themAtts)))
                           `less` (myPawns .|. _pAttacs themAtts)
          !spaSafe   = popCount safe
          !behi      = shadowUp myPawns .&. safe
          !spaBehind = popCount behi
          ------ Center control ------
          center = 0x0000003C3C000000
          !cenPawns   = popCount $ _pAttacs usAtts .&. center
          !cenKnights = popCount $ _nAttacs usAtts .&. center
          !cenBishops = popCount $ _bAttacs usAtts .&. center
          !cenRooks   = popCount $ _rAttacs usAtts .&. center
          !cenQueens  = popCount $ _qAttacs usAtts .&. center
          !cenKing    = popCount $ _kAttacs usAtts .&. center
          ------ En prise: only queens, rooks & minors attacked by pawns ------
          ------ and queens & rooks attacked by minors
          !enpQP    = popCount $ myQueens .&. _pAttacs themAtts
          !enpRP    = popCount $ myRooks  .&. _pAttacs themAtts
          !myMinors = myKnights .|. myBishops
          !enpMP    = popCount $ myMinors .&. _pAttacs themAtts
          !yoMAtts  = _nAttacs themAtts .|. _bAttacs themAtts
          !enpQM    = popCount $ myQueens .&. yoMAtts
          !enpRM    = popCount $ myRooks .&. yoMAtts
          ------ (Your) King Safety ------
          !kisZPAtts = popCount $ ykZone .&. _pAttacs usAtts
          !kisZNAtts = popCount $ ykZone .&. _nAttacs usAtts
          !kisZBAtts = popCount $ ykZone .&. _bAttacs usAtts
          !kisZRAtts = popCount $ ykZone .&. _rAttacs usAtts
          !kisZQAtts = popCount $ ykZone .&. _qAttacs usAtts
          !kisFree   = popCount $ ykZone `less` (_allAttacs usAtts .|. them)
          !kpzExt1   = bbLeft ykPreZone .|. bbRight ykPreZone	-- pre attacks zone: +1 left/right
          !kpzExt    = kpzExt1 .|. (kpzExt1 `unsafeShiftL` 8)	-- and +1 down
          !kisPPAtts = popCount $ kpzExt .&. _pAttacs usAtts
          !kisPNAtts = popCount $ kpzExt .&. _nAttacs usAtts
          !kisPBAtts = popCount $ kpzExt .&. _bAttacs usAtts
          !kisPRAtts = popCount $ kpzExt .&. _rAttacs usAtts
          !kisPQAtts = popCount $ kpzExt .&. _qAttacs usAtts
          !kisPStorm = popCount $ kpzExt .&. myPawns
          ------ Passed pawns ------
          -- Every passed pawn will be evaluated separately and only the
          -- 3 pawns nearest to theyr promotion square will be kept
          pass3 = map (perPassedPawnBlack yksq (rooks p .|. queens p) us them
                           (_allAttacs usAtts) (_allAttacs themAtts))
              $ take 3 . reverse . sortBy (comparing fst) $ map addBehindBlack . bbToSquares
              $ passed p .&. us
          [ (papBehind1, papBlocked1, papCtrl1, papRBehind1, papAKDist1),
            (papBehind2, papBlocked2, papCtrl2, papRBehind2, papAKDist2),
            (papBehind3, papBlocked3, papCtrl3, papRBehind3, papAKDist3)]
              = take 3 $ pass3 ++ repeat (0, 0, 0, 0, 0)

perPassedPawnWhite :: Square -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> (Int, Square)
                   -> (Int, Int, Int, Int, Int)
perPassedPawnWhite yoksq rqs moi toi moia toia (be, sq)
    = perPassedPawnOk sqa yoksq be rqs sqab behind moi toi moia toia
    where !sqbb   = 1 `unsafeShiftL` sq
          !behind = shadowDown sqbb
          !sqa    = sq + 8
          !sqab   = sqbb `unsafeShiftL` 8

perPassedPawnBlack :: Square -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> (Int, Square)
                   -> (Int, Int, Int, Int, Int)
perPassedPawnBlack yoksq rqs moi toi moia toia (be, sq)
    = perPassedPawnOk sqa yoksq be rqs sqab behind moi toi moia toia
    where !sqbb   = 1 `unsafeShiftL` sq
          !behind = shadowUp sqbb
          !sqa    = sq - 8
          !sqab   = sqbb `unsafeShiftR` 8

-- For every passed pawn: we qualify it by the following parameters:
-- how far is from initial pawn position (1 - initial, 6 - last rank before promotion)
-- if it is blocked: 0 - no, 1 - by own piece, 2 - by adverse piece
-- if we control the advance field: 0 - no, 1 - we have attacks, -1 - adverse attacks
-- if it has a rook/queen behind: 0 - none, -1 - adverse, 1 - own
-- distance of adverse king to the advance field
perPassedPawnOk :: Square -> Square -> Int -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard
                -> (Int, Int, Int, Int, Int)
perPassedPawnOk sq yoksq be rqs sqab behind moi toi moia toia = (be, blocked, control, rbehind, akd)
    where blocked | sqab .&. moi /= 0 = 1
                  | sqab .&. toi /= 0 = 2
                  | otherwise         = 0
          !fbehind = rAttacs rqs sq .&. behind .&. rqs
          rbehind | fbehind .&. moi /= 0 =  1
                  | fbehind .&. toi /= 0 = -1
                  | otherwise            =  0
          control | sqab .&. moia /= 0 =  1
                  | rbehind == 1       =  1
                  | sqab .&. toia /= 0 = -1
                  | otherwise          =  0
          akd = squareDistance sq yoksq

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
