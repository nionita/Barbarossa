{-# LANGUAGE BangPatterns, PatternGuards, ExistentialQuantification #-}
module Eval.Eval (
    initEvalState,
    posEval,
    maxStatsDepth, maxStatsIntvs,
    inLimits,
    paramNames, parLims, parDim, paramPairs
) where

import Prelude hiding ((++), head, foldl, map, concat, filter, takeWhile, iterate, sum,
                       zip, zipWith, foldr, concatMap, length, replicate, lookup, repeat)
import Data.Array.Base (unsafeAt)
import Data.Bits hiding (popCount)
import Data.List.Stream
import Control.Monad.State.Lazy
import Data.Array.Unboxed

import Struct.Struct
import Struct.Status
import Moves.Moves
import Moves.BitBoard
import Moves.Muster
-- import Eval.Gradient
-- import Eval.BasicEval

-- Criteria (x2, once for every player):
-- + number of every piece (except king) (5 times)
-- + king safety 1st zone: opponents attacs & own defends (own & opponent's king)
-- * king safety 2nd zone: opponents attacs & own defends (own & opponent's king)
-- + king openness (attacs of a rook or bishop from the kings square) if the opponent has rook, bishop or queen
-- + attacs count
-- + attacs count in the center
-- + attacs count in the opponents fields
-- * pass pawns
-- * semifree pawns
-- * minimum squares to transform when pass
-- * minimum squares to transform when semi pass
-- * sum of squares to transform (all pawns)
-- * number of pawn groups
-- * number of undefended pieces

type IParams = [Int]
type DParams = [Double]
type Limits = [(Double, Double)]

class EvalItem a where
    evalItem    :: MyPos -> a -> IParams
    evalItemNDL :: a -> [(String, (Double, (Double, Double)))]	-- Name, Default, Limits

-- some handy functions for eval item types:
paramName :: (a, b) -> a
paramName    = fst

paramDefault :: (a, (b, c)) -> b
paramDefault = fst . snd

paramLimits :: (a, (b, c)) -> c
paramLimits  = snd . snd

data AnyEvalItem = forall a . EvalItem a => EvIt a

-- This is the list of evaluated characteristics of a positions
-- Every item can have one or more parameters which have a name, a default value
-- and a range of values (values are kept for learning purposes as doubles,
-- but for the evaluation itself one copy of integer parameter values is also kept)
evalItems :: [AnyEvalItem]
evalItems = [ EvIt Material,	-- material balance (i.e. white - black material
              -- EvIt EnPrise,	-- when not quiescent - pieces en prise
              EvIt Redundance,	-- bishop pair and rook redundance
              EvIt NRCorrection,	-- material correction for knights & rooks
              EvIt RookPawn,	-- the rook pawns are about 15% less valuable
              EvIt KingSafe,	-- king safety
              EvIt KingOpen,	-- malus for king openness
              -- EvIt KingCenter,	-- malus for king on center files
              EvIt KingPlace,	-- bonus when king is near some fields
              -- EvIt KingMob,	-- bonus for restricted mobility of adverse king when alone
              -- EvIt Castles,	-- bonus for castle rights
              EvIt LastLine,	-- malus for pieces on last line (except rooks and king)
              EvIt Mobility,	-- pieces mobility
              EvIt Center,	-- attacs of center squares
              -- EvIt DblPawns,	-- malus for doubled pawns
              EvIt PassPawns	-- pass pawns
            ]

parDim :: Int
parDim = sum $ map evalLen evalItems
    where evalLen (EvIt a) = length $ evalItemNDL a

parLims :: [(Double, Double)]
parLims = concatMap evalLims evalItems
    where evalLims (EvIt a) = map paramLimits $ evalItemNDL a

-- zeroParam :: DParams
-- zeroParam = replicate parDim 0	-- theese are doubles

zeroFeats :: [Int]
zeroFeats = replicate parDim 0	-- theese are ints

evalItemPar :: EvalItem a => a -> DParams -> (String, Double) -> Maybe DParams
evalItemPar a dps (s, v) = lookup s (zip lu posi) >>= \i -> Just (replace dps i v)
    where lu = map paramName $ evalItemNDL a
          replace []       _ _ = []
          replace (_ : ds) 0 v' = v' : ds
          replace (d : ds) i v' = d : replace ds (i-1) v'
          posi = [0..] :: [Int]

oneParam :: [(AnyEvalItem, DParams)] -> (String, Double) -> [(AnyEvalItem, DParams)]
oneParam [] _ = []
oneParam (evp@(EvIt ei, dp) : evps) sd
    | Just ndp <- evalItemPar ei dp sd = (EvIt ei, ndp) : evps
    | otherwise = evp : oneParam evps sd

-- Map a list of parameter assignments (name, value)
-- to a vector of parameter, taking defaults for missing parameters
allParams :: [(String, Double)] -> DParams
allParams = concatMap snd . foldl oneParam defevps
    where defevps = map defp evalItems
          defp ei@(EvIt a) = (ei, map paramDefault $ evalItemNDL a)

paramNames :: [String]
paramNames = concatMap pnames evalItems
    where pnames (EvIt a) = map paramName $ evalItemNDL a

paramPairs :: [Int] -> [(String, Int)]
paramPairs = zip paramNames

------------------------------------------------------------------
-- Parameters of this module ------------
granCoarse, granCoarse2, granCoarseM, maxStatsDepth, maxStatsIntvs, shift2Cp :: Int
granCoarse    = 4	-- coarse granularity
granCoarse2   = granCoarse `div` 2
granCoarseM   = complement (granCoarse - 1)
shift2Cp      = 3	-- we have 2^shift2Cp units per centipawn
maxStatsDepth = 12	-- for error statistics of the eval function - maximum depth
maxStatsIntvs = 20	-- number of difference interval
-- statsIntv     = 25	-- difference interval length

-- subOptimal :: Double
-- subOptimal    = 2	-- optimal step is so many times smaller

-- samplesPerChange :: Int
-- samplesPerChange = 10	-- number of samples before a parameter change occurs
-----------------------------------------------

initEvalState :: [(String, Double)] -> EvalState
initEvalState sds = EvalState {
        esDParams = params,
        esIParams = map round params
    }
    where params = inLimits parLims $ allParams sds

inLimits :: Limits -> DParams -> DParams
inLimits ls ps = map inlim $ zip ls ps
    where inlim ((mi, ma), p) = max mi $ min ma p

(<*>) :: Num a => [a] -> [a] -> a
a <*> b = sum $ zipWith (*) a b
{-# SPECIALIZE (<*>) :: [Int] -> [Int] -> Int #-}

matesc :: Int
matesc = 20000 - 255	-- attention, this is also defined in Base.hs!!

posEval :: MyPos -> State EvalState (Int, [Int])
posEval !p = do
    sti <- get
    let (sce, feat) = evalDispatch p sti
        !scl = min matesc $ max (-matesc) sce
        !scc = if granCoarse > 0 then (scl + granCoarse2) .&. granCoarseM else scl
        -- !sc = if moving p == White then scc else -scc
    return (scc, feat)

evalDispatch :: MyPos -> EvalState -> (Int, [Int])
evalDispatch p sti
    | pawns p == 0 = evalNoPawns p sti
    | pawns p .&. me p == 0 ||
      pawns p .&. yo p == 0 = evalSideNoPawns p sti
    | otherwise    = normalEval p sti

itemEval :: MyPos -> AnyEvalItem -> [Int]
itemEval p (EvIt a) = evalItem p a

normalEval :: MyPos -> EvalState -> (Int, [Int])
normalEval p sti = (sc, feat)
    where !feat = concatMap (itemEval p) evalItems
          !sc   = feat <*> esIParams sti `shiftR` shift2Cp

evalSideNoPawns :: MyPos -> EvalState -> (Int, [Int])
evalSideNoPawns p sti
    | npwin && insufficient = (0, zeroFeats)
    | otherwise             = (nsc, feats)
    where (nsc, feats) = normalEval p sti
          npside = if pawns p .&. me p == 0 then me p else yo p
          npwin = npside == me p && nsc > 0 || npside == yo p && nsc < 0
          insufficient = majorcnt == 0 && (minorcnt == 1 || minorcnt == 2 && bishopcnt == 0)
          bishopcnt = popCount1 $ bishops p .&. npside
          minorcnt  = popCount1 $ (bishops p .|. knights p) .&. npside
          majorcnt  = popCount1 $ (queens p .|. rooks p) .&. npside

-- These evaluation function distiguishes between some known finals with no pawns
evalNoPawns :: MyPos -> EvalState -> (Int, [Int])
evalNoPawns p sti = (sc, zeroFeats)
    where !sc | onlykings   = 0
              | kmk || knnk = 0		-- one minor or two knights
              | kbbk        = mateKBBK p kaloneyo	-- 2 bishops
              | kbnk        = mateKBNK p kaloneyo	-- bishop + knight
              | kMxk        = mateKMajxK p kaloneyo	-- simple mate with at least one major
              -- | kqkx        = mateQRest p kaloneb	-- queen against minor or rook
              | otherwise   = fst $ normalEval p sti
          kaloneme = me p `less` kings p == 0
          kaloneyo = yo p `less` kings p == 0
          onlykings = kaloneme && kaloneyo
          kmk  = (kaloneme || kaloneyo) && minorcnt == 1 && majorcnt == 0
          knnk = (kaloneme || kaloneyo) && minorcnt == 2 && majorcnt == 0 && bishops p == 0
          kbbk = (kaloneme || kaloneyo) && minorcnt == 2 && majorcnt == 0 && knights p == 0
          kbnk = (kaloneme || kaloneyo) && minorcnt == 2 && not (knnk || kbbk)
          kMxk = (kaloneme || kaloneyo) && majorcnt > 0
          minor   = bishops p .|. knights p
          minorcnt = popCount1 minor
          major    = queens p .|. rooks p
          majorcnt = popCount1 major

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
    where !kadv = if mywin then kb else kw
          !kw = kingSquare (kings p) (me p)
          !kb = kingSquare (kings p) (me p)
          !distk = squareDistance kw kb
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

-- Some helper functions:

{-# INLINE zoneAttacs #-}
zoneAttacs :: MyPos -> BBoard -> (Int, Int)
zoneAttacs p zone = (m, y)
    where m = popCount $ zone .&. myAttacs p
          y = popCount $ zone .&. yoAttacs p

----------------------------------------------------------------------------
-- Here we have the implementation of the evaluation items
-- They do not return a score, but a vector of fulfillments of some criteria
-- With version 0.55 we compute everything from white point of view
-- and only at the end we negate the score if black side is asked
----------------------------------------------------------------------------
------ King Safety ------
data KingSafe = KingSafe

instance EvalItem KingSafe where
    evalItem p _  = kingSafe p
    evalItemNDL _ = [("kingSafe", (1, (0, 20)))]

-- Rewrite of king safety taking into account number and quality
-- of pieces attacking king neighbour squares
-- This function is almost optimised, it could perhaps be faster
-- if we eliminate the lists
kingSafe :: MyPos -> [Int]
kingSafe !p = [ksafe]
    where !ksafe = mattacs - yattacs
          !freem = popCount1 $ myKAttacs p .&. yoAttacs p `less` me p
          !freey = popCount1 $ yoKAttacs p .&. myAttacs p `less` yo p
          flag k a = if k .&. a /= 0 then 1 else 0
          qual k a = popCount1 $ k .&. a
          flagYo = flag (myKAttacs p)
          flagMe = flag (yoKAttacs p)
          qualYo = qual (myKAttacs p)
          qualMe = qual (yoKAttacs p)
          ($:) = flip ($)
          attsm = map (p $:) [ myPAttacs, myNAttacs, myBAttacs, myRAttacs, myQAttacs, myKAttacs ]
          !ixm = max 0 $ min 63 $ fm * cm - freey
          attsy = map (p $:) [ yoPAttacs, yoNAttacs, yoBAttacs, yoRAttacs, yoQAttacs, yoKAttacs ]
          !ixy = max 0 $ min 63 $ fy * cy - freem
          !mattacs = attCoef `unsafeAt` ixm
          !yattacs = attCoef `unsafeAt` ixy
          qualWeights = [1, 1, 1, 2, 3, 1]
          !(Flc fm cm) = sumCount flagMe qualMe $ zip attsm qualWeights
          !(Flc fy cy) = sumCount flagYo qualYo $ zip attsy qualWeights

-- To make the sum and count in one pass
data Flc = Flc !Int !Int

{-# INLINE sumCount #-}
sumCount :: (BBoard -> Int) -> (BBoard -> Int) -> [(BBoard, Int)] -> Flc
sumCount flag qual = foldl' (\(Flc f c) (b, i) -> Flc (f + flag b) (c + i * qual b)) (Flc 0 0)

attCoef :: UArray Int Int
attCoef = listArray (0, 63) [ f x | x <- [0..63] ]
    where f :: Int -> Int
          f x = let y = fromIntegral x :: Double in round $ (2.92968750 - 0.03051758*y)*y*y

kingSquare :: BBoard -> BBoard -> Square
kingSquare kingsb colorp = head $ bbToSquares $ kingsb .&. colorp
{-# INLINE kingSquare #-}

------ Material ------
data Material = Material

instance EvalItem Material where
    evalItem p _  = materDiff p
    evalItemNDL _ = [("materialDiff", (8, (8, 8)))]

materDiff :: MyPos -> IParams
materDiff p = [md]
    where !md | moving p == White =   mater p
              | otherwise         = - mater p

------ King openness ------
data KingOpen = KingOpen

instance EvalItem KingOpen where
    evalItem p _  = kingOpen p
    evalItemNDL _ = [ ("kingOpenOwn", (-20, (-48, 1))), ("kingOpenAdv", (20, (0, 32)))] 

-- Openness can be tought only with pawns (like we take) or all pieces
-- This function is optimized
kingOpen :: MyPos -> IParams
kingOpen p = [own, adv]
    where mopbishops = popCount1 $ bishops p .&. yo p
          moprooks   = popCount1 $ rooks p .&. yo p
          mopqueens  = popCount1 $ queens p .&. yo p
          mwb = popCount $ bAttacs paw msq `less` paw
          mwr = popCount $ rAttacs paw msq `less` paw
          yopbishops = popCount1 $ bishops p .&. me p
          yoprooks   = popCount1 $ rooks p .&. me p
          yopqueens  = popCount1 $ queens p .&. me p
          ywb = popCount $ bAttacs paw ysq `less` paw
          ywr = popCount $ rAttacs paw ysq `less` paw
          paw = pawns p
          msq = kingSquare (kings p) $ me p
          ysq = kingSquare (kings p) $ yo p
          comb !oB !oR !oQ !wb !wr = let !v = oB * wb + oR * wr + oQ * (wb + wr) in v
          !own = comb mopbishops moprooks mopqueens mwb mwr
          !adv = comb yopbishops yoprooks yopqueens ywb ywr

------ King on a center file ------
data KingCenter = KingCenter

instance EvalItem KingCenter where
    evalItem p _  = kingCenter p
    evalItemNDL _ = [ ("kingCenter", (-120, (-200, 0))) ]

-- This function is optimised
kingCenter :: MyPos -> IParams
kingCenter p = [ kcd ]
    where kcenter = fileC .|. fileD .|. fileE .|. fileF
          !wkc = popCount1 (kings p .&. me p .&. kcenter) * (brooks + 2 * bqueens - 1)
          !bkc = popCount1 (kings p .&. yo p .&. kcenter) * (wrooks + 2 * wqueens - 1)
          !kcd = wkc - bkc
          !wrooks  = popCount1 $ rooks  p .&. me p
          !wqueens = popCount1 $ queens p .&. me p
          !brooks  = popCount1 $ rooks  p .&. yo p
          !bqueens = popCount1 $ queens p .&. yo p

------ King placement ------
data KingPlace = KingPlace

instance EvalItem KingPlace where
    evalItem p _  = kingPlace p
    evalItemNDL _ = [
                      ("kingPlaceCent", (4, (0, 400))),
                      ("kingPlacePwns", (4, (0, 400)))
                    ]

-- Parameters of the king placement
materRook, materQueen, materFree :: Int
materRook  = 2
materQueen = 5
materFree  = 3
materBonusScale, pawnBonusScale :: Int
materBonusScale = 3
pawnBonusScale  = 4
-- Variants for scales mater/pawn:
-- orig: 4/5

-- Depending on which pieces are on the board we have some preferences
-- where the king should be placed. For example, in the opening and middle game it should
-- be in some corner, in endgame it should be near some (passed) pawn(s)
kingPlace :: MyPos -> IParams
kingPlace p = [ kcd, kpd ]
    where !kcd = mpl - ypl
          !kpd = mpi - ypi
          !mks = kingSquare (kings p) $ me p
          !yks = kingSquare (kings p) $ yo p
          !mkm = yminor + materRook * yrooks + materQueen * yqueens - materFree
          !ykm = mminor + materRook * mrooks + materQueen * mqueens - materFree
          !mpl = kingMaterBonus mpawns mkm mks
          !ypl = kingMaterBonus ypawns ykm yks
          !mpi | passed p /= 0            = kingPawnsBonus c mks (passed p) mpassed ypassed
               | mkm <= 0 && pawns p /= 0 = kingPawnsBonus c mks (pawns  p) mpawns  ypawns
               | otherwise                = 0
          !ypi | passed p /= 0            = kingPawnsBonus c yks (passed p) mpassed ypassed
               | ykm <= 0 && pawns p /= 0 = kingPawnsBonus c yks (pawns  p) mpawns  ypawns
               | otherwise                = 0
          !mrooks  = popCount1 $ rooks p .&. me p
          !mqueens = popCount1 $ queens p .&. me p
          !mminor  = popCount1 $ (bishops p .|. knights p) .&. me p
          !yrooks  = popCount1 $ rooks p .&. yo p
          !yqueens = popCount1 $ queens p .&. yo p
          !yminor  = popCount1 $ (bishops p .|. knights p) .&. yo p
          !mpawns  = pawns p .&. me p
          !ypawns  = pawns p .&. yo p
          !mpassed = passed p .&. me p
          !ypassed = passed p .&. yo p
          !c = moving p

-- We give bonus also for pawn promotion squares, if the pawn is near enough to promote
-- Give as parameter bitboards for all pawns, white pawns and black pawns for performance
kingPawnsBonus :: Color -> Square -> BBoard -> BBoard -> BBoard -> Int
kingPawnsBonus White !ksq !alp !mbb !ybb = kingPawnsBonus' ksq alp wHalf bHalf
    where !wHalf = 0x00000000FFFFFFFF .&. ybb
          !bHalf = 0xFFFFFFFF00000000 .&. mbb
kingPawnsBonus Black !ksq !alp !mbb !ybb = kingPawnsBonus' ksq alp wHalf bHalf
    where !wHalf = 0x00000000FFFFFFFF .&. mbb
          !bHalf = 0xFFFFFFFF00000000 .&. ybb

promoW, promoB :: Square -> Square
promoW s = 56 + (s .&. 7)
promoB s =       s .&. 7

promoFieldDistIncr :: Int -> Int
promoFieldDistIncr = \d -> d + 1

kingPawnsBonus' :: Square -> BBoard -> BBoard -> BBoard -> Int
kingPawnsBonus' !ksq !alp !wHalf !bHalf = bonus
    where !bpsqs = sum $ map (proxyBonus . squareDistance ksq) $ bbToSquares alp
          !bqsqs = sum $ map (proxyBonus . promoFieldDistIncr . squareDistance ksq)
                       $ map promoW (bbToSquares bHalf) ++ map promoB (bbToSquares wHalf)
          !bonus = (bpsqs + bqsqs) `unsafeShiftR` pawnBonusScale

kingMaterBonus :: BBoard -> Int -> Square -> Int
kingMaterBonus myp mat ksq = bonus
    where !bonus = (mat * prx) `unsafeShiftR` materBonusScale
          !prx = proxyBonus (squareDistance ksq wa) * opawns wa
               + proxyBonus (squareDistance ksq wh) * opawns wh
               + proxyBonus (squareDistance ksq ba) * opawns ba
               + proxyBonus (squareDistance ksq bh) * opawns bh
               -- + proxyLine 0 ksq
               -- + proxyLine 7 ksq
          opawns s = popCount1 $ kAttacs s .&. myp
          [wa, wh] = bbToSquares $ row1 .&. (fileB .|. fileG)
          [ba, bh] = bbToSquares $ row8 .&. (fileB .|. fileG)

-- Make it longer, for artificially increased distances
proxyBonusArr :: UArray Int Int -- 0   1   2   3  4  5  6  7
proxyBonusArr = listArray (0, 15) $ [75, 25, 10, 5, 3, 2, 1] ++ repeat 0

proxyLineArr :: UArray Int Int -- 7  6  5  4  3  2   1   0   1   2   3  4  5  6  7
proxyLineArr = listArray (-7, 7) [0, 1, 2, 3, 5, 10, 25, 75, 25, 10, 5, 3, 2, 1, 0]

proxyBonus :: Int -> Int
proxyBonus = unsafeAt proxyBonusArr

proxyLine :: Int -> Square -> Int
proxyLine line sq = proxyBonusArr `unsafeAt` (unsafeShiftR sq 3 - line)

------ Mobility ------
data Mobility = Mobility	-- "safe" moves

instance EvalItem Mobility where
    evalItem p _  = mobDiff p
    evalItemNDL _ = [ ("mobilityKnight", (72, (60, 100))),
                      ("mobilityBishop", (72, (60, 100))),
                      ("mobilityRook", (48, (40, 100))),
                      ("mobilityQueen", (3, (0, 50))) ]

-- Here we do not calculate pawn mobility (which, calculated as attacs, is useless)
mobDiff :: MyPos -> IParams
mobDiff p = [n, b, r, q]
    where !myN = popCount1 $ myNAttacs p `less` (me p .|. yoPAttacs p)
          !myB = popCount1 $ myBAttacs p `less` (me p .|. yoPAttacs p)
          !myR = popCount1 $ myRAttacs p `less` (me p .|. yoA1)
          !myQ = popCount1 $ myQAttacs p `less` (me p .|. yoA2)
          !yoA1 = yoPAttacs p .|. yoNAttacs p .|. yoBAttacs p
          !yoA2 = yoA1 .|. yoRAttacs p
          !yoN = popCount1 $ yoNAttacs p `less` (yo p .|. myPAttacs p)
          !yoB = popCount1 $ yoBAttacs p `less` (yo p .|. myPAttacs p)
          !yoR = popCount1 $ yoRAttacs p `less` (yo p .|. myA1)
          !yoQ = popCount1 $ yoQAttacs p `less` (yo p .|. myA2)
          !myA1 = myPAttacs p .|. myNAttacs p .|. myBAttacs p
          !myA2 = myA1 .|. myRAttacs p
          !n = myN - yoN
          !b = myB - yoB
          !r = myR - yoR
          !q = myQ - yoQ

------ Center control ------
data Center = Center

instance EvalItem Center where
    evalItem p _  = centerDiff p
    evalItemNDL _ = [("centerAttacs", (72, (50, 100)))]

-- This function is already optimised
centerDiff :: MyPos -> IParams
centerDiff p = [wb]
    where (w, b) = zoneAttacs p center
          !wb = w - b
          center = 0x0000003C3C000000

------ En prise ------
--data EnPrise = EnPrise
--
--instance EvalItem EnPrise where
--    evalItem p c _ = enPrise p c
--    evalItemNDL _  = [("enPriseFrac", (10, (0, 100)))]

-- Here we could also take care who is moving and even if it's check - now we don't
--enPrise :: MyPos -> Color -> IParams
--enPrise p _ = [epp]
--    where !ko = popCount1 $ white p .&. knights p .&. blAttacs p
--          !ka = popCount1 $ black p .&. knights p .&. whAttacs p
--          !bo = popCount1 $ white p .&. bishops p .&. blAttacs p
--          !ba = popCount1 $ black p .&. bishops p .&. whAttacs p
--          !ro = popCount1 $ white p .&. rooks p .&. blAttacs p
--          !ra = popCount1 $ black p .&. rooks p .&. whAttacs p
--          !qo = popCount1 $ white p .&. queens p .&. blAttacs p
--          !qa = popCount1 $ black p .&. queens p .&. whAttacs p
--          !k = (ka - ko) * matPiece White Knight
--          !b = (ba - bo) * matPiece White Bishop
--          !r = (ra - ro) * matPiece White Rook
--          !q = (qa - qo) * matPiece White Queen
--          !epp = (k + b + r + q) `div` 100

------ Castle rights ------
--data Castles = Castles
--
--instance EvalItem Castles where
--    evalItem p c _ = castles p c
--    evalItemNDL _ = [("castlePoints", (0, (-50, 200)))]

-- This will have to be replaced, because not the castle rights are important, but
-- the king safety and the rook mobility
--castles :: MyPos -> Color -> IParams
--castles p _ = [crd]
--    where (ok, ork, orq, ak, ark, arq) = (4, 7, 0, 60, 63, 56)
--          !epc = epcas p
--          !okmoved = not $ epc `testBit` ok
--          !akmoved = not $ epc `testBit` ak
--          !orkc = if epc `testBit` ork then 1 else 0
--          !arkc = if epc `testBit` ark then 1 else 0
--          !orqc = if epc `testBit` orq then 1 else 0
--          !arqc = if epc `testBit` arq then 1 else 0
--          !co = if okmoved then 0 else orkc + orqc
--          !ca = if akmoved then 0 else arkc + arqc
--          !cdiff = co - ca
--          !qfact = popCount1 $ queens p
--          !rfact = popCount1 $ rooks p
--          !crd = cdiff * (2 * qfact + rfact)

------ Last Line ------
data LastLine = LastLine

instance EvalItem LastLine where
    evalItem p _  = lastline p
    evalItemNDL _ = [("lastLinePenalty", (8, (0, 24)))]

-- This function is already optimised
lastline :: MyPos -> IParams
lastline p = [cdiff]
    where !whl = popCount1 $ (me p `less` (rooks p .|. kings p)) .&. 0xFF
          !bll = popCount1 $ (yo p `less` (rooks p .|. kings p)) .&. 0xFF00000000000000
          !cdiff = bll - whl

------ King Mobility when alone ------
--data KingMob = KingMob
--
--instance EvalItem KingMob where
--    evalItem p c _ = kingAlone p c
--    evalItemNDL _ = [("advKingAlone", (26, (0, 100)))]
--
--kingAlone :: MyPos -> Color -> IParams
--kingAlone p _ = [kmb]
--    where !kmb = if okalone then 8 - okmvs + together else 0
--          !together = popCount1 $ whKAttacs p .&. blKAttacs p
--          !okmvs = popCount1 $ blAttacs p
--          !okalone = black p `less` kings p == 0

------ Redundance: bishop pair and rook redundance ------
data Redundance = Redundance

instance EvalItem Redundance where
    evalItem p _  = evalRedundance p
    evalItemNDL _ = [("bishopPair",       (320,  (100, 400))),
                     ("redundanceRook",   (-104,  (-150, 0))) ]

-- This function is optimised
evalRedundance :: MyPos -> [Int]
evalRedundance p = [bp, rr]
    where !wbl = bishops p .&. me p .&. lightSquares
          !wbd = bishops p .&. me p .&. darkSquares
          !bbl = bishops p .&. yo p .&. lightSquares
          !bbd = bishops p .&. yo p .&. darkSquares
          !bpw = popCount1 wbl .&. popCount1 wbd	-- tricky here: exact 1 and 1 is ok
          !bpb = popCount1 bbl .&. popCount1 bbd	-- and here
          !bp  = bpw - bpb
          !wro = rooks p .&. me p
          !bro = rooks p .&. yo p
          !wrr = popCount1 wro `unsafeShiftR` 1	-- tricky here: 2, 3 are the same...
          !brr = popCount1 bro `unsafeShiftR` 1	-- and here
          !rr  = wrr - brr

------ Knight & Rook correction according to own pawns ------
data NRCorrection = NRCorrection

instance EvalItem NRCorrection where
    evalItem p _  = evalNRCorrection p
    evalItemNDL _ = [("nrCorrection", (0, (0, 8)))]

-- This function seems to be already optimised
evalNRCorrection :: MyPos -> [Int]
evalNRCorrection p = [md]
    where !wpc = popCount1 (pawns p .&. me p) - 5
          !bpc = popCount1 (pawns p .&. yo p) - 5
          !wnp = popCount1 (knights p .&. me p) * wpc * 6	-- 1/16 for each pawn over 5
          !bnp = popCount1 (knights p .&. yo p) * bpc * 6	-- 1/16 for each pawn over 5
          !wrp = - popCount1 (rooks p .&. me p) * wpc * 12	-- 1/8 for each pawn under 5
          !brp = - popCount1 (rooks p .&. yo p) * bpc * 12	-- 1/8 for each pawn under 5
          !md = wnp + wrp - bnp - brp

------ Rook pawn weakness ------
data RookPawn = RookPawn

instance EvalItem RookPawn where
    evalItem p _  = evalRookPawn p
    evalItemNDL _ = [("rookPawn", (-64, (-120, 0))) ]

-- This function is already optimised
evalRookPawn :: MyPos -> [Int]
evalRookPawn p = [rps]
    where !wrp = popCount1 $ pawns p .&. me p .&. rookFiles
          !brp = popCount1 $ pawns p .&. yo p .&. rookFiles
          !rps = wrp - brp

------ Pass pawns ------
data PassPawns = PassPawns

instance EvalItem PassPawns where
    evalItem p _  = passPawns p
    evalItemNDL _ = [("passPawnBonus", (104, (  0, 160))),
                     ("passPawn4",     (424, (400, 480))),
                     ("passPawn5",     (520, (520, 640))),
                     ("passPawn6",     (1132, (1100, 1200))),
                     ("passPawn7",     (1920, (1600, 2300))) ]

passPawns :: MyPos -> IParams
passPawns p = [dfp, dfp4, dfp5, dfp6, dfp7]
    where !wfpbb = passed p .&. me p
          !bfpbb = passed p .&. yo p
          !wfp  = popCount1   wfpbb
          !wfp4 = popCount1 $ wfpbb .&. row4m
          !wfp5 = popCount1 $ wfpbb .&. row5m
          !wfp6 = popCount1 $ wfpbb .&. row6m
          !wfp7 = popCount1 $ wfpbb .&. row7m
          !bfp  = popCount1   bfpbb
          !bfp4 = popCount1 $ bfpbb .&. row4y
          !bfp5 = popCount1 $ bfpbb .&. row5y
          !bfp6 = popCount1 $ bfpbb .&. row6y
          !bfp7 = popCount1 $ bfpbb .&. row7y
          !dfp  = wfp  - bfp
          !dfp4 = wfp4 - bfp4
          !dfp5 = wfp5 - bfp5
          !dfp6 = wfp6 - bfp6
          !dfp7 = wfp7 - bfp7
          (!row4m, !row5m, !row6m, !row7m, !row4y, !row5y, !row6y, !row7y)
              | moving p == White = (row4, row5, row6, row7, row5, row4, row3, row2)
              | otherwise         = (row5, row4, row3, row2, row4, row5, row6, row7)
--------------------------------------
