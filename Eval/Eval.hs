{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Eval.Eval (
    initEvalState,
    posEval,
    maxStatsDepth, maxStatsIntvs,
    inLimits,
    weightNames, weightLims, parDim, weightPairs
) where

import Prelude hiding ((++), head, foldl, map, concat, filter, takeWhile, iterate, sum, minimum,
                       zip, zipWith, foldr, concatMap, length, replicate, lookup, repeat, null,
                       unzip, drop)
import Data.Array.Base (unsafeAt)
import Data.Bits hiding (popCount)
import Data.List.Stream
import Control.Monad.State.Lazy
import Data.Array.Unboxed
import Data.Ord (comparing)

import Struct.Struct
import Struct.Status
import Struct.Config
import Moves.Moves
import Moves.BitBoard
import Moves.Muster

-- These are characteristics weights
type IWeights = [Int]
type DWeights = [Double]
type Limits = [(Double, Double)]

instance CollectParams EvalParams where
    type CollectFor EvalParams = EvalParams
    npColInit = EvalParams {
                    epMaterMinor = 1,
                    epMaterRook  = 3,
                    epMaterQueen = 11,
                    epMaterScale = 1,
                    epMaterBonusScale = 4,
                    epPawnBonusScale  = 4
                }
    npColParm = collectEvalParams
    npSetParm = id

collectEvalParams :: (String, Double) -> EvalParams -> EvalParams
collectEvalParams (s, v) ep = lookApply s v ep [
        ("epMaterMinor",      setEpMaterMinor),
        ("epMaterRook",       setEpMaterRook),
        ("epMaterQueen",      setEpMaterQueen),
        ("epMaterScale",      setEpMaterScale),
        ("epMaterBonusScale", setEpMaterBonusScale),
        ("epPawnBonusScale",  setEpPawnBonusScale)
    ]
    where setEpMaterMinor      v' ep' = ep' { epMaterMinor      = round v' }
          setEpMaterRook       v' ep' = ep' { epMaterRook       = round v' }
          setEpMaterQueen      v' ep' = ep' { epMaterQueen      = round v' }
          setEpMaterScale      v' ep' = ep' { epMaterScale      = round v' }
          setEpMaterBonusScale v' ep' = ep' { epMaterBonusScale = round v' }
          setEpPawnBonusScale  v' ep' = ep' { epPawnBonusScale  = round v' }

class EvalItem a where
    evalItem    :: EvalParams -> MyPos -> a -> IWeights
    evalItemNDL :: a -> [(String, ((Double, Double), (Double, Double)))] -- Name, Defaults, Limits

-- some handy functions for eval item types:
weightName :: (a, b) -> a
weightName    = fst

weightDefault :: (a, (b, c)) -> b
weightDefault = fst . snd

weightLimits :: (a, (b, c)) -> c
weightLimits  = snd . snd

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
              EvIt KingPlace,	-- bonus when king is near some fields
              -- EvIt Castles,	-- bonus for castle rights
              EvIt LastLine,	-- malus for pieces on last line (except rooks and king)
              EvIt Mobility,	-- pieces mobility
              EvIt Center,	-- attacs of center squares
              -- EvIt DblPawns,	-- malus for doubled pawns
              EvIt PassPawns,	-- pass pawns
              EvIt RookPlc	-- rooks points for placements
            ]

parDim :: Int
parDim = sum $ map evalLen evalItems
    where evalLen (EvIt a) = length $ evalItemNDL a

weightLims :: [(Double, Double)]
weightLims = concatMap evalLims evalItems
    where evalLims (EvIt a) = map weightLimits $ evalItemNDL a

zeroFeats :: [Int]
zeroFeats = replicate parDim 0	-- theese are ints

evalItemPar :: EvalItem a => a -> (DWeights, DWeights) -> (String, Double)
                               -> Maybe (DWeights, DWeights)
evalItemPar a (dms, des) (s, v) = lookup sf (zip lu posi) >>= return . f
    where lu = map weightName $ evalItemNDL a
          posi = [0..] :: [Int]
          (sf, f) | mid `isPrefixOf` s = (restmid, \i -> (replace dms i v, des))
                  | end `isPrefixOf` s = (restend, \i -> (dms, replace des i v))
                  | otherwise          = (s, \i -> (replace dms i v, replace des i v))
          replace []       _ _ = []
          replace (_ : ds) 0 v' = v' : ds
          replace (d : ds) i v' = d : replace ds (i-1) v'
          mid = "mid."
          end = "end."
          restmid = drop (length mid) s
          restend = drop (length end) s

oneWeight :: [(AnyEvalItem, (DWeights, DWeights))] -> (String, Double)
          -> [(AnyEvalItem, (DWeights, DWeights))]
oneWeight [] _ = []
oneWeight (evp@(EvIt ei, dp) : evps) sds
    | Just ndp <- evalItemPar ei dp sds = (EvIt ei, ndp) : evps
    | otherwise = evp : oneWeight evps sds

-- Map a list of parameter assignments (name, value)
-- to two vectors of parameters (mid, end), taking defaults for missing parameters
allWeights :: [(String, Double)] -> (DWeights, DWeights)
allWeights sds = (concat ms, concat es)
    where defevps = map defp evalItems
          defp ei@(EvIt a) = (ei, unzip . map weightDefault $ evalItemNDL a)
          (ms, es) = unzip $ map snd $ foldl oneWeight defevps sds

weightNames :: [String]
weightNames = concatMap pnames evalItems
    where pnames (EvIt a) = map weightName $ evalItemNDL a

weightPairs :: [Int] -> [(String, Int)]
weightPairs = zip weightNames

------------------------------------------------------------------
-- Parameters of this module ------------
granCoarse, granCoarse2, granCoarseM, maxStatsDepth, maxStatsIntvs, shift2Cp :: Int
granCoarse    = 4	-- coarse granularity
granCoarse2   = granCoarse `div` 2
granCoarseM   = complement (granCoarse - 1)
shift2Cp      = 3	-- we have 2^shift2Cp units per centipawn
maxStatsDepth = 12	-- for error statistics of the eval function - maximum depth
maxStatsIntvs = 20	-- number of difference interval
-----------------------------------------------

initEvalState :: [(String, Double)] -> EvalState
initEvalState sds = EvalState {
        esDWeightsM = weightsM,
        esDWeightsE = weightsE,
        esIWeightsM = map round weightsM,
        esIWeightsE = map round weightsE,
        esEParams  = npSetParm (colParams sds :: CollectFor EvalParams)
    }
    where (weightsM, weightsE) = inLimits weightLims $ allWeights sds

inLimits :: Limits -> (DWeights, DWeights) -> (DWeights, DWeights)
inLimits ls (psm, pse) = (map inlim $ zip ls psm, map inlim $ zip ls pse)
    where inlim ((mi, ma), p) = max mi $ min ma p

(<*>) :: Num a => [a] -> [a] -> a
a <*> b = sum $ zipWith (*) a b
{-# SPECIALIZE (<*>) :: [Int] -> [Int] -> Int #-}

matesc :: Int
matesc = 20000 - 255	-- warning, this is also defined in Base.hs!!

posEval :: MyPos -> State EvalState (Int, [Int])
posEval !p = do
    sti <- get
    let (sce, feat) = evalDispatch p sti
        !scl = min matesc $ max (-matesc) sce
        !scc = if granCoarse > 0 then (scl + granCoarse2) .&. granCoarseM else scl
    return (scc, feat)

evalDispatch :: MyPos -> EvalState -> (Int, [Int])
evalDispatch p sti
    | pawns p == 0 = evalNoPawns p sti
    | pawns p .&. me p == 0 ||
      pawns p .&. yo p == 0 = evalSideNoPawns p sti
    | kings p .|. pawns p == occup p,
      Just r <- pawnEndGame p = r
    | otherwise    = normalEval p sti

itemEval :: EvalParams -> MyPos -> AnyEvalItem -> [Int]
itemEval ep p (EvIt a) = evalItem ep p a

normalEval :: MyPos -> EvalState -> (Int, [Int])
normalEval p !sti = (sc, feat)
    where feat = concatMap (itemEval (esEParams sti) p) evalItems
          scm  = feat <*> esIWeightsM sti
          sce  = feat <*> esIWeightsE sti
          gph  = gamePhase p
          !sc = ((scm + meMovingMid) * gph + (sce + meMovingEnd) * (256 - gph)) `unsafeShiftR` nm
          meMovingMid = 15 `shiftL` shift2Cp	-- advantage for moving in mid game
          meMovingEnd =  5 `shiftL` shift2Cp	-- advantage for moving in end game
          nm = shift2Cp + 8

gamePhase :: MyPos -> Int
gamePhase p = g
    where qs = popCount1 $ queens p
          rs = popCount1 $ rooks p
          bs = popCount1 $ bishops p
          ns = popCount1 $ knights p
          !g = qs * 39 + rs * 20 + (bs + ns) * 12	-- opening: 254, end: 0

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
    evalItem _ p _ = kingSafe p
    evalItemNDL _  = [("kingSafe", ((1, 0), (0, 20)))]

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
    evalItem _ p _ = materDiff p
    evalItemNDL _  = [("materialDiff", ((8, 8), (8, 8)))]

materDiff :: MyPos -> IWeights
materDiff p = [md]
    where !md | moving p == White =   mater p
              | otherwise         = - mater p

------ King openness ------
data KingOpen = KingOpen

instance EvalItem KingOpen where
    evalItem _ p _ = kingOpen p
    evalItemNDL _  = [ ("kingOpenOwn", ((-8, 0), (-48, 0))), ("kingOpenAdv", ((8, 0), (0, 48)))] 

-- Openness can be tought only with pawns (like we take) or all pieces
kingOpen :: MyPos -> IWeights
kingOpen p = [own, adv]
    where -- mopbishops = popCount1 $ bishops p .&. yo p
          moprooks   = popCount1 $ rooks p .&. yo p
          mopqueens  = popCount1 $ queens p .&. yo p
          mwb = popCount $ bAttacs paw msq `less` paw
          mwr = popCount $ rAttacs paw msq `less` (paw .|. lastrs)
          -- yopbishops = popCount1 $ bishops p .&. me p
          yoprooks   = popCount1 $ rooks p .&. me p
          yopqueens  = popCount1 $ queens p .&. me p
          ywb = popCount $ bAttacs paw ysq `less` paw
          ywr = popCount $ rAttacs paw ysq `less` (paw .|. lastrs)
          paw = pawns p
          msq = kingSquare (kings p) $ me p
          ysq = kingSquare (kings p) $ yo p
          comb !oR !oQ !wb !wr = let !v = oR * wr + 2 * oQ * (wb + wr) in v
          !own = comb moprooks mopqueens mwb mwr
          !adv = comb yoprooks yopqueens ywb ywr
          lastrs = 0xFF000000000000FF	-- approx: take out last row which cant be covered by pawns

------ King placement ------
data KingPlace = KingPlace

instance EvalItem KingPlace where
    evalItem ep p _  = kingPlace ep p
    evalItemNDL _ = [
                      ("kingPlaceCent", ((3, 7), (0, 400))),
                      ("kingPlacePwns", ((4, 0), (0, 400)))
                    ]


-- Depending on which pieces are on the board we have some preferences
-- where the king should be placed. For example, in the opening and middle game it should
-- be in some corner, in endgame it should be near some (passed) pawn(s)
kingPlace :: EvalParams -> MyPos -> IWeights
kingPlace ep p = [ kcd, kpd ]
    where !kcd = mpl - ypl
          !kpd = 0	-- mpi - ypi
          !mks = kingSquare (kings p) $ me p
          !yks = kingSquare (kings p) $ yo p
          !mkm = materFun yminor yrooks yqueens
          !ykm = materFun mminor mrooks mqueens
          !mpl = kingMaterBonus mpawns mro mkm mks `unsafeShiftR` epMaterBonusScale ep
          !ypl = kingMaterBonus ypawns yro ykm yks `unsafeShiftR` epMaterBonusScale ep
          {--
          !mpi | passed p /= 0            = kingPawnsBonus c mks (passed p) mpassed ypassed
               | mkm <= 0 && pawns p /= 0 = kingPawnsBonus c mks (pawns  p) mpawns  ypawns
               | otherwise                = 0
          !ypi | passed p /= 0            = kingPawnsBonus c yks (passed p) mpassed ypassed
               | ykm <= 0 && pawns p /= 0 = kingPawnsBonus c yks (pawns  p) mpawns  ypawns
               | otherwise                = 0
          --}
          !mro     = rooks p .&. me p
          !mrooks  = popCount1 mro
          !mqueens = popCount1 $ queens p .&. me p
          !mminor  = popCount1 $ (bishops p .|. knights p) .&. me p
          !yro     = rooks p .&. yo p
          !yrooks  = popCount1 yro
          !yqueens = popCount1 $ queens p .&. yo p
          !yminor  = popCount1 $ (bishops p .|. knights p) .&. yo p
          !mpawns  = pawns p .&. me p
          !ypawns  = pawns p .&. yo p
          -- !mpassed = passed p .&. me p
          -- !ypassed = passed p .&. yo p
          -- !c = moving p
          materFun m r q = (m * epMaterMinor ep + r * epMaterRook ep + q * epMaterQueen ep)
                               `unsafeShiftR` epMaterScale ep

promoW, promoB :: Square -> Square
promoW s = 56 + (s .&. 7)
promoB s =       s .&. 7

{--
-- We give bonus also for pawn promotion squares, if the pawn is near enough to promote
-- Give as parameter bitboards for all pawns, white pawns and black pawns for performance
kingPawnsBonus :: Color -> Square -> BBoard -> BBoard -> BBoard -> Int
kingPawnsBonus White !ksq !alp !mbb !ybb = kingPawnsBonus' ksq alp wHalf bHalf
    where !wHalf = 0x00000000FFFFFFFF .&. ybb
          !bHalf = 0xFFFFFFFF00000000 .&. mbb
kingPawnsBonus Black !ksq !alp !mbb !ybb = kingPawnsBonus' ksq alp wHalf bHalf
    where !wHalf = 0x00000000FFFFFFFF .&. mbb
          !bHalf = 0xFFFFFFFF00000000 .&. ybb

promoFieldDistIncr :: Int -> Int
promoFieldDistIncr = \d -> d + 1

kingPawnsBonus' :: Square -> BBoard -> BBoard -> BBoard -> Int
kingPawnsBonus' !ksq !alp !wHalf !bHalf = bonus
    where !bpsqs = sum $ map (proxyBonus . squareDistance ksq) $ bbToSquares alp
          !bqsqs = sum $ map (proxyBonus . promoFieldDistIncr . squareDistance ksq)
                       $ map promoW (bbToSquares bHalf) ++ map promoB (bbToSquares wHalf)
          !bonus = (bpsqs + bqsqs) `unsafeShiftR` pawnBonusScale
--}

-- This is a bonus for the king beeing near one corner
-- It's bigger when the enemy has more material (only pieces)
-- and when that corner has a pawn shelter
kingMaterBonus :: BBoard -> BBoard -> Int -> Square -> Int
kingMaterBonus !myp !myrooks !mat !ksq = bonus
    where !bonus = matFactor mat * prx
          !prx = prxWA + prxWH + prxBA + prxBH
          !prxWA = (unsafeShiftL (opawns shWA2) 1 + opawns shWA3 - roMWA) * (prxBo wa + prxBo wb)
          !prxWH = (unsafeShiftL (opawns shWH2) 1 + opawns shWH3 - roMWH) * (prxBo wg + prxBo wh)
          !prxBA = (unsafeShiftL (opawns shBA7) 1 + opawns shBA6 - roMBA) * (prxBo ba + prxBo bb)
          !prxBH = (unsafeShiftL (opawns shBH7) 1 + opawns shBH6 - roMBH) * (prxBo bg + prxBo bh)
          !roMWA = flip unsafeShiftL 2 $ popCount1 $ myrooks .&. roWA
          !roMWH = flip unsafeShiftL 2 $ popCount1 $ myrooks .&. roWH
          !roMBA = flip unsafeShiftL 2 $ popCount1 $ myrooks .&. roBA
          !roMBH = flip unsafeShiftL 2 $ popCount1 $ myrooks .&. roBH
          opawns = popCount1 . (.&. myp)
          prxBo  = proxyBonus . squareDistance ksq
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
          roWA = row1 .&. (fileA .|. fileB .|. fileC)
          roWH = row1 .&. (fileG .|. fileH)
          roBA = row8 .&. (fileA .|. fileB .|. fileC)
          roBH = row8 .&. (fileG .|. fileH)
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

matKCArr :: UArray Int Int   -- 0              5             10
matKCArr = listArray (0, 63) $ [0, 0, 0, 1, 1, 2, 3, 4, 5, 7, 9, 10, 11, 12] ++ repeat 12

proxyBonus :: Int -> Int
proxyBonus = unsafeAt proxyBonusArr

{--
proxyLineArr :: UArray Int Int -- 7  6  5  4  3  2   1   0   1   2   3  4  5  6  7
proxyLineArr = listArray (-7, 7) [0, 1, 2, 3, 5, 10, 25, 75, 25, 10, 5, 3, 2, 1, 0]

proxyLine :: Int -> Square -> Int
proxyLine line sq = proxyBonusArr `unsafeAt` (unsafeShiftR sq 3 - line)
--}

------ Rookm placement points ------
data RookPlc = RookPlc

instance EvalItem RookPlc where
    evalItem _ p _ = evalRookPlc p
    evalItemNDL _  = [ ("rookHOpen", ((206, 254), (0, 500))),
                       ("rookOpen",  ((275, 247), (0, 800))) ]
                  --   ("rook7th",   ((400, 500), (0, 900))),
                  --   ("rookBhnd",  ((100, 800), (0, 900))) ]

evalRookPlc :: MyPos -> [Int]
evalRookPlc p = [ ho, op ]
    where !mRs = rooks p .&. me p
          !mPs = pawns p .&. me p
          (mho, mop) = foldr (perRook (pawns p) mPs) (0, 0) $ bbToSquares mRs
          !yRs = rooks p .&. yo p
          !yPs = pawns p .&. yo p
          (yho, yop) = foldr (perRook (pawns p) yPs) (0, 0) $ bbToSquares yRs
          !ho = mho - yho
          !op = mop - yop

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
    evalItem _ p _ = mobDiff p
    evalItemNDL _  = [ ("mobilityKnight", ((61, 74), (50, 120))),
                       ("mobilityBishop", ((60, 73), (50, 120))),
                       ("mobilityRook",   ((28, 25), ( 0, 100))),
                       ("mobilityQueen",  (( 0,  7), (-5,  50))) ]

-- Here we do not calculate pawn mobility (which, calculated as attacs, is useless)
mobDiff :: MyPos -> IWeights
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
    evalItem _ p _ = centerDiff p
    evalItemNDL _  = [("centerAttacs", ((72, 72), (0, 100)))]

-- This function is already optimised
centerDiff :: MyPos -> IWeights
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
--enPrise :: MyPos -> Color -> IWeights
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
--castles :: MyPos -> Color -> IWeights
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
    evalItem _ p _ = lastline p
    evalItemNDL _  = [("lastLinePenalty", ((24, 0), (0, 100)))]

-- This function is already optimised
lastline :: MyPos -> IWeights
lastline p = [cdiff]
    where !whl = popCount1 $ (me p `less` rok) .&. lali
          !bll = popCount1 $ (yo p `less` rok) .&. lali
          !cdiff = bll - whl
          !rok = rooks p .|. kings p
          lali = 0xFF000000000000FF	-- approximation!

------ Redundance: bishop pair and rook redundance ------
data Redundance = Redundance

instance EvalItem Redundance where
    evalItem _ p _ = evalRedundance p
    evalItemNDL _  = [("bishopPair",       ((320,  320), ( 100, 400))),
                      ("redundanceRook",   ((  0, -104), (-150,   0))) ]

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
    evalItem _ p _ = evalNRCorrection p
    evalItemNDL _  = [("nrCorrection", ((0, 0), (0, 8)))]

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
    evalItem _ p _ = evalRookPawn p
    evalItemNDL _  = [("rookPawn", ((-64, -64), (-120, 0))) ]

-- This function is already optimised
evalRookPawn :: MyPos -> [Int]
evalRookPawn p = [rps]
    where !wrp = popCount1 $ pawns p .&. me p .&. rookFiles
          !brp = popCount1 $ pawns p .&. yo p .&. rookFiles
          !rps = wrp - brp

------ Pass pawns ------
data PassPawns = PassPawns

instance EvalItem PassPawns where
    evalItem _ p _ = passPawns p
    evalItemNDL _  = [("passPawnBonus", ((  50,   77), (   0,  200))),
                      ("passPawn4",     (( 326,  324), ( 200,  520))),
                      ("passPawn5",     (( 488,  476), ( 250,  740))),
                      ("passPawn6",     (( 856,  849), ( 500, 1200))),
                      ("passPawn7",     ((1489, 1491), ( 900, 2300)))
                     ]
 
passPawns :: MyPos -> IWeights
passPawns p = [dfp, dfp4, dfp5, dfp6, dfp7]
    where !mfpbb = passed p .&. me p
          !yfpbb = passed p .&. yo p
          !mfp  = popCount1   mfpbb
          !mfp4 = popCount1 $ mfpbb .&. row4m
          !mfp5 = popCount1 $ mfpbb .&. row5m
          !mfp6 = popCount1 $ mfpbb .&. row6m
          !mfp7 = popCount1 $ mfpbb .&. row7m
          !yfp  = popCount1   yfpbb
          !yfp4 = popCount1 $ yfpbb .&. row4y
          !yfp5 = popCount1 $ yfpbb .&. row5y
          !yfp6 = popCount1 $ yfpbb .&. row6y
          !yfp7 = popCount1 $ yfpbb .&. row7y
          !dfp  = mfp  - yfp
          !dfp4 = mfp4 - yfp4
          !dfp5 = mfp5 - yfp5
          !dfp6 = mfp6 - yfp6
          !dfp7 = mfp7 - yfp7
          (!row4m, !row5m, !row6m, !row7m, !row4y, !row5y, !row6y, !row7y)
              | moving p == White = (row4, row5, row6, row7, row5, row4, row3, row2)
              | otherwise         = (row5, row4, row3, row2, row4, row5, row6, row7)

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
pawnEndGame :: MyPos -> Maybe (Int, [Int])
pawnEndGame p
    -- | null mescds && null yescds             = Nothing
    | not (null mescds) && not (null yescds) = Just (dpr, [dpr])
    | not (null mescds)                      = Just (myrace, [myrace])
    |                      not (null yescds) = Just (yorace, [yorace])
    | otherwise                              = Nothing
    -- | -- here we will consider what is with 2 passed pawns which are far enough from each other
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
          -- yoPawnCount = popCount1 $ pawns p .&. yo p
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
          !pc0 = popCount1 $ bb  .&. band
          !bb1 = bb  `unsafeShiftL` 16
          !pc1 = popCount1 $ bb1 .&. band
          !bb2 = bb1 `unsafeShiftL` 16
          !pc2 = popCount1 $ bb2 .&. band
          !pc  = (pc0 `unsafeShiftL` 2) + (pc1 `unsafeShiftL` 1) + pc2
          band = 0x00FFFF0000000000	-- row 6 and 7

simplePvBlack :: BBoard -> Int
simplePvBlack !bb = pv
    where !pv = 100 * pc
          !pc0 = popCount1 $ bb  .&. band
          !bb1 = bb  `unsafeShiftR` 16
          !pc1 = popCount1 $ bb1 .&. band
          !bb2 = bb1 `unsafeShiftR` 16
          !pc2 = popCount1 $ bb2 .&. band
          !pc  = (pc0 `unsafeShiftL` 2) + (pc1 `unsafeShiftL` 1) + pc2
          band = 0x0000000000FFFF00	-- row 2 and 3

halfPawnMax :: Int -> Int -> Int
halfPawnMax mx d
    | steps > mx = 100 * mx
    | otherwise  = 100 * steps
    where steps = (d + 1) `unsafeShiftR` 1
--}
--------------------------------------
