{-# LANGUAGE BangPatterns, PatternGuards, ExistentialQuantification #-}
module Eval.Eval (
    initEvalState,
    posEval,
    maxStatsDepth, maxStatsIntvs,
    inLimits,
    paramNames, parLims, parDim
) where

import Data.Array.Base (unsafeAt)
import Data.Bits hiding (popCount)
import Data.List
import Control.Monad.State.Lazy
import Data.Array.Unboxed

import Struct.Struct
import Struct.Status
import Moves.Moves
import Moves.BitBoard
import Moves.Muster
-- import Eval.Gradient
import Eval.BasicEval

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
    evalItem    :: MyPos -> Color -> a -> IParams
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
              EvIt KingCenter,	-- malus for king on center files
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

zeroParam :: DParams
zeroParam = replicate parDim 0	-- theese are doubles

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

------------------------------------------------------------------
-- Parameters of this module ------------
granCoarse, granCoarse2, granCoarseM, maxStatsDepth, maxStatsIntvs :: Int
granCoarse    = 4	-- coarse granularity
granCoarse2   = granCoarse `div` 2
granCoarseM   = complement (granCoarse - 1)
shift2Cp      = 3	-- we have 2^shift2Cp units per centipawn
maxStatsDepth = 12	-- for error statistics of the eval function - maximum depth
maxStatsIntvs = 20	-- number of difference interval
-- statsIntv     = 25	-- difference interval length

subOptimal :: Double
subOptimal    = 2	-- optimal step is so many times smaller

samplesPerChange :: Int
samplesPerChange = 10	-- number of samples before a parameter change occurs
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

posEval :: MyPos -> Color -> State EvalState (Int, [Int])
posEval !p !c = do
    sti <- get
    let (sc''', feat) = evalDispatch p c sti
        !sc'  = if sc''' > matesc then matesc else if sc''' < -matesc then -matesc else sc'''
        !sc'' = if granCoarse > 0 then (sc' + granCoarse2) .&. granCoarseM else sc'
        !sc = if c == White then sc'' else -sc''
    return $! sc `seq` (sc, feat)

evalDispatch :: MyPos -> Color -> EvalState -> (Int, [Int])
evalDispatch p c sti
    | pawns p == 0 = evalNoPawns p c sti
    | pawns p .&. white p == 0 ||
      pawns p .&. black p == 0 = evalSideNoPawns p c sti
    | otherwise    = normalEval p c sti

itemEval :: MyPos -> Color -> AnyEvalItem -> [Int]
itemEval p c (EvIt a) = evalItem p c a

normalEval :: MyPos -> Color -> EvalState -> (Int, [Int])
normalEval p c sti = (sc, feat)
    where !feat = concatMap (itemEval p c) evalItems
          !sc   = feat <*> esIParams sti `shiftR` shift2Cp

evalSideNoPawns :: MyPos -> Color -> EvalState -> (Int, [Int])
evalSideNoPawns p c sti
    | npwin && insufficient = (0, zeroFeats)
    | otherwise             = (nsc, feats)
    where (nsc, feats) = normalEval p c sti
          npside = if pawns p .&. white p == 0 then White else Black
          npwin = npside == White && nsc > 0 || npside == Black && nsc < 0
          insufficient = majorcnt == 0 && (minorcnt == 1 || minorcnt == 2 && bishopcnt == 0)
          col = if npside == White then white p else black p
          bishopcnt = popCount1 $ bishops p .&. col
          minorcnt  = popCount1 $ (bishops p .|. knights p) .&. col
          majorcnt  = popCount1 $ (queens p .|. rooks p) .&. col

-- These evaluation function distiguishes between some known finals with no pawns
evalNoPawns :: MyPos -> Color -> EvalState -> (Int, [Int])
evalNoPawns p c sti = (sc, zeroFeats)
    where !sc | onlykings   = 0
              | kmk || knnk = 0		-- one minor or two knights
              | kbbk        = mateKBBK p kaloneb	-- 2 bishops
              | kbnk        = mateKBNK p kaloneb	-- bishop + knight
              | kMxk        = mateKMajxK p kaloneb	-- simple mate with at least one major
              -- | kqkx        = mateQRest p kaloneb	-- queen against minor or rook
              | otherwise   = fst $ normalEval p c sti
          kalonew = white p `less` kings p == 0
          kaloneb = black p `less` kings p == 0
          onlykings = kalonew && kaloneb
          kmk  = (kalonew || kaloneb) && minorcnt == 1 && majorcnt == 0
          knnk = (kalonew || kaloneb) && minorcnt == 2 && majorcnt == 0 && bishops p == 0
          kbbk = (kalonew || kaloneb) && minorcnt == 2 && majorcnt == 0 && knights p == 0
          kbnk = (kalonew || kaloneb) && minorcnt == 2 && not (knnk || kbbk)
          kMxk = (kalonew || kaloneb) && majorcnt > 0
          minor   = bishops p .|. knights p
          minorcnt = popCount1 minor
          major    = queens p .|. rooks p
          majorcnt = popCount1 major

winBonus :: Int
winBonus = 200	-- when it's known win

mateKBBK :: MyPos -> Bool -> Int
mateKBBK p wwin = mater p + if wwin then sc else -sc
    where kadv = if wwin then kb else kw
          kw = kingSquare (kings p) (white p)
          kb = kingSquare (kings p) (black p)
          distk = squareDistance kw kb
          distc = centerDistance kadv
          sc = winBonus + distc*distc - distk*distk

mateKBNK :: MyPos -> Bool -> Int
mateKBNK p wwin = mater p + if wwin then sc else -sc
    where kadv = if wwin then kb else kw
          kw = kingSquare (kings p) (white p)
          kb = kingSquare (kings p) (black p)
          distk = squareDistance kw kb
          distc = bnMateDistance wbish kadv
          wbish = bishops p .&. lightSquares /= 0
          sc = winBonus + distc*distc - distk*distk

mateKMajxK :: MyPos -> Bool -> Int
mateKMajxK p wwin = mater p + if wwin then sc else -sc
    where kadv = if wwin then kb else kw
          kw = kingSquare (kings p) (white p)
          kb = kingSquare (kings p) (black p)
          distk = squareDistance kw kb
          distc = centerDistance kadv
          sc = winBonus + distc*distc - distk*distk

-- This square distance should be pre calculated
squareDistance :: Square -> Square -> Int
squareDistance f t = max (abs (fr - tr)) (abs (fc - tc))
    where (fr, fc) = f `divMod` 8
          (tr, tc) = t `divMod` 8

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
zoneAttacs p zone = (wh, bl)
    where wh = popCount $ zone .&. whAttacs p
          bl = popCount $ zone .&. blAttacs p

----------------------------------------------------------------------------
-- Here we have the implementation of the evaluation items
-- They do not return a score, but a vector of fulfillments of some criteria
-- With version 0.55 we compute everything from white point of view
-- and only at the end we negate the score if black side is asked
----------------------------------------------------------------------------
------ King Safety ------
data KingSafe = KingSafe

instance EvalItem KingSafe where
    evalItem p c _ = kingSafe p c
    evalItemNDL _ = [("kingSafe", (1, (0, 20)))]

-- Rewrite of king safety taking into account number and quality
-- of pieces attacking king neighbour squares
kingSafe :: MyPos -> Color -> [Int]
kingSafe p _ = [ksafe]
    where !ksafe = wattacs - battacs
          freew = popCount1 $ whKAttacs p .&. blAttacs p `less` white p
          freeb = popCount1 $ blKAttacs p .&. whAttacs p `less` black p
          flag k a = if k .&. a /= 0 then 1 else 0
          qual k a = popCount1 $ k .&. a
          flagBlack = flag (whKAttacs p)
          flagWhite = flag (blKAttacs p)
          qualBlack = qual (whKAttacs p)
          qualWhite = qual (blKAttacs p)
          ($:) = flip ($)
          attsw = map (p $:) [ whPAttacs, whNAttacs, whBAttacs, whRAttacs, whQAttacs, whKAttacs ]
          fw = sum $ map flagWhite attsw
          cw = sum $ zipWith (*) qualWeights $ map qualWhite attsw
          ixw = max 0 $ min 63 $ fw * cw - freeb
          attsb = map (p $:) [ blPAttacs, blNAttacs, blBAttacs, blRAttacs, blQAttacs, blKAttacs ]
          fb = sum $ map flagBlack attsb
          cb = sum $ zipWith (*) qualWeights $ map qualBlack attsb
          ixb = max 0 $ min 63 $ fb * cb - freew
          wattacs = attCoef `unsafeAt` ixw
          battacs = attCoef `unsafeAt` ixb
          qualWeights = [1, 1, 1, 2, 3, 1]

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
    evalItem p c _ = materDiff p c
    evalItemNDL _ = [("materialDiff", (8, (8, 8)))]

materDiff :: MyPos -> Color -> IParams
materDiff p _ = [mater p]

------ King openness ------
data KingOpen = KingOpen

instance EvalItem KingOpen where
    evalItem p c _ = kingOpen p c
    evalItemNDL _ = [ ("kingOpenOwn", (-20, (-48, 1))), ("kingOpenAdv", (20, (0, 32)))] 

-- Openness can be tought only with pawns (like we take) or all pieces
kingOpen :: MyPos -> Color -> IParams
kingOpen p _ = own `seq` adv `seq` [own, adv]
    where mopbishops = popCount1 $ bishops p .&. black p
          moprooks   = popCount1 $ rooks p .&. black p
          mopqueens  = popCount1 $ queens p .&. black p
          mwb = popCount $ bAttacs paw msq `less` paw
          mwr = popCount $ rAttacs paw msq `less` paw
          yopbishops = popCount1 $ bishops p .&. white p
          yoprooks   = popCount1 $ rooks p .&. white p
          yopqueens  = popCount1 $ queens p .&. white p
          ywb = popCount $ bAttacs paw ysq `less` paw
          ywr = popCount $ rAttacs paw ysq `less` paw
          paw = pawns p
          msq = kingSquare (kings p) $ white p
          ysq = kingSquare (kings p) $ black p
          comb !oB !oR !oQ wb wr =
                if oB /= 0 then oB * wb else 0
              + if oR /= 0 then oR * wr else 0
              + if oQ /= 0 then oQ * (wb + wr) else 0
          own = comb mopbishops moprooks mopqueens mwb mwr
          adv = comb yopbishops yoprooks yopqueens ywb ywr

------ King on a center file ------
data KingCenter = KingCenter

instance EvalItem KingCenter where
    evalItem p c _ = kingCenter p c
    evalItemNDL _  = [ ("kingCenter", (-120, (-200, 0))) ]

kingCenter :: MyPos -> Color -> IParams
kingCenter p _ = [ kcd ]
    where kcenter = fileC .|. fileD .|. fileE .|. fileF
          !wkc = if kings p .&. white p .&. kcenter /= 0 then brooks + 2 * bqueens - 1 else 0
          !bkc = if kings p .&. black p .&. kcenter /= 0 then wrooks + 2 * wqueens - 1 else 0
          !kcd = wkc - bkc
          wrooks  = popCount1 $ rooks p .&. white p
          wqueens = popCount1 $ queens p .&. white p
          brooks  = popCount1 $ rooks p .&. black p
          bqueens = popCount1 $ queens p .&. black p

------ Mobility ------
data Mobility = Mobility	-- "safe" moves

instance EvalItem Mobility where
    evalItem p c _ = mobDiff p c
    evalItemNDL _ = [ ("mobilityKnight", (72, (60, 100))),
                      ("mobilityBishop", (72, (60, 100))),
                      ("mobilityRook", (48, (40, 100))),
                      ("mobilityQueen", (3, (0, 50))) ]

-- Here we do not calculate pawn mobility (which, calculated as attacs, is useless)
mobDiff :: MyPos -> Color -> IParams
mobDiff p _ = [n, b, r, q]
    where !whN = popCount1 $ whNAttacs p `less` (white p .|. blPAttacs p)
          !whB = popCount1 $ whBAttacs p `less` (white p .|. blPAttacs p)
          !whR = popCount1 $ whRAttacs p `less` (white p .|. blA1)
          !whQ = popCount1 $ whQAttacs p `less` (white p .|. blA2)
          !blA1 = blPAttacs p .|. blNAttacs p .|. blBAttacs p
          !blA2 = blA1 .|. blRAttacs p
          !blN = popCount1 $ blNAttacs p `less` (black p .|. whPAttacs p)
          !blB = popCount1 $ blBAttacs p `less` (black p .|. whPAttacs p)
          !blR = popCount1 $ blRAttacs p `less` (black p .|. whA1)
          !blQ = popCount1 $ blQAttacs p `less` (black p .|. whA2)
          !whA1 = whPAttacs p .|. whNAttacs p .|. whBAttacs p
          !whA2 = whA1 .|. whRAttacs p
          !n = whN - blN
          !b = whB - blB
          !r = whR - blR
          !q = whQ - blQ

------ Center control ------
data Center = Center

instance EvalItem Center where
    evalItem p c _ = centerDiff p c
    evalItemNDL _ = [("centerAttacs", (72, (50, 100)))]

centerDiff :: MyPos -> Color -> IParams
centerDiff p _ = [wb]
    where (w, b) = zoneAttacs p center
          !wb = w - b
          -- center = 0x0000001818000000
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
    evalItem p c _ = lastline p c
    evalItemNDL _ = [("lastLinePenalty", (8, (0, 24)))]

lastline :: MyPos -> Color -> IParams
lastline p _ = [cdiff]
    where !whl = popCount1 $ (white p `less` (rooks p .|. kings p)) .&. 0xFF
          !bll = popCount1 $ (black p `less` (rooks p .|. kings p)) .&. 0xFF00000000000000
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
    evalItem p c _ = evalRedundance p c
    evalItemNDL _ = [("bishopPair",       (320,  (100, 400))),
                     ("redundanceRook",   (-104,  (-150, 0))) ]

evalRedundance :: MyPos -> Color -> [Int]
evalRedundance p _ = [bp, rr]
    where !wbl = bishops p .&. white p .&. lightSquares
          !wbd = bishops p .&. white p .&. darkSquares
          !bbl = bishops p .&. black p .&. lightSquares
          !bbd = bishops p .&. black p .&. darkSquares
          !bpw = if wbl /= 0 && wbd /= 0 then 1 else 0
          !bpb = if bbl /= 0 && bbd /= 0 then 1 else 0
          !bp  = bpw - bpb
          !wro = rooks p .&. white p
          !bro = rooks p .&. black p
          !wrr = if wro > 1 then 1 else 0
          !brr = if bro > 1 then 1 else 0
          !rr  = wrr - brr

------ Knight & Rook correction according to own pawns ------
data NRCorrection = NRCorrection

instance EvalItem NRCorrection where
    evalItem p _ _ = evalNRCorrection p
    evalItemNDL _  = [("nrCorrection", (0, (0, 8)))]

evalNRCorrection :: MyPos -> [Int]
evalNRCorrection p = [md]
    where !wpc = popCount1 (pawns p .&. white p) - 5
          !bpc = popCount1 (pawns p .&. black p) - 5
          !wnp = popCount1 (knights p .&. white p) * wpc * 6	-- 1/16 for each pawn over 5
          !bnp = popCount1 (knights p .&. black p) * bpc * 6	-- 1/16 for each pawn over 5
          !wrp = - popCount1 (rooks p .&. white p) * wpc * 12	-- 1/8 for each pawn under 5
          !brp = - popCount1 (rooks p .&. black p) * bpc * 12	-- 1/8 for each pawn under 5
          !md = wnp + wrp - bnp - brp

------ Rook pawn weakness ------
data RookPawn = RookPawn

instance EvalItem RookPawn where
    evalItem p c _ = evalRookPawn p c
    evalItemNDL _ = [("rookPawn", (-64, (-120, 0))) ]

evalRookPawn :: MyPos -> Color -> [Int]
evalRookPawn p _ = [rps]
    where !wrp = popCount1 $ pawns p .&. white p .&. rookFiles
          !brp = popCount1 $ pawns p .&. black p .&. rookFiles
          !rps = wrp - brp

------ Pass pawns ------
data PassPawns = PassPawns

whitePassPBBs, blackPassPBBs :: UArray Square BBoard
whitePassPBBs = array (0, 63) [(sq, wPassPBB sq) | sq <- [0 .. 63]]
blackPassPBBs = array (0, 63) [(sq, bPassPBB sq) | sq <- [0 .. 63]]

wPassPBB :: Square -> BBoard
wPassPBB sq = foldl' (.|.) 0 $ takeWhile (/= 0) $ iterate (`shiftL` 8) bsqs
    where bsq = bit sq
          bsq8 = bsq `shiftL` 8
          bsq7 = if bsq .&. fileA /= 0 then 0 else bsq `shiftL` 7
          bsq9 = if bsq .&. fileH /= 0 then 0 else bsq `shiftL` 9
          bsqs = bsq7 .|. bsq8 .|. bsq9

bPassPBB :: Square -> BBoard
bPassPBB sq = foldl' (.|.) 0 $ takeWhile (/= 0) $ iterate (`shiftR` 8) bsqs
    where bsq = bit sq
          bsq8 = bsq `shiftR` 8
          bsq7 = if bsq .&. fileH /= 0 then 0 else bsq `shiftR` 7
          bsq9 = if bsq .&. fileA /= 0 then 0 else bsq `shiftR` 9
          bsqs = bsq7 .|. bsq8 .|. bsq9

instance EvalItem PassPawns where
    evalItem p c _ = passPawns p c
    evalItemNDL _  = [("passPawnBonus", (104, (  0, 160))),
                      ("passPawn4",     (424, (400, 480))),
                      ("passPawn5",     (520, (520, 640))),
                      ("passPawn6",     (1132, (1100, 1200))),
                      ("passPawn7",     (1920, (1600, 2300))) ]

passPawns :: MyPos -> Color -> IParams
passPawns p _ = [dfp, dfp4, dfp5, dfp6, dfp7]
    where !wfpbb = foldl' (.|.) 0 $ map bit $ filter wpIsPass $ bbToSquares wpawns
          !bfpbb = foldl' (.|.) 0 $ map bit $ filter bpIsPass $ bbToSquares bpawns
          !wfp = popCount1 wfpbb
          !wfp4 = popCount1 $ wfpbb .&. row4
          !wfp5 = popCount1 $ wfpbb .&. row5
          !wfp6 = popCount1 $ wfpbb .&. row6
          !wfp7 = popCount1 $ wfpbb .&. row7
          !bfp = popCount1 bfpbb
          !bfp4 = popCount1 $ bfpbb .&. row5
          !bfp5 = popCount1 $ bfpbb .&. row4
          !bfp6 = popCount1 $ bfpbb .&. row3
          !bfp7 = popCount1 $ bfpbb .&. row2
          !wpawns = pawns p .&. white p
          !bpawns = pawns p .&. black p
          !dfp = wfp - bfp
          !dfp4 = wfp4 - bfp4
          !dfp5 = wfp5 - bfp5
          !dfp6 = wfp6 - bfp6
          !dfp7 = wfp7 - bfp7
          wpIsPass sq = (whitePassPBBs!sq) .&. bpawns == 0
          bpIsPass sq = (blackPassPBBs!sq) .&. wpawns == 0
--------------------------------------
