module Search.AlbetaTypes (
    DoResult(..),
    Comm(..),
    AbortPolicy(..),
    ABControl(..),
    SStats(..),
    ssts0, formatStats, addStats,
    countRFut, formatRFutH,
    nulDebug
) where

import Data.Int
import qualified Data.IntMap.Strict as IM
import Struct.Struct

data AbortPolicy
    = AbortByTime {
          firstMoveMs :: Int,
          laterMovesMs :: Int
      }
    | AbortByNodes {
          nodeBudget :: Int
      }
    | NoAbort
    deriving Show

data ABControl = ABC {
        maxdepth  :: Int,
        lastpv    :: [Move],
        lastscore :: Maybe Int,
        rootmvs   :: [Move],
        window    :: Int,
        intuning  :: Bool,
        abortPolicy :: AbortPolicy
    } deriving Show

data DoResult = Exten !Int !Bool !Bool	-- return mit extension, iscapt & canlmr
              | Final			-- this is always draw
              | Illegal			-- illegal move

data Comm = LogMes String
          | BestMv Int Int Int Int64 [Move]
          | CurrMv Move Int
          | InfoStr String

-- Data type for search statistics:
data SStats = SStats {
        sNodes, sNodesQS,
        sRetr, sRFnd, sRFal, sRSuc, sBeta, sBM1, sBM2, sBM3, sBM4p,
        sRedu, sReMi, sReBe, sReSe, sReNo,
        sRFut, sRFutW :: !Int64,	-- shadow reverse futility pruning: decisions, wrong ones (failed low)
        sRFutH :: !(IM.IntMap Int64),	-- decisions by wrong, depth, (static - beta) / depth (countRFut)
        sRFutL :: [String]	-- the wrong decisions (countRFut), newest first
    } deriving Show

ssts0 :: SStats
ssts0 = SStats { sNodes = 0, sNodesQS = 0,
                 sRetr = 0, sRFnd = 0, sRFal = 0, sRSuc = 0,
                 sBeta = 0, sBM1 = 0, sBM2 = 0, sBM3 = 0, sBM4p = 0,
                 sRedu = 0, sReMi = 0, sReBe = 0, sReSe = 0, sReNo = 0,
                 sRFut = 0, sRFutW = 0, sRFutH = IM.empty, sRFutL = [] }

addStats :: SStats -> SStats -> SStats
addStats a b = SStats {
    sNodes = sNodes a + sNodes b, sNodesQS = sNodesQS a + sNodesQS b,
    sRetr = sRetr a + sRetr b, sRFnd = sRFnd a + sRFnd b, sRFal = sRFal a + sRFal b,
    sRSuc = sRSuc a + sRSuc b,
    sBeta = sBeta a + sBeta b, sBM1 = sBM1 a + sBM1 b, sBM2 = sBM2 a + sBM2 b,
    sBM3  = sBM3 a + sBM3 b, sBM4p = sBM4p a + sBM4p b,
    sRedu = sRedu a + sRedu b, sReMi = sReMi a + sReMi b, sReBe = sReBe a + sReBe b,
    sReSe = sReSe a + sReSe b, sReNo = sReNo a + sReNo b,
    sRFut = sRFut a + sRFut b, sRFutW = sRFutW a + sRFutW b,
    sRFutH = IM.unionWith (+) (sRFutH a) (sRFutH b),
    sRFutL = sRFutL b ++ sRFutL a
    }

-- Count a shadow reverse futility decision: wrong or not, depth, excess = static score - beta, and a
-- description line kept when wrong; histogram key: wrong * 100000 + depth * 1000 + excess per depth
-- (cp, 0 .. 999)
countRFut :: Bool -> Int -> Int -> String -> SStats -> SStats
countRFut wrong d excess line s
    | wrong     = s' { sRFutW = sRFutW s + 1, sRFutL = line : sRFutL s }
    | otherwise = s'
    where s' = s { sRFut = sRFut s + 1, sRFutH = IM.insertWith (+) key 1 (sRFutH s) }
          key = (if wrong then 100000 else 0) + d * 1000 + min 999 (max 0 (excess `div` d))

-- Lines "RFPH|depth|excess per depth|correct|wrong" of the shadow reverse futility histogram
formatRFutH :: SStats -> [String]
formatRFutH s = [ "RFPH|" ++ show d ++ "|" ++ show q ++ "|" ++ show c ++ "|" ++ show w
                | k <- IM.keys $ IM.mapKeys (`mod` 100000) h,
                  let (d, q) = k `divMod` 1000
                      c = IM.findWithDefault 0 k h
                      w = IM.findWithDefault 0 (k + 100000) h ]
    where h = sRFutH s

nulDebug :: Bool
nulDebug = False

formatStats :: SStats -> [String]
formatStats sst = [
       "Nodes: " ++ show (sNodes sst) ++ ", in QS: " ++ show (sNodesQS sst) ++ " (QS%: "
            ++ show (if sNodes sst == 0 then 0 else 100 * sNodesQS sst `div` sNodes sst) ++ ")",
       "Retrieve: " ++ show (sRetr sst) ++ ", not found: " ++ show (sRFal sst)
            ++ ", found usable: " ++ show (sRFnd sst)
            ++ ", found not usable: " ++ show (sRetr sst - (sRFal sst + sRFnd sst))
            ++ " (usable%: " ++ show (if sRetr sst == 0 then 0 else 100 * sRFnd sst `div` sRetr sst)
            ++ ", success: " ++ show (sRSuc sst) ++ ")",
       "Beta cuts: " ++ show (sBeta sst) ++ ", move 1: " ++ show (sBM1 sst)
            ++ ", move 2: " ++ show (sBM2 sst) ++ ", move 3: " ++ show (sBM3 sst)
            ++ ", move 4+: " ++ show (sBM4p sst) ++ " (move 1%: "
            ++ show (if sBeta sst == 0 then 0 else 100 * sBM1 sst `div` sBeta sst) ++ ")"
    ] ++
          if nulDebug
             then [ "Null moves: " ++ show (sReBe sst) ++ ", Low: " ++ show (sReMi sst) ]
             else [ "Reduced: " ++ show (sRedu sst) ++ ", Re-Searched: " ++ show (sReSe sst) ]
