{-# LANGUAGE BangPatterns #-}

module Search.AlbetaTypes (
    DoResult(..),
    ABControl(..),
    SStats(..),
    ssts0, formatStats, addStats,
    nulDebug
) where

import Data.Int
import Struct.Struct

data ABControl = ABC {
        maxdepth  :: Int,
        lastpv    :: [Move],
        lastscore :: Maybe Int,
        rootmvs   :: [Move],
        window    :: Int,
        mainThrd  :: Bool,
        stoptime  :: Int
    } deriving Show

data DoResult = Exten !Int !Bool	-- return mit extension & special
              | Final !Int	-- return with a final score (probably draw)
              | Illegal		-- illegal move

-- Data type for search statistics:
data SStats = SStats {
        sNodes, sNodesQS,
        sRetr, sRFnd, sRFal, sRSuc, sBeta, sBM1, sBM2, sBM3, sBM4p,
        sRedu, sReMi, sReBe, sReSe, sReNo :: !Int64
    } deriving Show

ssts0 :: SStats
ssts0 = SStats { sNodes = 0, sNodesQS = 0,
                 sRetr = 0, sRFnd = 0, sRFal = 0, sRSuc = 0,
                 sBeta = 0, sBM1 = 0, sBM2 = 0, sBM3 = 0, sBM4p = 0,
                 sRedu = 0, sReMi = 0, sReBe = 0, sReSe = 0, sReNo = 0 }

addStats :: SStats -> SStats -> SStats
addStats a b = SStats {
    sNodes = sNodes a + sNodes b, sNodesQS = sNodesQS a + sNodesQS b,
    sRetr = sRetr a + sRetr b, sRFnd = sRFnd a + sRFnd b, sRFal = sRFal a + sRFal b,
    sRSuc = sRSuc a + sRSuc b,
    sBeta = sBeta a + sBeta b, sBM1 = sBM1 a + sBM1 b, sBM2 = sBM2 a + sBM2 b,
    sBM3  = sBM3 a + sBM3 b, sBM4p = sBM4p a + sBM4p b,
    sRedu = sRedu a + sRedu b, sReMi = sReMi a + sReMi b, sReBe = sReBe a + sReBe b,
    sReSe = sReSe a + sReSe b, sReNo = sReNo a + sReNo b
    }

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
