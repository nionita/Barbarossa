{-# LANGUAGE BangPatterns #-}

module Moves.CurSe (
        CurSe, newCurSe, deferMove, startSearching, finishedSearch
    ) where

import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as V

import Struct.Struct

type CurSe = V.IOVector ZKey

csSize, csWayB, csWays :: Int
-- csSize = 32768				-- number of clusters
csSize = 16384				-- number of clusters
csWayB = 2				-- shift bits for the address of a cluster
csWays = 1 `unsafeShiftL` csWayB	-- size of a cluster

csMask :: ZKey
csMask = fromIntegral $ csSize - 1

newCurSe :: IO CurSe
newCurSe = V.replicate (csSize * csWays) 0

adr :: ZKey -> Int
adr k = fromIntegral $ (k .&. csMask) `unsafeShiftL` csWayB

-- This part is credited to Tom Kerrigan
-- http://www.tckerrigan.com/Chess/Parallel_Search/Simplified_ABDADA/simplified_abdada.html

-- Should we defer the search of this move?
deferMove :: CurSe -> ZKey -> IO Bool
deferMove curse zkey = do
    let i = adr zkey
    go i csWays
    where go :: Int -> Int -> IO Bool
          go !_ 0 = return False
          go  i r = do
              k <- V.unsafeRead curse i
              if k == zkey
                 then return True
                 else go (i+1) (r-1)

-- Mark the start of a move search
-- Returns the address which was used, so that finishedSearch
-- can delete it directly
startSearching :: CurSe -> ZKey -> IO Int
startSearching curse zkey = do
    let i = adr zkey
    go i i csWays
    where go :: Int -> Int -> Int -> IO Int
          go !i !_ 0 = do
              V.unsafeWrite curse i zkey
              return i
          go  i j r = do
              k <- V.unsafeRead curse j
              if k == 0
                 then do
                     V.unsafeWrite curse j zkey
                     return j
                 else if k /= zkey
                         then go i (j+1) (r-1)
                         else return j

-- Mark the end of a move search
finishedSearch :: CurSe -> Int -> IO ()
finishedSearch curse i = V.unsafeWrite curse i 0
