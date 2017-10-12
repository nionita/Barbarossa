{-# LANGUAGE BangPatterns #-}

module Moves.CurSe (
        CurSe, newCurSe, deferMove, startSearching, finishedSearch
    ) where

import Control.Monad
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
              -- k <- V.read curse i
              if k == zkey
                 then return True
                 else go (i+1) (r-1)

-- Mark the start of a move search
startSearching :: CurSe -> ZKey -> IO ()
startSearching curse zkey = do
    let i = adr zkey
    go i i csWays
    where go :: Int -> Int -> Int -> IO ()
          go !i !_ 0 = V.unsafeWrite curse i zkey
          go  i  j r = do
              k <- V.unsafeRead curse j
              -- k <- V.read curse j
              if k == 0
                 then V.unsafeWrite curse j zkey
                 else if k /= zkey
                         then go i (j+1) (r-1)
                         else return ()

-- Mark the end of a move search
finishedSearch :: CurSe -> ZKey -> IO ()
finishedSearch curse zkey = do
    let i = adr zkey
    go i csWays
    where go :: Int -> Int -> IO ()
          go !_ 0 = return ()
          go  i r = do
              k <- V.unsafeRead curse i
              -- k <- V.read curse i
              when (k == zkey) $ V.unsafeWrite curse i 0
              go (i+1) (r-1)
