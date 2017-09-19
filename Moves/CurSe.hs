{-# LANGUAGE BangPatterns #-}

module Moves.CurSe (
        CurSe, newCurSe, deferMove, startSearching, finishedSearch
    ) where

import Control.Monad
import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as V

import Struct.Struct

-- Int32 should be enough for now with max depth 20
type CurSe = V.IOVector ZKey

csSize, csWays :: Int
csSize = 32768
csWays = 4

csMask :: ZKey
csMask = fromIntegral $ csSize - 1

newCurSe :: IO CurSe
newCurSe = V.replicate (csSize * csWays) 0

adr :: ZKey -> Int
adr k = fromIntegral $ fromIntegral csWays * k

-- This part is credited to Tom Kerrigan
-- http://www.tckerrigan.com/Chess/Parallel_Search/Simplified_ABDADA/simplified_abdada.html

deferMove :: CurSe -> ZKey -> IO Bool
deferMove curse zkey = do
    let n = zkey .&. csMask
        i = adr n
    go i csWays
    where go :: Int -> Int -> IO Bool
          go !_ 0 = return False
          go  i r = do
              k <- V.unsafeRead curse i
              if k == zkey
                 then return True
                 else go (i+1) (r-1)

startSearching :: CurSe -> ZKey -> IO ()
startSearching curse zkey = do
    let n = zkey .&. csMask
        i = adr n
    go i i csWays
    where go :: Int -> Int -> Int -> IO ()
          go !i !_ 0 = V.unsafeWrite curse i zkey
          go  i  j r = do
              k <- V.unsafeRead curse j
              if k == 0
                 then V.unsafeWrite curse j zkey
                 else if k /= zkey
                         then go i (j+1) (r-1)
                         else return ()

finishedSearch :: CurSe -> ZKey -> IO ()
finishedSearch curse zkey = do
    let n = zkey .&. csMask
        i = adr n
    go i csWays
    where go :: Int -> Int -> IO ()
          go !_ 0 = return ()
          go  i r = do
              k <- V.unsafeRead curse i
              when (k == zkey) $ V.unsafeWrite curse i 0
              go (i+1) (r-1)
