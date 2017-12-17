{-# LANGUAGE BangPatterns #-}

module Moves.CurSe (
        CurSe, newCurSe, finishedSearch, reserveSearch
    ) where

import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as V

import Struct.Struct

type CurSe = V.IOVector ZKey

csSize, csWayB, csWays :: Int
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
-- The API was simplified to only 2 functions

-- Mark the end of a move search
finishedSearch :: CurSe -> Int -> IO ()
finishedSearch curse i = V.unsafeWrite curse i 0

-- Can we start a move search?
-- If yes, returns the address which was used, so that
-- finishedSearch can delete it directly
-- Logic is: search for the key
-- - if found: we cant reserve
-- - if not found:
--   - if one free entry found: reserve in that entry
--   - otherwise reserve in a random entry (1 from 4)
reserveSearch :: CurSe -> ZKey -> IO (Maybe Int)
reserveSearch curse zkey = do
    let i = adr zkey
    k <- V.unsafeRead curse i
    let randi = i + fromIntegral (k .&. 0x03)	-- the random entry index
    go csWays k i (-1) randi
    where go :: Int -> ZKey -> Int -> Int -> Int -> IO (Maybe Int)
          go 0  !k !crt !freei !randi =
             if k == zkey
                then return Nothing
                else do
                    let !r | k == 0    = crt
                           | freei < 0 = randi
                           | otherwise = freei
                    V.unsafeWrite curse r zkey
                    return (Just r)
          go res k crt freei randi =
             if k == zkey
                then return Nothing
                else do
                    let !nz1 | k == 0    = crt
                             | otherwise = freei
                        !crt1 = crt + 1
                        !res1 = res - 1
                    k1 <- V.unsafeRead curse crt1
                    go res1 k1 crt1 nz1 randi
