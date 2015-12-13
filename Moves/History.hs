{-# LANGUAGE BangPatterns #-}
module Moves.History (
        History, newHist, toHist, valHist, prepHist
    ) where

import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as V
import Struct.Struct

type History = V.IOVector Int

rows, cols, vsize :: Int
rows = 12
cols = 64
vsize = rows * cols

adr :: Move -> Int
adr m = cols * f + t
    where f | moveColor m == White = e
            | otherwise            = 6 + e
          e = fromEnum $ movePiece m
          t = toSquare m

newHist :: IO History
newHist = V.replicate vsize 0

histw :: Int -> Int
histw !d = 1 `unsafeShiftL` dm
    where !dm = maxd - d
          maxd = 20

toHist :: History -> Bool -> Move -> Int -> IO ()
toHist h True  m d = addHist h (adr m) (histw d)
toHist h False m d = subHist h (adr m) (histw d)

{-# INLINE valHist #-}
valHist :: History -> Move -> IO Int
valHist !h = V.unsafeRead h . adr

addHist :: History -> Int -> Int -> IO ()
addHist h !ad !p = do
    a <- V.unsafeRead h ad
    let !u = a - p	-- trick here: we subtract, so that the sort is big to small
        !v = if u < lowLimit then lowHalf else u
    V.unsafeWrite h ad v
    where lowLimit = -1000000000
          lowHalf  =  -500000000

subHist :: History -> Int -> Int -> IO ()
subHist h !ad !p = do
    a <- V.unsafeRead h ad
    let !u = a + p	-- trick here: we add, so that the sort is big to small
        !v = if u > higLimit then higHalf else u
    V.unsafeWrite h ad v
    where higLimit = 1000000000
          higHalf  =  500000000

prepHist :: History -> IO ()
prepHist h = go 0
    where go :: Int -> IO ()
          go !i | i == vsize = return ()
                | otherwise  = do
                    -- a <- V.unsafeRead h i
                    -- V.unsafeWrite h i ((a+a+a) `unsafeShiftR` 2)
                    V.unsafeWrite h i 0
                    go (i+1)
