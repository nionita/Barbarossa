{-# LANGUAGE BangPatterns #-}
module Moves.History (
        History, newHist, toHist, valHist
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
histw !d = maxb `unsafeShiftR` d
    where maxb = 1 `unsafeShiftL` 20

toHist :: History -> Bool -> Move -> Int -> IO ()
toHist h True  m d = addHist h (adr m) (histw d)
toHist h False m d = subHist h (adr m) (histw d)

{-# INLINE valHist #-}
valHist :: History -> Move -> IO Int
valHist !h = V.unsafeRead h . adr

addHist :: History -> Int -> Int -> IO ()
addHist h !ad !p = do
    a <- V.unsafeRead h ad
    let !v = (a - lowLimit) `unsafeShiftR` 1
        -- trick here: we subtract, so that the sort is big to small
        !u | p <= v    = a - p
           | otherwise = a - v
    V.unsafeWrite h ad u
    where lowLimit = - (maxBound `unsafeShiftR` 1)

subHist :: History -> Int -> Int -> IO ()
subHist h !ad !p = do
    a <- V.unsafeRead h ad
    let !v = (higLimit - a) `unsafeShiftR` 1
        -- trick here: we add, so that the sort is big to small
        !u | p <= v    = a + p
           | otherwise = a + v
    V.unsafeWrite h ad u
    where higLimit = maxBound `unsafeShiftR` 1
