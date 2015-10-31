{-# LANGUAGE BangPatterns #-}
module Moves.History (
        History, newHist, toHist, valHist, allHist, histSortMvs
    ) where

import Control.Monad.ST.Strict (runST)
import Data.Bits
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed.Mutable  as M
import qualified Data.Vector.Unboxed          as V
import qualified Data.Vector.Algorithms.Intro as V
import Struct.Struct

type History = M.IOVector Int

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
newHist = M.replicate vsize 0

histw :: Int -> Int
histw !d = 1 `unsafeShiftL` dm
    where !dm = maxd - d
          maxd = 20

toHist :: History -> Bool -> Move -> Int -> IO ()
toHist h True  m d = addHist h (adr m) (histw d)
toHist h False m d = subHist h (adr m) (histw d)

{-# INLINE valHist #-}
valHist :: History -> Move -> IO Int
valHist !h = M.unsafeRead h . adr

{-# INLINE allHist #-}
allHist :: History -> IO (V.Vector Int)
allHist = V.unsafeFreeze

histSortMvs :: V.Vector Int -> V.Vector Move -> V.Vector Move
histSortMvs hv mv = V.map fst $ runST (stSort vts)
    where vts = V.zip mv $ V.map (zeroHigh . V.unsafeIndex hv . adr) mv
          zeroHigh 0 = maxBound
          zeroHigh x = x
          stSort v = do
              vt <- V.unsafeThaw v
              V.sortBy (comparing snd) vt
              vf <- V.unsafeFreeze vt
              return vf

addHist :: History -> Int -> Int -> IO ()
addHist h !ad !p = do
    a <- M.unsafeRead h ad
    let !u = a - p	-- trick here: we subtract, so that the sort is big to small
        !v = if u < lowLimit then lowHalf else u
    M.unsafeWrite h ad v
    where lowLimit = -1000000000
          lowHalf  =  -500000000

subHist :: History -> Int -> Int -> IO ()
subHist h !ad !p = do
    a <- M.unsafeRead h ad
    let !u = a + p	-- trick here: we add, so that the sort is big to small
        !v = if u > higLimit then higHalf else u
    M.unsafeWrite h ad v
    where higLimit = 1000000000
          higHalf  =  500000000
