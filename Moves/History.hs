{-# LANGUAGE BangPatterns #-}

module Moves.History (
        History, newHist, toHist, valHist, histSortMoves
    ) where

import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad.ST
import Data.Bits
import qualified Data.Set as S
import Data.Ord (comparing)
import qualified Data.Vector.Algorithms.Heap as H	-- Intro sort was slower
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed         as U
import Data.Word

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

{-# INLINE histSortMoves #-}
histSortMoves :: Int -> History -> [Move] -> [Move]
histSortMoves d h ms
    | d > 1     = mtsList $ MTS h $ S.fromList $ map (\(Move w) -> fromIntegral w) ms
    | otherwise = dirSort h ms

data MovesToSort = MTS History (S.Set Word16)

mtsList :: MovesToSort -> [Move]
mtsList (MTS h ms)
    | S.null ms = []
    | otherwise = m : mtsList mts
    where (m, mts) = runST $ do
              let uz = zipVals h $ S.elems ms
              vz <- U.unsafeThaw uz
              H.selectBy (comparing snd) vz 1
              w <- fst <$> V.unsafeRead vz 0
              return (Move w, MTS h (S.delete w ms))

zipVals :: History -> [Word16] -> U.Vector (Word16, Int)
zipVals h ws = runST $ do
    v  <- unsafeIOToST $ U.unsafeFreeze h
    let uw = U.fromList ws
        uv = U.map (\w -> U.unsafeIndex v (adr (Move w))) uw
    vw <- U.unsafeThaw uw
    vv <- U.unsafeThaw uv
    uz <- U.unsafeFreeze $ V.zip vw vv
    return uz

dirSort :: History -> [Move] -> [Move]
dirSort h ms = runST $ do
    let uz = zipVals h $ map (\(Move w) -> w) ms
    vz <- U.unsafeThaw uz
    H.sortBy (comparing snd) vz
    us <- U.unsafeFreeze vz
    let (uw, _) = U.unzip us
    return $ map Move $ U.toList uw
