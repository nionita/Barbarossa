{-# LANGUAGE BangPatterns #-}
module Moves.History (
        History, newHist, toHist, valHist, histSortMoves
    ) where

import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad.ST.Lazy
import Data.Bits
import qualified Data.IntSet as S
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed         as U
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
    | otherwise = ms

data MovesToSort = MTS History S.IntSet

mtsList :: MovesToSort -> [Move]
mtsList (MTS h ms)
    | S.null ms = []
    | otherwise = m : mtsList mts
    where (m, mts) = runST $ do
              v <- strictToLazyST . unsafeIOToST $ U.unsafeFreeze h
              let Just (m'@(Move w), _)
                      = foldr (better v) Nothing $ map (Move . fromIntegral) $ S.elems ms
              return (m', MTS h (S.delete (fromIntegral w) ms))

better :: U.Vector Int -> Move -> Maybe (Move, Int) -> Maybe (Move, Int)
better v m Nothing = Just (m, U.unsafeIndex v (adr m))
better v m old@(Just (_, s0))
    = let !s = U.unsafeIndex v (adr m)
      in if s < s0 then Just (m, s)
                   else old
