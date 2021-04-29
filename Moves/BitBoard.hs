{-# LANGUAGE BangPatterns #-}
module Moves.BitBoard (
    lsbBBoard, bbToSquares, less, firstOne,
    bbToSquaresBB,
    shadowDown, shadowUp, uTestBit, uBit
) where

import Data.Bits
import Data.List (unfoldr)

import Struct.Struct

-- A bitboard with only one bit set from the argument (least significant bit)
{-# INLINE lsbBBoard #-}
lsbBBoard :: BBoard -> BBoard
lsbBBoard = uBit . firstOne

{-# INLINE less #-}
less :: BBoard -> BBoard -> BBoard
less w1 w2 = w1 .&. complement w2

-- First set bit number (lsb order)
{-# INLINE firstOne #-}
firstOne :: BBoard -> Square
firstOne = countTrailingZeros

{-# INLINE bbToSquares #-}
bbToSquares :: BBoard -> [Square]
bbToSquares = unfoldr f
    where f :: BBoard -> Maybe (Square, BBoard)
          f 0 = Nothing
          f b = Just $ extractSquare b

-- Which implementation is better?
{-# INLINE bbToSquaresBB #-}
bbToSquaresBB :: (Square -> BBoard) -> BBoard -> BBoard
bbToSquaresBB f = foldr (\sq w -> f sq .|. w) 0 . bbToSquares
{-
bbToSquaresBB f = go 0
    where go w 0 = w
          go w b = let (sq, b') = extractSquare b
                       !w' = f sq .|. w
                   in go w' b'
-}

{-# INLINE extractSquare #-}
extractSquare :: BBoard -> (Square, BBoard)
extractSquare b = let !sq = firstOne b
                  in (sq, b `xor` uBit sq)

-- Because the normal Bits operations are all safe
-- we define here the unsafe versions specialized for BBoard
{-# INLINE uTestBit #-}
uTestBit :: BBoard -> Int -> Bool
uTestBit w b = w .&. uBit b /= 0

{-# INLINE uBit #-}
uBit :: Square -> BBoard
uBit = unsafeShiftL 1

{-# INLINE shadowDown #-}
shadowDown :: BBoard -> BBoard
shadowDown !wp = wp3
    where !wp0 =          wp  `unsafeShiftR`  8
          !wp1 = wp0 .|. (wp0 `unsafeShiftR`  8)
          !wp2 = wp1 .|. (wp1 `unsafeShiftR` 16)
          !wp3 = wp2 .|. (wp2 `unsafeShiftR` 32)

{-# INLINE shadowUp #-}
shadowUp :: BBoard -> BBoard
shadowUp !wp = wp3
    where !wp0 =          wp  `unsafeShiftL`  8
          !wp1 = wp0 .|. (wp0 `unsafeShiftL`  8)
          !wp2 = wp1 .|. (wp1 `unsafeShiftL` 16)
          !wp3 = wp2 .|. (wp2 `unsafeShiftL` 32)
