{-# LANGUAGE BangPatterns #-}
module Moves.BitBoard (
    popCount, popCount1, lsb, bbToSquares, less, firstOne, exactOne, bbToSquaresBB,
    uTestBit
) where

-- import Control.Exception (assert)
import Data.Array.Base
import Data.Array.Unboxed
import Data.Bits hiding (popCount)
import qualified Data.Bits as B
import Data.List.Stream (unfoldr)
import Data.Word

import Struct.Struct

-- First, the bit scan funtion
-- This could be replaced through an asm function for CPUs which have bitscan
{-# INLINE lsb #-}
lsb :: BBoard -> BBoard
lsb b = b .&. (-b)

{-# INLINE exactOne #-}
exactOne :: BBoard -> Bool
exactOne = (==1) . B.popCount

{-# INLINE less #-}
less :: BBoard -> BBoard -> BBoard
less w1 w2 = w1 .&. complement w2

{-# INLINE firstOne #-}
firstOne :: BBoard -> Square
firstOne = bitToSquare . lsb

-- Here the bitboard must have exactly one bit set!
bitToSquare :: BBoard -> Square
bitToSquare !b = bitScanDatabase `unsafeAt` mbsm b

bitScanMagic :: BBoard
bitScanMagic = 0x07EDD5E59A4E28C2

bitScanDatabase :: UArray Int Int
bitScanDatabase = array (0, 63) paar
    where ones = take 64 $ zip [0..] $ iterate (`unsafeShiftL` 1) 1
          paar = [(mbsm bit, i) | (i, bit) <- ones]

{-# INLINE mbsm #-}
mbsm :: BBoard -> Int
mbsm x = fromIntegral $ (x * bitScanMagic) `unsafeShiftR` 58

{-# INLINE bbToSquares #-}
bbToSquares :: BBoard -> [Square]
bbToSquares bb = unfoldr f bb
    where f :: BBoard -> Maybe (Square, BBoard)
          f 0  = Nothing
          f b = let lsbb = lsb b
                    !sq = bitToSquare lsbb
                    nlsbb = complement lsbb
                    b' = b .&. nlsbb
                in Just (sq, b')

{-# INLINE bbToSquaresBB #-}
bbToSquaresBB :: (Square -> BBoard) -> BBoard -> BBoard
bbToSquaresBB f bb = go bb 0
    where go 0 w = w
          go b w = let lsbb = lsb b
                       !sq = bitToSquare lsbb
                       nlsbb = complement lsbb
                       b' = b .&. nlsbb
                       !w' = f sq .|. w
                   in go b' w'

-- Population count function, good for bigger populations:
{-# INLINE popCount #-}
popCount :: BBoard -> Int
popCount = B.popCount

-- Population count function, good for small populations:
{-# INLINE popCount1 #-}
popCount1 :: BBoard -> Int
popCount1 = B.popCount

-- Because the normal Bits operations are all safe
-- we define here the unsafe versions specialized for BBoard
{-# INLINE uTestBit #-}
uTestBit :: BBoard -> Int -> Bool
uTestBit w b = let bb = 1 `unsafeShiftL` b
               in w .&. bb /= 0
