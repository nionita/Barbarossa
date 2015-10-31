{-# LANGUAGE BangPatterns #-}
module Moves.BitBoard (
    lsb, less, firstOne,
    bbToSquares,
    bbToSquaresV,
    bbToSquaresVS,
    bbToSquaresBB,
    shadowDown, shadowUp, uTestBit, uBit
    -- exactOne,
    -- bbFoldr
) where

import qualified Data.Vector.Unboxed as V
import Data.Bits
import Data.List (unfoldr, sort)

import Struct.Struct

-- First, the bit scan funtion
-- This could be replaced through an asm function for CPUs which have bitscan
{-# INLINE lsb #-}
lsb :: BBoard -> BBoard
lsb b = b .&. (-b)

{-# INLINE less #-}
less :: BBoard -> BBoard -> BBoard
less w1 w2 = w1 .&. complement w2

{-# INLINE firstOne #-}
firstOne :: BBoard -> Square
firstOne = bitToSquare . lsb

-- Here the bitboard must have exactly one bit set!
bitToSquare :: BBoard -> Square
bitToSquare !b = bitScanDatabase `V.unsafeIndex` mbsm b

bitScanMagic :: BBoard
bitScanMagic = 0x07EDD5E59A4E28C2

bitScanDatabase :: V.Vector Int
bitScanDatabase = V.fromListN 64 $ map snd $ sort [(mbsm ibit, i) | (i, ibit) <- ones ]
    where ones = take 64 $ zip [0..] $ iterate (`unsafeShiftL` 1) 1

{-# INLINE mbsm #-}
mbsm :: BBoard -> Int
mbsm x = fromIntegral $ (x * bitScanMagic) `unsafeShiftR` 58

{-# INLINE bbToSquares #-}
bbToSquares :: BBoard -> [Square]
bbToSquares bb = unfoldr f bb
    where f :: BBoard -> Maybe (Square, BBoard)
          f 0 = Nothing
          f b = Just $ extractSquare b

{-# INLINE bbToSquaresV #-}
bbToSquaresV :: BBoard -> V.Vector Square
bbToSquaresV bb = V.unfoldrN 32 f bb
    where f :: BBoard -> Maybe (Square, BBoard)
          f 0 = Nothing
          f b = Just $ extractSquare b

{-# INLINE bbToSquaresVS #-}
bbToSquaresVS :: Square -> BBoard -> V.Vector (Square, Square)
bbToSquaresVS src bb = V.unfoldrN 32 f bb
    where f :: BBoard -> Maybe ((Square, Square), BBoard)
          f 0 = Nothing
          f b = let (dst, b') = extractSquare b
                in Just $ ((src, dst), b')

{-# INLINE bbToSquaresBB #-}
bbToSquaresBB :: (Square -> BBoard) -> BBoard -> BBoard
bbToSquaresBB f bb = go bb 0
    where go 0 w = w
          go b w = let (sq, b') = extractSquare b
                       !w' = f sq .|. w
                   in go b' w'

{-# INLINE extractSquare #-}
extractSquare :: BBoard -> (Square, BBoard)
extractSquare b = let lsbb = lsb b
                      !sq = bitToSquare lsbb
                      nlsbb = complement lsbb
                      b' = b .&. nlsbb
                  in (sq, b')

-- Because the normal Bits operations are all safe
-- we define here the unsafe versions specialized for BBoard
{-# INLINE uTestBit #-}
uTestBit :: BBoard -> Int -> Bool
uTestBit w b = let bb = 1 `unsafeShiftL` b
               in w .&. bb /= 0

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

{--
{-# INLINE exactOne #-}
exactOne :: BBoard -> Bool
exactOne = (==1) . popCount

{-# INLINE bbFoldr #-}
bbFoldr :: (BBoard -> a -> a) -> a -> BBoard -> a
bbFoldr f = go
    where go !r 0 = r
          go !r b = let lsbb = lsb b
                    in go (f lsbb r) (b .&. complement lsbb)
--}
