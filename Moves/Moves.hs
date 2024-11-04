{-# LANGUAGE BangPatterns #-}
module Moves.Moves (
    movesInit, pAttacs, pawnWhiteAttacks, pawnBlackAttacks, targetPawnMoves,
    fAttacs, kAttacs, qAttacs, rAttacs, bAttacs, nAttacs
    ) where

import Data.Array.Base
import Data.Bits

import Struct.Struct
import Moves.GenMagics
import Moves.Magics
import Moves.BitBoard
import Moves.Pattern

-- Used to compute all the needed tables by initialiasation:
movesInit :: Int
movesInit
    | w == 0    = 0
    | otherwise = 1
    where r = rAttacs 0 0
          b = bAttacs 1 0
          k = kAttacs 2
          n = nAttacs 3
          w = r .|. b .|. k .|. n

-- To implement Array of Structures, we use 3 entries of an array of bitboards
-- per square and do manually the index and masking computations

-- Move tables and hash function for sliding pieces
data SlMoves = SlMoves {
        database :: !DbArray,
        sqmagics :: !MaArray
    }

sliderMoves :: (Square -> [(Int, BBoard)]) -> (Square -> BBoard) -> ShArray -> MaArray -> SlMoves
sliderMoves genSlider genSlMask sBits sMagic = SlMoves { database = bdb, sqmagics = mgarr }
    where (bdb, bdbb) = genDatabase genSlider
          mgarr = listArray (0, ub) $ concatMap f [0..63]
          ub = 64 * 3 - 1
          f sq = [ begsh, msk, mag ]
              where beg = bdbb  `unsafeAt` sq
                    sh  = sBits `unsafeAt` sq
                    msk = genSlMask sq
                    mag = sMagic `unsafeAt` sq
                    begsh = (fromIntegral beg `unsafeShiftL` 16) .|. fromIntegral sh

rookMoves, bishopMoves :: SlMoves
rookMoves = sliderMoves genRook genRookMask rBits rMagic
bishopMoves = sliderMoves genBishop genBishopMask bBits bMagic

{-# INLINE smoves #-}
smoves :: SlMoves -> BBoard -> Square -> BBoard
smoves bbmoves occ sq = database bbmoves `unsafeAt` idx
    where sq3   = sq + sq + sq
          begsh = sqmagics bbmoves `unsafeAt` sq3
          msk   = sqmagics bbmoves `unsafeAt` (sq3+1)
          mag   = sqmagics bbmoves `unsafeAt` (sq3+2)
          beg = fromIntegral (begsh `unsafeShiftR` 16)
          sh  = fromIntegral (begsh .&. 0xFFFF)
          off = fromIntegral $ ((occ .&. msk) * mag) `unsafeShiftR` sh
          idx = beg + off

{-# INLINE fmoves #-}
fmoves :: MaArray -> Square -> BBoard
fmoves = unsafeAt

{-# INLINE kAttacs #-}
{-# INLINE rAttacs #-}
{-# INLINE bAttacs #-}
{-# INLINE qAttacs #-}
{-# INLINE nAttacs #-}
kAttacs, nAttacs :: Square -> BBoard
rAttacs, bAttacs, qAttacs :: BBoard -> Square -> BBoard
kAttacs = fmoves movKings
nAttacs = fmoves movKnights
rAttacs = smoves rookMoves
bAttacs = smoves bishopMoves
qAttacs occ sq = smoves bishopMoves occ sq .|. smoves rookMoves occ sq

-- Pawn attacs
pawnWhiteAttacks, pawnBlackAttacks :: BBoard -> BBoard
pawnWhiteAttacks !b = (bbLeft b .|. bbRight b) `unsafeShiftL` 8
pawnBlackAttacks !b = (bbLeft b .|. bbRight b) `unsafeShiftR` 8
{-# INLINE pawnWhiteAttacks #-}
{-# INLINE pawnBlackAttacks #-}

pAttacs :: Color -> Square -> BBoard
pAttacs White sq = pawnWhiteAttacks $ uBit sq
pAttacs Black sq = pawnBlackAttacks $ uBit sq
{-# INLINE pAttacs #-}

-- From / to regular pawn moves (no captures) to the given squares (bitboard)
-- Used to block a check by a pawn move
-- Because some pawns can be moved 2 squares, we must have the occupancy
-- We go back from the target squares to find the source squares
{-# INLINE targetPawnMoves #-}
targetPawnMoves :: Color -> BBoard -> BBoard -> BBoard -> [(Square, Square)]
targetPawnMoves White pws occ bb = pAll1Moves White (pws .&. bb1) ++ pAll2Moves White (pws .&. bb2)
    where bb1 = (bb `less` occ) `unsafeShiftR` 8
          bb2 = ((bb1 `less` occ) `unsafeShiftR` 8) .&. row2
targetPawnMoves Black pws occ bb = pAll1Moves Black (pws .&. bb1) ++ pAll2Moves Black (pws .&. bb2)
    where bb1 = (bb `less` occ) `unsafeShiftL` 8
          bb2 = ((bb1 `less` occ) `unsafeShiftL` 8) .&. row7

-- Generate from / to regular pawn moves with 1 step, given only pawns that are not blocked
-- This is not the general case! To generate all possible pawn moves, use targetPawnMoves with
-- whole board (0xFFFF...) as a target
pAll1Moves :: Color -> BBoard -> [(Square, Square)]
pAll1Moves White ps = map (\x -> (x - 8, x)) $ bbToSquares $ ps `unsafeShiftL` 8
pAll1Moves Black ps = map (\x -> (x + 8, x)) $ bbToSquares $ ps `unsafeShiftR` 8

-- Generate from / to regular pawn moves with 2 steps, given only pawns on second rank that are not blocked
-- This is not the general case! To generate all possible pawn moves, use targetPawnMoves with
-- whole board (0xFFFF...) as a target
pAll2Moves :: Color -> BBoard -> [(Square, Square)]
pAll2Moves White ps = map (\x -> (x - 16, x)) $ bbToSquares $ ps `unsafeShiftL` 16
pAll2Moves Black ps = map (\x -> (x + 16, x)) $ bbToSquares $ ps `unsafeShiftR` 16

{-# INLINE fAttacs #-}
fAttacs :: Square -> Piece -> BBoard -> BBoard  -- piece attacs except pawn
fAttacs sq Bishop !oc = bAttacs oc sq
fAttacs sq Rook   !oc = rAttacs oc sq
fAttacs sq Queen  !oc = qAttacs oc sq
fAttacs sq King   _   = kAttacs    sq
fAttacs sq Knight _   = nAttacs    sq
fAttacs _  _      _  = 0	-- this would be for pawn, which is calculated different
