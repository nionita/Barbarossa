{-# LANGUAGE BangPatterns #-}
module Moves.Moves (
    movesInit, pAttacs, pawnWhiteAttacks, pawnBlackAttacks,
    fAttacs,
    pMovs,
    kAttacs, qAttacs, rAttacs, bAttacs, nAttacs,
    pAll1Moves, pAll2Moves,
    getAlignedDiag, getAlignedRoCo
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

-- The moves of a white pawn (no captures)
pawnSlideW :: Square -> BBoard -> BBoard
pawnSlideW !sq oc
    | bb1 .&. oc /= 0               = 0
    | row /= sec || bb2 .&. oc /= 0 = bb1
    | otherwise                     = bb12
    where bb1 = 1 `unsafeShiftL` (sq + 8)
          !bb2 = bb1 `unsafeShiftL` 8
          !bb12 = bb1 .|. bb2
          !row = sq `unsafeShiftR` 3
          sec = 1

-- The moves of a black pawn (no captures)
pawnSlideB :: Square -> BBoard -> BBoard
pawnSlideB !sq oc
    | bb1 .&. oc /= 0               = 0
    | row /= sec || bb2 .&. oc /= 0 = bb1
    | otherwise                     = bb12
    where bb1 = 1 `unsafeShiftL` (sq - 8)	-- here L is ok! Replaces a 'bit sq `shiftR` 8'
          !bb2 = bb1 `unsafeShiftR` 8
          !bb12 = bb1 .|. bb2
          !row = sq `unsafeShiftR` 3
          sec = 6

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

pMovs :: Square -> Color -> BBoard -> BBoard
pMovs s White o = pawnSlideW s o
pMovs s Black o = pawnSlideB s o

pAll1Moves :: Color -> BBoard -> BBoard -> [(Square, Square)]
pAll1Moves White !ps !occ = map f $ bbToSquares $ (ps `unsafeShiftL` 8) `less` occ
    where f !x = (x - 8, x)
pAll1Moves Black !ps !occ = map f $ bbToSquares $ (ps `unsafeShiftR` 8) `less` occ
    where f !x = (x + 8, x)

pAll2Moves :: Color -> BBoard -> BBoard -> [(Square, Square)]
pAll2Moves White ps occ = map f $ bbToSquares $ (ps2 `unsafeShiftL` 16) `less` occ2
    where ps2 = ps .&. 0x000000000000FF00
          occ2 = occ .|. (occ `unsafeShiftL` 8)
          f !x = (x - 16, x)
pAll2Moves Black ps occ = map f $ bbToSquares $ (ps2 `unsafeShiftR` 16) `less` occ2
    where ps2 = ps .&. 0x00FF000000000000
          occ2 = occ .|. (occ `unsafeShiftR` 8)
          f !x = (x + 16, x)

{-# INLINE fAttacs #-}
fAttacs :: Square -> Piece -> BBoard -> BBoard  -- piece attacs except pawn
fAttacs sq Bishop !oc = bAttacs oc sq
fAttacs sq Rook   !oc = rAttacs oc sq
fAttacs sq Queen  !oc = qAttacs oc sq
fAttacs sq King   _   = kAttacs    sq
fAttacs sq Knight _   = nAttacs    sq
fAttacs _  _      _  = 0	-- this would be for pawn, which is calculated different

-- We want to know if 2 squares are aligned on a diagonal or line/column
-- Tipically we have the square (Int) and a bitboard with some pieces (for example kings
-- or sliders)
alignedDiag, alignedRoCo :: UArray Int BBoard
alignedDiag = listArray (0, 63) $ map (bAttacs 0) [0..63]
alignedRoCo = listArray (0, 63) $ map (rAttacs 0) [0..63]

getAlignedDiag, getAlignedRoCo :: Int -> BBoard -> BBoard
getAlignedDiag s bb = (alignedDiag `unsafeAt` s) .&. bb
getAlignedRoCo s bb = (alignedRoCo `unsafeAt` s) .&. bb
