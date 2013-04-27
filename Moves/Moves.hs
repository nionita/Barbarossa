{-# LANGUAGE BangPatterns #-}
module Moves.Moves (
    movesInit, pAttacs,
    fAttacs,
    pMovs,
    kAttacs, qAttacs, rAttacs, bAttacs, nAttacs,
    pAll1Moves, pAll2Moves
    ) where

import Data.Array.Base
import Data.Array.Unboxed
import Data.Bits

import Struct.Struct
import Moves.GenMagics
import Moves.Magics
import Moves.BitBoard
import Moves.Muster

-- Used to compute all the needed tables by initialiasation:
movesInit
    | w == 0    = 0
    | otherwise = 1
    where r = rAttacs 0 0
          b = bAttacs 1 0
          k = kAttacs 2
          n = nAttacs 3
          w = r .|. b .|. k .|. n

-- Move tables and hash function for sliding pieces
data SlMoves = SlMoves {
        database :: DbArray,
        dbbegins :: ShArray,
        shifts   :: ShArray,
        masks    :: MaArray,
        magics   :: MaArray
    }

(bdb, bdbb) = genDatabase genBishop
(rdb, rdbb) = genDatabase genRook

bishopMoves = SlMoves {
    database = bdb, dbbegins = bdbb, shifts = bBits,
    masks = mskBishop, magics = bMagic
        }

rookMoves = SlMoves {
    database = rdb, dbbegins = rdbb, shifts = rBits,
    masks = mskRook, magics = rMagic
        }

{-# INLINE smoves #-}
smoves :: SlMoves -> BBoard -> Square -> BBoard
smoves bbmoves occup sq = database bbmoves `unsafeAt` idx
    where idx = dbbegins bbmoves `unsafeAt` sq + off
          off = fromIntegral
                    $ ((occup .&. masks bbmoves `unsafeAt` sq) * magics bbmoves `unsafeAt` sq)
                        `unsafeShiftR` (shifts bbmoves `unsafeAt` sq)

{-# INLINE smoves2 #-}
smoves2 :: SlMoves -> SlMoves -> BBoard -> Square -> BBoard
smoves2 bbmoves1 bbmoves2 occup sq
    = bb1 .|. bb2
    where bb1 = database bbmoves1 `unsafeAt` idx1
          bb2 = database bbmoves2 `unsafeAt` idx2
          idx1 = dbbegins bbmoves1 `unsafeAt` sq + off1
          idx2 = dbbegins bbmoves2 `unsafeAt` sq + off2
          off1 = fromIntegral
                    $ ((occup .&. masks bbmoves1 `unsafeAt` sq) * magics bbmoves1 `unsafeAt` sq)
                        `unsafeShiftR` (shifts bbmoves1 `unsafeAt` sq)
          off2 = fromIntegral
                    $ ((occup .&. masks bbmoves2 `unsafeAt` sq) * magics bbmoves2 `unsafeAt` sq)
                        `unsafeShiftR` (shifts bbmoves2 `unsafeAt` sq)

{-# INLINE fmoves #-}
fmoves :: MaArray -> Square -> BBoard
-- fmoves maarr sq = maarr `unsafeAt` sq
fmoves = unsafeAt

{-# INLINE kAttacs #-}
{-# INLINE rAttacs #-}
{-# INLINE bAttacs #-}
{-# INLINE qAttacs #-}
{-# INLINE nAttacs #-}
kAttacs = fmoves movKings
rAttacs = smoves rookMoves
bAttacs = smoves bishopMoves
qAttacs = smoves2 bishopMoves rookMoves
nAttacs = fmoves movKnights

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
whitePawnAtt, blackPawnAtt :: MaArray
whitePawnAtt = array (0, 63) $ genArray 0x00000050000 9
blackPawnAtt = array (0, 63) $ genArray 0x50000000000 49

pAttacs :: Color -> Square -> BBoard
pAttacs White sq = whitePawnAtt `unsafeAt` sq
pAttacs Black sq = blackPawnAtt `unsafeAt` sq
{-# INLINE pAttacs #-}

pMovs s White o = pawnSlideW s o
pMovs s Black o = pawnSlideB s o

pAll1Moves :: Color -> BBoard -> BBoard -> [(Square, Square)]
pAll1Moves White !pawns !occup = map f $ bbToSquares $ (pawns `unsafeShiftL` 8) `less` occup
    where f !x = (x - 8, x)
pAll1Moves Black !pawns !occup = map f $ bbToSquares $ (pawns `unsafeShiftR` 8) `less` occup
    where f !x = (x + 8, x)

pAll2Moves :: Color -> BBoard -> BBoard -> [(Square, Square)]
pAll2Moves White pawns occup = map f $ bbToSquares $ (pawns2 `unsafeShiftL` 16) `less` occ2
    where pawns2 = pawns .&. 0x000000000000FF00
          occ2 = occup .|. (occup `unsafeShiftL` 8)
          f !x = (x - 16, x)
pAll2Moves Black pawns occup = map f $ bbToSquares $ (pawns2 `unsafeShiftR` 16) `less` occ2
    where pawns2 = pawns .&. 0x00FF000000000000
          occ2 = occup .|. (occup `unsafeShiftR` 8)
          f !x = (x + 16, x)

{-# INLINE fAttacs #-}
fAttacs :: Square -> Piece -> BBoard -> BBoard  -- piece attacs except pawn
fAttacs sq King   _  = kAttacs sq
fAttacs sq Knight _  = nAttacs sq
fAttacs sq Bishop oc = bAttacs oc sq
fAttacs sq Rook   oc = rAttacs oc sq
fAttacs sq Queen  oc = qAttacs oc sq
fAttacs _  _      _  = 0	-- this would be for pawn, which is calculated different
