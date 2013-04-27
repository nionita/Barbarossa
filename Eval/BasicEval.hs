module Eval.BasicEval (
    matPiece
) where

import Data.Array.Unboxed
import Data.Array.Base
import GHC.Arr (unsafeIndex)

import Struct.Struct

matvals :: UArray Piece Int
matvals = listArray (Pawn, King) [ 100, 325, 325, 500, 975, 20000 ]

matPiece1 :: Piece -> Int
matPiece1 Pawn   = 100
matPiece1 Knight = 325
matPiece1 Bishop = 325
matPiece1 Rook   = 500
matPiece1 Queen  = 975
matPiece1 King   = 20000

{-# INLINE matPiece #-}
matPiece :: Color -> Piece -> Int
{-
matPiece c p = case c of
               White ->   matPiece1 p
               Black -> - matPiece1 p
-}
matPiece White = unsafeAt matvals . unsafeIndex (Pawn, King)
matPiece Black = negate . unsafeAt matvals . unsafeIndex (Pawn, King)
