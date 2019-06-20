module Eval.BasicEval (
    matPiece, seeValue
) where

import Struct.Struct

seeValue :: Piece -> Int
seeValue Pawn   = 100
seeValue Knight = 378
seeValue Bishop = 378
seeValue Rook   = 594
seeValue Queen  = 1140
seeValue King   = 20000

fun :: Color -> Int -> Int
fun White = id
fun Black = negate

{-# INLINE matPiece #-}
matPiece :: Color -> Piece -> Int
matPiece c = fun c . seeValue
