module Eval.BasicEval (
    matPiece, seeValue
) where

import Struct.Struct

seeValue :: Piece -> Int
seeValue Pawn   = 100
seeValue Knight = 360
seeValue Bishop = 360
seeValue Rook   = 565
seeValue Queen  = 1100
seeValue King   = 20000

fun :: Color -> Int -> Int
fun White = id
fun Black = negate

{-# INLINE matPiece #-}
matPiece :: Color -> Piece -> Int
matPiece c = fun c . seeValue
