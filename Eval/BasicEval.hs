module Eval.BasicEval (
    matPiece, seeValue
) where

import Struct.Struct

matPiece1 :: Piece -> Int
matPiece1 Pawn   = 100
matPiece1 Knight = 360
matPiece1 Bishop = 360
matPiece1 Rook   = 565
matPiece1 Queen  = 1100
matPiece1 King   = 20000

fun :: Color -> Int -> Int
fun White = id
fun Black = negate

{-# INLINE matPiece #-}
matPiece :: Color -> Piece -> Int
matPiece c = fun c . matPiece1

seeValue :: Piece -> Int
seeValue Pawn   = 1
seeValue Knight = 3
seeValue Bishop = 3
seeValue Rook   = 5
seeValue Queen  = 10
seeValue King   = 200
