module Eval.BasicEval (
    matPiece, seeValue
) where

import Struct.Struct

matPiece1 :: Piece -> Int
matPiece1 Pawn   = 100
matPiece1 Knight = 420
matPiece1 Bishop = 434
matPiece1 Rook   = 670
matPiece1 Queen  = 1308
matPiece1 King   = 20000

fun :: Color -> Int -> Int
fun White = id
fun Black = negate

{-# INLINE matPiece #-}
matPiece :: Color -> Piece -> Int
matPiece c = fun c . matPiece1

seeValue :: Piece -> Int
seeValue Pawn   = 1
seeValue Knight = 4
seeValue Bishop = 4
seeValue Rook   = 7
seeValue Queen  = 13
seeValue King   = 200
