module Moves.Muster (
        genArray,
        row1,  row2,  row3,  row4,  row5,  row6,  row7,  row8,
        fileA, fileB, fileC, fileD, fileE, fileF, fileG, fileH,
        rowBB, colBB,
        rookFiles, lightSquares, darkSquares
    ) where

-- import Data.Word
import Data.Bits
import Data.Array.Unboxed

import Struct.Struct

row1, row2, row3, row4, row5, row6, row7, row8 :: BBoard
row1  = 0xFF
row2  = 0xFF00
row3  = 0xFF0000
row4  = 0xFF000000
row5  = 0xFF00000000
row6  = 0xFF0000000000
row7  = 0xFF000000000000
row8  = 0xFF00000000000000

fileA, fileB, fileC, fileD, fileE, fileF, fileG, fileH :: BBoard
fileA = 0x0101010101010101
fileB = 0x0202020202020202
fileC = 0x0404040404040404
fileD = 0x0808080808080808
fileE = 0x1010101010101010
fileF = 0x2020202020202020
fileG = 0x4040404040404040
fileH = 0x8080808080808080

rookFiles :: BBoard
rookFiles = fileA .|. fileH

lightSquares, darkSquares :: BBoard
lightSquares = 0xAA55AA55AA55AA55
darkSquares  = 0x55AA55AA55AA55AA

-- Arrays of rows and files
rowArray, fileArray :: UArray Int BBoard
rowArray  = listArray (0, 7) [row1,  row2,  row3,  row4,  row5,  row6,  row7,  row8]
fileArray = listArray (0, 7) [fileA, fileB, fileC, fileD, fileE, fileF, fileG, fileH]

rowBB, colBB :: Int -> BBoard
rowBB = (!) rowArray
colBB = (!) fileArray

left, right, up, down :: BBoard -> BBoard
left  = flip shiftR 1 . (.&.) (complement fileA)
right = flip shiftL 1 . (.&.) (complement fileH)
down  = flip shiftR 8
up    = flip shiftL 8

type Elem = (Int, BBoard)

genArray :: BBoard -> Int -> [Elem]
genArray b i = concatMap genRow $ genFile e
    where e = (i, b)

genDir :: (Elem -> Elem) -> (Elem -> Bool) -> Elem -> [Elem]
genDir g f e = drop 1 $ takeWhile f $ iterate g e

genLeft, genRight, genUp, genDown, genFile, genRow :: Elem -> [Elem]
genLeft ib@(i0, _) = genDir (\(i, b) -> (i-1, left b)) (\(i, _) -> i `div` 8 == i0 `div` 8) ib

genRight ib@(i0, _) = genDir (\(i, b) -> (i+1, right b)) (\(i, _) -> i `div` 8 == i0 `div` 8) ib

genDown = genDir (\(i, b) -> (i-8, down b)) (\(i, _) -> i >= 0)

genUp = genDir (\(i, b) -> (i+8, up b)) (\(i, _) -> i <= 63)

genFile e = e : genUp e ++ genDown e

genRow e = e : genLeft e ++ genRight e
