module Moves.Muster (
        genArray,
        row1,  row2,  row3,  row4,  row5,  row6,  row7,  row8,
        fileA, fileB, fileC, fileD, fileE, fileF, fileG, fileH,
        rowBB, colBB,
        rookFiles, lightSquares, darkSquares
    ) where

import Data.Word
import Data.Bits
import Data.Array.Unboxed

import Struct.Struct

row1  = 0xFF               :: BBoard
row2  = 0xFF00             :: BBoard
row3  = 0xFF0000           :: BBoard
row4  = 0xFF000000         :: BBoard
row5  = 0xFF00000000       :: BBoard
row6  = 0xFF0000000000     :: BBoard
row7  = 0xFF000000000000   :: BBoard
row8  = 0xFF00000000000000 :: BBoard

fileA = 0x0101010101010101 :: BBoard
fileB = 0x0202020202020202 :: BBoard
fileC = 0x0404040404040404 :: BBoard
fileD = 0x0808080808080808 :: BBoard
fileE = 0x1010101010101010 :: BBoard
fileF = 0x2020202020202020 :: BBoard
fileG = 0x4040404040404040 :: BBoard
fileH = 0x8080808080808080 :: BBoard

rookFiles = fileA .|. fileH

lightSquares = 0xAA55AA55AA55AA55 :: BBoard
darkSquares  = 0x55AA55AA55AA55AA :: BBoard

-- Arrays of rows and files
rowArray, fileArray :: UArray Int BBoard
rowArray  = listArray (0, 7) [row1,  row2,  row3,  row4,  row5,  row6,  row7,  row8]
fileArray = listArray (0, 7) [fileA, fileB, fileC, fileD, fileE, fileF, fileG, fileH]

rowBB, colBB :: Int -> BBoard
rowBB = (!) rowArray
colBB = (!) fileArray

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

genLeft ib@(i0, _) = genDir (\(i, b) -> (i-1, left b)) (\(i, _) -> i `div` 8 == i0 `div` 8) ib

genRight ib@(i0, _) = genDir (\(i, b) -> (i+1, right b)) (\(i, _) -> i `div` 8 == i0 `div` 8) ib

genDown = genDir (\(i, b) -> (i-8, down b)) (\(i, _) -> i >= 0)

genUp = genDir (\(i, b) -> (i+8, up b)) (\(i, _) -> i <= 63)

genFile e = e : genUp e ++ genDown e

genRow e = e : genLeft e ++ genRight e
