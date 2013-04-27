{-# LANGUAGE BangPatterns #-}
module Moves.GenMagics (
    genDatabase, genBishop, genRook,
    mskBishop, mskRook,
    movKings, movKnights
    ) where

import Data.Array.Unboxed
import Data.Bits
import Data.List
import qualified Data.Set as S
import Data.Word

import Struct.Struct
import Moves.Magics
import Moves.Muster

-- Bring one configuration of the given length into a bitboard
-- according to a given pattern
-- The pattern is expressed as an offset from the square, where bit 0 of
-- the configuration will be placed, and a direction for every further bit:
-- 1 for right, -1 for left
-- The pattern will be shifted by the target square before beeing applied
confToBB :: Int -> Int -> Int -> Square -> Word8 -> BBoard
confToBB offs dir len sq conf
    = foldl' setIf 0 $ zip (bitList conf len) $ repeat (sq+offs)
    where setIf w ((_, False), _) = w
          setIf w ((i, True), f) = w `setBit` (dir*i + f)

bitList c l = zip [0..] [ c `testBit` i | i <- [0..l-1]]

-- Bring the configuration to different positions relative to a square
toEast len  = confToBB len (-1) len
toNorth len = confToBB (8*len) (-8) len
toWest len  = confToBB (-len) 1 len
toSouth len = confToBB (-8*len) 8 len
toNoEa len  = confToBB (9*len) (-9) len
toNoWe len  = confToBB (7*len) (-7) len
toSoWe len  = confToBB (-9*len) 9 len
toSoEa len  = confToBB (-7*len) 7 len

-- Generate the mask for sliding pieces per square
genSlMask :: Bool -> Square -> BBoard
genSlMask isrook sq = ma1 .|. ma2 .|. ma3 .|. ma4
    where (ql1, ql2) = quarterLen sq
          (q1, q2, q3, q4) = if isrook then ql1 else ql2
          all1 = 0xFF
          ma1 = if isrook then toEast  q1 sq all1
                          else toNoEa  q1 sq all1
          ma2 = if isrook then toNorth q2 sq all1
                          else toNoWe  q2 sq all1
          ma3 = if isrook then toWest  q3 sq all1
                          else toSoWe  q3 sq all1
          ma4 = if isrook then toSouth q4 sq all1
                          else toSoEa  q4 sq all1

-- Rook masks per square
mskRook :: MaArray
mskRook = listArray (0, 63) $ map (genSlMask True) [0..63]

-- Bishop masks per square
mskBishop :: MaArray
mskBishop = listArray (0,63) $ map (genSlMask False) [0..63]

movKnights :: MaArray
movKnights = array (0, 63) $ genArray 0x0000000A1100110A 18

movKings :: MaArray
movKings = array (0, 63) $ genArray 0x0000000000070507 9

-- Generate the list of bishop moves by occupancy for one square
genBishop :: Square -> [(Int, BBoard)]
genBishop sq = S.elems . S.fromList $ zip has rez
    where inpr = genInpRez False sq
          inps = map fst inpr
          rez = map snd inpr 
          has = map (compHash sq bBits bMagic) inps

-- Generate the list of rook moves by occupancy for one square
genRook :: Square -> [(Int, BBoard)]
genRook sq = S.elems . S.fromList $ zip has rez
    where inpr = genInpRez True sq
          inps = map fst inpr
          rez = map snd inpr 
          has = map (compHash sq rBits rMagic) inps

genDatabase :: (Square -> [(Int, BBoard)]) -> (DbArray, ShArray)
genDatabase f = (db, bg)
    where lis = map f [0..63]
          anf = scanl (+) 0 $ map ((+ 1) . maximum . map fst) lis
          dba = concat $ zipWith (\a l -> map (offset a) l) anf lis
          offset a (i, b) = (a+i, b)
          db = accumArray repl 0 (0, last anf - 1) dba
          bg = listArray (0, length anf - 1) anf
          repl _ b = b

-- This computes the hash of an input given the square, the shift and
-- magic arrays
compHash :: Square -> ShArray -> MaArray -> BBoard -> Int
compHash sq shfa mula inp = fromIntegral $ shiftR (ma * inp) sh
    where ma = mula!sq
          sh = shfa!sq

-- Generate the relevant inputs and computes the corresponding attacs
genInpRez :: Bool -> Square -> [(BBoard, BBoard)]
genInpRez isrook sq = lis
    where lelis = dlis isrook sq
          lis = concatMap goutp lelis
          goutp g = zip (inputs g) $ repeat (attacs g)

data Group = Group {
                leader :: !BBoard,
                attacs :: !BBoard,
                inputs :: [BBoard]
                }

-- Generate all group leaders and all inputs and attacs bitboards for every
-- group leader given a piece type (rook or bishop) and the square
dlis :: Bool -> Square -> [Group]
dlis rook sq = do
    (le1, (at1, fo1)) <- src1
    (le2, (at2, fo2)) <- src2
    (le3, (at3, fo3)) <- src3
    (le4, (at4, fo4)) <- src4
    let fo = [f1 .|. f2 .|. f3 .|. f4 | f1 <- fo1, f2 <- fo2,
                f3 <- fo3, f4 <- fo4]
    return Group { leader = le1 .|. le2 .|. le3 .|. le4,
                   attacs = at1 .|. at2 .|. at3 .|. at4,
                   inputs = fo
                   }
    where src1 = if rook then partLi toEast  sq q1 a1
                         else partLi toNoEa  sq q1 a1
          src2 = if rook then partLi toNorth sq q2 a2
                         else partLi toNoWe  sq q2 a2
          src3 = if rook then partLi toWest  sq q3 a3
                         else partLi toSoWe  sq q3 a3
          src4 = if rook then partLi toSouth sq q4 a4
                         else partLi toSoEa  sq q4 a4
          (q1, q2, q3, q4) = if rook then qqr else qqb
          (qqr, qqb) = quarterLen sq
          (a1, a2, a3, a4) = if rook then qar else qab
          (qar, qab) = attacLen sq

-- Return a list of partial input given a direction function, the square
-- the length of the quarter and the length of the attacs in that direction
partLi f sq 0 a = [(0, (f a sq 0xff, [0]))]
partLi f sq q a = do
    ql <- qleader q
    let fo = map (f q sq) $ qconfig ql
        at = f a sq $ qattac ql
    return (f q sq ql, (at, fo))

-- Compute the quarter lengths for a square
quarterLen :: Square -> ((Int, Int, Int, Int), (Int, Int, Int, Int))
quarterLen !sq = ((e, n, w, s), (ne, nw, sw, se))
    where ho = sq `mod` 8
          ve = sq `div` 8
          e = if ho <= 6 then 6 - ho else 0
          w = if ho >= 1 then ho - 1 else 0
          n = if ve <= 6 then 6 - ve else 0
          s = if ve >= 1 then ve - 1 else 0
          ne = min n e
          nw = min n w
          se = min s e
          sw = min s w

-- Compute the attac lengths for a square
attacLen :: Square -> ((Int, Int, Int, Int), (Int, Int, Int, Int))
attacLen !sq = ((e, n, w, s), (ne, nw, sw, se))
    where ho = sq `mod` 8
          ve = sq `div` 8
          e = 7 - ho
          w = ho
          n = 7 - ve
          s = ve
          ne = min n e
          nw = min n w
          se = min s e
          sw = min s w

-- Generate all possible quarter leaders for a given field witdh.
-- For example, in a quarter of length 2, we have 3 leaders:
-- 10, 01 and 00
qleader :: Int -> [Word8]
qleader 0 = []
qleader !x = 0 : take x (iterate (`shiftR` 1) $ 1 `shiftL` (x-1))

-- Generate all possible quarter configurations for one quarter leader
qconfig :: Word8 -> [Word8]
qconfig 0 = [0]
qconfig !b = [ b .|. x | x <- [0..b-1]]

-- Generate the attac bitboard for one quarter leader
qattac :: Word8 -> Word8
qattac 0 = 0xFF
qattac !b = foldl' (.|.) 0 $ take 8 $ drop 1 $ iterate (`shiftL` 1) b
