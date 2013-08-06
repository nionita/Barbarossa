{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, BangPatterns #-}
module Moves.SEE
    (
    findLKA,
    myPieces, yoPieces, thePieces,
    -- genMoveCaptSEE,
    genMoveCaptWL
    )
    where

import Prelude hiding ((++), concatMap, concat, map, reverse, tail, take, foldr,
                       null, repeat, filter, head, takeWhile, zip, length)
import Data.Bits
import Data.List.Stream
import Data.Ord
import Data.Array

import Struct.Struct
import Moves.Moves
import Moves.BitBoard
import Eval.BasicEval

-- find pinning lines for a piece type, given the king & piece squares
-- the queen is very hard, so we solve it as a composition of rook and bishop
-- and when we call findLKA we always know as which piece the queen checks
{-# INLINE findLKA #-}
findLKA Queen ksq psq
    | rAttacs bpsq ksq .&. bpsq == 0 = findLKA Bishop ksq psq
    | otherwise                      = findLKA Rook   ksq psq
    where bpsq = bit psq
findLKA pt ksq psq = (psq, kp .&. pk)
    where kp = fAttacs ksq pt $ bit psq
          pk = fAttacs psq pt $ bit ksq

{-# INLINE myPieces #-}
myPieces :: MyPos -> Color -> BBoard
myPieces !p !c = if c == White then white p else black p

{-# INLINE yoPieces #-}
yoPieces :: MyPos -> Color -> BBoard
yoPieces !p !c = if c == White then black p else white p

{-# INLINE thePieces #-}
thePieces :: MyPos -> Color -> (BBoard, BBoard)
thePieces p c = if c == White then (white p, black p) else (black p, white p)

-- The new SEE functions (swap-based)
-- Choose the cheapest of a set of pieces
chooseAttacker :: MyPos -> BBoard -> (BBoard, Int)
chooseAttacker pos !frompieces
    | p /= 0 = p1 `seq` (p1, value Pawn)
    | n /= 0 = n1 `seq` (n1, value Knight)
    | b /= 0 = b1 `seq` (b1, value Bishop)
    | r /= 0 = r1 `seq` (r1, value Rook)
    | q /= 0 = q1 `seq` (q1, value Queen)
    | k /= 0 = k1 `seq` (k1, value King)
    | otherwise = (0, 0)
    where p = frompieces .&. pawns pos
          n = frompieces .&. knights pos
          b = frompieces .&. bishops pos
          r = frompieces .&. rooks pos
          q = frompieces .&. queens pos
          k = frompieces .&. kings pos
          p1 = lsb p
          n1 = lsb n
          b1 = lsb b
          r1 = lsb r
          q1 = lsb q
          k1 = lsb k

newAttacs :: MyPos -> Square -> BBoard -> BBoard
newAttacs pos sq moved = bAttacs occ sq .&. (b .|. q)
                     .|. rAttacs occ sq .&. (r .|. q)
                     .|. nAttacs 0   sq .&. n
                     .|. kAttacs 0   sq .&. k
                     .|. (pAttacs White sq .&. black pos .|. pAttacs Black sq .&. white pos) .&. p
    where !occ = occup pos `less` moved
          !b = bishops pos  `less` moved
          !r = rooks pos    `less` moved
          !q = queens pos  `less` moved
          !n = knights pos `less` moved
          !k = kings pos   `less` moved
          !p = pawns pos   `less` moved

slideAttacs :: Square -> BBoard -> BBoard -> BBoard -> BBoard -> BBoard
slideAttacs sq b r q occup = bAttacs occup sq .&. (b .|. q)
                         .|. rAttacs occup sq .&. (r .|. q)

xrayAttacs :: MyPos -> Square -> Bool
xrayAttacs pos sq = sa1 /= sa0
    where !sa1 = slideAttacs sq (bishops pos) (rooks pos) (queens pos) (occup pos)
          !sa0 = slideAttacs sq (bishops pos) (rooks pos) (queens pos) 0

unimax gs a = foldl' (\a g -> min g (-a)) a gs

value = matPiece White

seeMoveValue :: MyPos -> Color -> Square -> Square -> Int -> Int
seeMoveValue pos col sqfa sqto gain0 = v
    where v = go gain0 attacs0 occup0 from0 valfrom moved0 (yopc, mypc) [gain0]
          go :: Int -> BBoard -> BBoard -> BBoard -> Int -> BBoard -> (BBoard, BBoard) -> [Int] -> Int
          go !gain !attacs !occ !from !val !moved (fcolp, ocolp) acc =
             let gain'    = val - gain
                 occ'     = occ    `xor` from
                 moved'   = moved   .|.  from
                 !attacs'' = attacs `xor` from
                 attacs'  = if posXRay && from .&. mayXRay /= 0
                               then newAttacs pos sqto moved'
                               else attacs''
                 -- attacs'  = newAttacs pos sqto moved'
                 (from', val') = chooseAttacker pos (attacs'' .&. ocolp)
             in if from' == 0
                   then unimax acc (minBound+2)
                   else go gain' attacs' occ' from' val' moved' (ocolp, fcolp) (gain':acc)
          (mypc, yopc) = thePieces pos col
          (from0, valfrom) = chooseAttacker pos (attacs0 .&. yopc)
          !mayXRay = pawns pos .|. bishops pos .|. rooks pos .|. queens pos
          !posXRay = xrayAttacs pos sqto
          !moved0  = bit sqfa
          attacs0 = newAttacs pos sqto moved0
          occup0  = occup pos `xor` moved0

-- This function can produce illegal captures with the king!
genMoveCaptWL :: MyPos -> Color -> ([(Square, Square)], [(Square, Square)])
genMoveCaptWL pos col = (wl, ll)
    where (wl, ll) = foldr (perCaptFieldWL pos col mypc yoAtt) ([],[]) $ squaresByMVV pos capts
          (mypc, yopc) = thePieces pos col
          (myAtt, yoAtt) = if col == White	-- here: for yoAtts: X-Ray is not considered!!!
                              then (whAttacs pos, blAttacs pos)
                              else (blAttacs pos, whAttacs pos)
          capts = myAtt .&. yopc

perCaptFieldWL :: MyPos -> Color -> BBoard -> BBoard -> Square
          -> ([(Square, Square)], [(Square, Square)])
          -> ([(Square, Square)], [(Square, Square)])
perCaptFieldWL pos col mypc advdefence sq mvlst
    = if hanging
         then foldr (addHanging sq) mvlst agrsqs
         else foldr (perCaptWL pos col valto sq) mvlst agrsqs
    where myattacs = mypc .&. newAttacs pos sq 0
          Busy _ pcto = tabla pos sq
          valto = value pcto
          hanging = not (advdefence `testBit` sq)
          agrsqs = squaresByLVA pos myattacs

approximateEasyCapts = True

perCaptWL :: MyPos -> Color -> Int -> Square -> Square
          -> ([(Square, Square)], [(Square, Square)])
          -> ([(Square, Square)], [(Square, Square)])
perCaptWL pos col gain0 sq sqfa (wsqs, lsqs)
    = if approx || adv <= gain0
         then (ss:wsqs, lsqs)
         else (wsqs, ss:lsqs)
    where ss = (sqfa, sq)
          approx = approximateEasyCapts && gain1 >= 0
          Busy _ pcfa = tabla pos sqfa
          v0 = value pcfa
          gain1 = gain0 - v0
          adv = seeMoveValue pos col sqfa sq v0

-- Captures of hanging pieces are always winning
addHanging :: Square -> Square
          -> ([(Square, Square)], [(Square, Square)])
          -> ([(Square, Square)], [(Square, Square)])
addHanging to from (wsqs, lsqs) = ((from, to):wsqs, lsqs)

squaresByMVV :: MyPos -> BBoard -> [Square]
squaresByMVV pos bb = map snd $ sortBy (comparing fst)
                              $ map (mostValuableFirst pos) $ bbToSquares bb

squaresByLVA :: MyPos -> BBoard -> [Square]
squaresByLVA pos bb = map snd $ sortBy (comparing fst)
                              $ map (mostValuableLast pos) $ bbToSquares bb

-- Sort by value in order to get the most valuable last
mostValuableLast :: MyPos -> Square -> (Int, Square)
mostValuableLast pos sq | Busy _ f <- tabla pos sq = let !v = value f in (v, sq)
mostValuableLast _   _                             = error "mostValuableLast: Empty"

-- Sort by negative value in order to get the most valuable first
mostValuableFirst :: MyPos -> Square -> (Int, Square)
mostValuableFirst pos sq | Busy _ f <- tabla pos sq = let !v = - value f in (v, sq)
mostValuableFirst _   _                             = error "mostValuableFirst: Empty"
