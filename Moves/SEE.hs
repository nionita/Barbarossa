{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, BangPatterns #-}
module Moves.SEE
    (
    findLKA,
    myPieces, yoPieces, thePieces,
    genMoveCaptSEE,
    genMoveCaptWL
    -- valueSEE, figAttacking, allAttackingPawns
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

{-# INLINE srcDestsSEE #-}
srcDestsSEE :: Piece -> (Square -> BBoard) -> Square -> [((Piece, Square), Square)]
srcDestsSEE p f !s = zip (repeat (p, s)) $ bbToSquares $ f s

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
chooseAttacker :: MyPos -> BBoard -> (BBoard, (Piece, Int))
chooseAttacker pos frompieces = go funcPiecesAsc
    where go [] = (0, (Pawn, 0))	-- should never happen
          go ((p,f):fps)
             | subset == 0 = go fps
             | otherwise   = (lsb subset, (p, value p))
             where subset = frompieces .&. f pos

funcPiecesAsc = [ (Pawn, pawns), (Knight, knights), (Bishop, bishops),
                  (Rook, rooks), (Queen, queens), (King, kings) ]
-- funcPiecesDsc = reverse funcPiecesAsc	-- will we need this?

funcPiecesSli = [ (bAttacs, bishops), (rAttacs, rooks), (qAttacs, queens) ]

allAttackingPawns :: MyPos -> Square -> BBoard -> BBoard
allAttackingPawns pos sq moved
    = (pAttacs White sq .&. black pos .|. pAttacs Black sq .&. white pos) .&. (pawns pos `less` moved)

newAttacs :: MyPos -> Square -> BBoard -> BBoard
newAttacs pos sq moved = foldl' go (allAttackingPawns pos sq moved) $ tail funcPiecesAsc
    where go z (p, f) = z .|. (modi f .&. fAttacs sq p (occup pos `less` moved))
          modi f = f pos `less` moved

-- unimax []     a = a
-- unimax (g:gs) a = unimax gs (min g (-a))

unimax gs a = foldl' (\a g -> min g (-a)) a gs

value = matPiece White

genMoveCaptSEE :: MyPos -> Color -> [(Square, Square)]
genMoveCaptSEE pos col = foldr (perCapt pos col mypc) [] $ bbToSquares capts
    where (mypc, yopc) = thePieces pos col
          myAtt = if col == White then whAttacs pos else blAttacs pos
          capts = myAtt .&. yopc

perCapt :: MyPos -> Color -> BBoard -> Square -> [(Square, Square)] -> [(Square, Square)]
perCapt pos col mypc sq sqs = if v >= 0 then (sqf, sq) : sqs else sqs
    where attacs = newAttacs pos sq 0
          -- mymovp = attacs .&. (mypc `less` pinned pos) -- `less` myking -- kinv
          Busy _ pcto = tabla pos sq
          -- (v, sqf) = valueSEE pos col sq attacs (value pcto)
          (v, sqf) = valueSEE pos col sq pcto
          myking = mypc .&. kings pos
          kinv = if attacs `less` mypc /= 0 then myking else 0

valueSEE :: MyPos -> Color -> Square -> Piece -> (Int, Square)
valueSEE pos col sqto pieceto = (v, firstOne initset)
    where v = go gain0 attacs0 (occup pos) initset p valfrom 0 (mypc, yopc) [(p, pieceto, gain0)]
          go :: Int -> BBoard -> BBoard -> BBoard -> Piece -> Int -> BBoard -> (BBoard, BBoard) -> [(Piece, Piece, Int)] -> Int
          go gain attacs occ fromset pfrom val moved (fcolp, ocolp) acc =
             let gain'    = val - gain
                 occ'     = occ    `xor` fromset
                 moved'   = moved   .|.  fromset
                 attacs'' = attacs `xor` fromset
                 attacs'  = if fromset .&. mayXRay /= 0 then newAttacs pos sqto moved' else attacs''
                 (fromset', (p, val')) = chooseAttacker pos (attacs'' .&. ocolp)
             in if fromset' == 0
                   then unimax (map (\(_,_,x) -> x) acc) (minBound+2)
                   else go gain' attacs' occ' fromset' p val' moved' (ocolp, fcolp) ((p, pfrom, gain'):acc)
          gain0 = value pieceto
          (mypc, yopc) = thePieces pos col
          (initset, (p, valfrom)) = chooseAttacker pos (attacs0 .&. mypc)
          mayXRay = pawns pos .|. bishops pos .|. rooks pos .|. queens pos
          attacs0 = newAttacs pos sqto 0

seeMoveValue :: MyPos -> Color -> Square -> Square -> Int -> Int
seeMoveValue pos col sqfa sqto gain0 = v
    where v = go gain0 attacs0 occup0 from0 valfrom moved0 (yopc, mypc) [gain0]
          go :: Int -> BBoard -> BBoard -> BBoard -> Int -> BBoard -> (BBoard, BBoard) -> [Int] -> Int
          go gain attacs occ from val moved (fcolp, ocolp) acc =
             let gain'    = val - gain
                 occ'     = occ    `xor` from
                 moved'   = moved   .|.  from
                 attacs'' = attacs `xor` from
                 attacs'  = if from .&. mayXRay /= 0 then newAttacs pos sqto moved' else attacs''
                 (from', (_, val')) = chooseAttacker pos (attacs'' .&. ocolp)
             in if from' == 0
                   then unimax acc (minBound+2)
                   else go gain' attacs' occ' from' val' moved' (ocolp, fcolp) (gain':acc)
          (mypc, yopc) = thePieces pos col
          (from0, (_, valfrom)) = chooseAttacker pos (attacs0 .&. yopc)
          mayXRay = pawns pos .|. bishops pos .|. rooks pos .|. queens pos
          moved0  = bit sqfa
          attacs0 = newAttacs pos sqto moved0
          occup0  = occup pos `xor` moved0

-- This function can produce illegal captures with the king!
genMoveCaptWL :: MyPos -> Color -> ([(Square, Square)], [(Square, Square)])
genMoveCaptWL pos col = (swl, sll)
    where (wl, ll) = foldr (perCaptFieldWL pos col mypc yoAtt) ([],[]) $ bbToSquares capts
          (mypc, yopc) = thePieces pos col
          (myAtt, yoAtt) = if col == White	-- here: for yoAtts: X-Ray is not considered!!!
                              then (whAttacs pos, blAttacs pos)
                              else (blAttacs pos, whAttacs pos)
          capts = myAtt .&. yopc
          swl = map snd $ sortBy (comparing fst) wl
          sll = map snd $ sortBy (comparing fst) ll

perCaptFieldWL :: MyPos -> Color -> BBoard -> BBoard -> Square
          -> ([(Int, (Square, Square))], [(Int, (Square, Square))])
          -> ([(Int, (Square, Square))], [(Int, (Square, Square))])
perCaptFieldWL pos col mypc advdefence sq mvlst
    = if hanging
         then foldr (addHanging valto sq) mvlst $ bbToSquares myattacs
         else foldr (perCaptWL pos col valto sq) mvlst $ bbToSquares myattacs
    where myattacs = mypc .&. newAttacs pos sq 0
          Busy _ pcto = tabla pos sq
          valto = value pcto
          hanging = not (advdefence `testBit` sq)

approximateEasyCapts = True

perCaptWL :: MyPos -> Color -> Int -> Square -> Square
          -> ([(Int, (Square, Square))], [(Int, (Square, Square))])
          -> ([(Int, (Square, Square))], [(Int, (Square, Square))])
perCaptWL pos col gain0 sq sqfa (wsqs, lsqs)
    = if approx || adv <= gain0
         -- then (inssort ss wsqs, lsqs)
         -- else (wsqs, inssort ss lsqs)
         then (ss:wsqs, lsqs)
         else (wsqs, ss:lsqs)
    where ss = (-win, (sqfa, sq))
          approx = approximateEasyCapts && gain1 >= 0
          win = if approx then gain1 else myv
          Busy _ pcfa = tabla pos sqfa
          v0 = value pcfa
          gain1 = gain0 - v0
          adv = seeMoveValue pos col sqfa sq v0
          myv = gain0 - adv

-- Captures of hanging pieces are always winning
addHanging :: Int -> Square -> Square
          -> ([(Int, (Square, Square))], [(Int, (Square, Square))])
          -> ([(Int, (Square, Square))], [(Int, (Square, Square))])
-- addHanging val to from (wsqs, lsqs) = (inssort (val, (from, to)) wsqs, lsqs)
addHanging val to from (wsqs, lsqs) = ((-val, (from, to)):wsqs, lsqs)

inssort vp [] = [vp]
inssort vp@(v, _) vps@(vp1@(v1, _) : vp1s)
    | v >= v1   = vp : vps
    | otherwise = vp1 : inssort vp vp1s
