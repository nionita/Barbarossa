{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Eval.Eval (
    posEval,
    posToIndexes, pieceToIdx, prettyQuiet
) where

import Data.Bits
import Debug.Trace (trace)

import Struct.Struct
import Struct.Status

import Eval.NNUE

------------------------------------------------------------------
-- Parameters of this module ------------
granCoarse, granCoarse2, granCoarseM :: Int
-- granCoarse, granCoarse2, granCoarseM, shift2Cp :: Int
granCoarse    = 4	-- coarse granularity
granCoarse2   = granCoarse `div` 2
granCoarseM   = complement (granCoarse - 1)
-- shift2Cp      = 3	-- we have 2^shift2Cp units per centipawn
-----------------------------------------------

matesc :: Int
matesc = 20000 - 255	-- warning, this is also defined in Base.hs!!

prod :: Bool
prod = False

-- Eval with NNUE!
{-# INLINE posEval #-}
posEval :: MyPos -> EvalState -> Int
posEval p (EvalState model)
    | prod || sce == sca = scc
    | otherwise          = error $ "Wrong sce = " ++ show sce ++ " correct would be: " ++ show sca
    where !sca = fromIntegral $ applyNNUE model ala
          ala = accumFromList model $ posToIndexes p (moving p)
          !sce = trace ("Eval: ala = " ++ show ala ++ ", mya = " ++ show (myAccum p))
              $ fromIntegral $ applyNNUE model $ myAccum p
          !scl = min matesc $ max (-matesc) sce
          !scc = if granCoarse > 0 then (scl + granCoarse2) .&. granCoarseM else scl

-- Direct NN structure: input layer sees all pieces from it pov
--
-- 12x64 -> L1 -> L2 -> 1
--
-- which means:
-- input has 12x64 = 768 inputs (0 or 1) where
-- the first 384 describe the part to move
-- and the next 384 describe the passive part
-- The input is sparse, we have at most 32 ones (4.16%)
-- The layout per color is: P, N, B, R, Q, K in order to compute the index quickly
-- from the ord function of the piece

posToIndexes :: MyPos -> Color -> [Int]
posToIndexes pos col = partToIndexes pos (me pos) col
     ++ map toPassive (partToIndexes pos (yo pos) col)

partToIndexes :: MyPos -> BBoard -> Color -> [Int]
partToIndexes pos part col
    =  pieceToIndexes Pawn   (part .&. pawns   pos) col
    ++ pieceToIndexes Knight (part .&. knights pos) col
    ++ pieceToIndexes Bishop (part .&. bishops pos) col
    ++ pieceToIndexes Rook   (part .&. rooks   pos) col
    ++ pieceToIndexes Queen  (part .&. queens  pos) col
    ++ pieceToIndexes King   (part .&. kings   pos) col

-- bbToSquares delivers the square number from white POV
-- so when we want the black perspective we have to mirror
pieceToIndexes :: Piece -> BBoard -> Color -> [Int]
pieceToIndexes p bb povcol
    | povcol == White = map ((+) offset) $ bbToSquares bb
    | otherwise       = map ((+) offset) $ map mirror $ bbToSquares bb
    where offset = fromEnum p * 64

-- Piece index for incremental update for both perspectives (white, black)
pieceToIdx :: Piece -> Color -> Square -> (Int, Int)
pieceToIdx piece color sq
    | color == White = (          offset + sq, passive + offset + sqm)
    | otherwise      = (passive + offset + sq,           offset + sqm)
    where offset  = fromEnum piece * 64
          passive = 384
          sqm = mirror sq

-- Change the square number as from black perspective (mirror)
-- file remains unchanged, the rank is complemented (000 <-> 111)
mirror :: Square -> Square
mirror = xor 56

-- Index mapping for the passive part
toPassive :: Int -> Int
toPassive = (+) 384

-- Function to filter training data for NNUE
-- We want a pretty quiet position, but really quite is probably too much
prettyQuiet :: MyPos -> Bool
prettyQuiet p
    | yoat == 0          = True		-- I have no attacks at all - really quiet
    | yoat .&. noya /= 0 = False	-- you have hanging pieces
    | epM /= 0           = False	-- minors en prise by pawns
    | epR /= 0           = False	-- rooks en prise by pawns or minors
    | epQ /= 0           = False	-- queens en prise by pawns, minors or rooks
    | otherwise          = True
    where !yoat = myAttacs p .&. yo p
          noya = complement (yoAttacs p)
          epM  = yo p .&. (knights p .|. bishops p) .&. myPAttacs p	-- minors attacked by pawns
          epR  = yo p .&. rooks  p .&. myA1	-- rooks attacked by pawns or minors
          epQ  = yo p .&. queens p .&. myA2	-- queens attacked by other than queens
          myA1 = myPAttacs p .|. myNAttacs p .|. myBAttacs p
          myA2 = myA1 .|. myRAttacs p
