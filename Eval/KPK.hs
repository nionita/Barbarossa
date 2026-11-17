{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

-- | KPK bitbase: for every position with king and pawn against king, whether the side with the pawn wins.
-- The table is computed by retrograde passes at the first use (or when forced by 'kpkInit'), 24 KB.
-- Design: Eval/KPK-BITBASE.md on rel090.
module Eval.KPK (
    kpkProbe,
    kpkWinNorm,
    kpkIndex,
    kpkSize,
    kpkInit
) where

import Control.Monad (when)
import Data.Array.Base (unsafeAt, unsafeRead, unsafeWrite)
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bits
import Data.Word

-- Normalised positions: the strong side (with the pawn) is white, the pawn is on the files a-d, ranks 2-7.
-- stm: 0 = the strong side moves, 1 = the weak side moves; wk, bk: strong, weak king; psq: the pawn
-- Squares: a1 = 0, h1 = 7, a8 = 56
kpkIndex :: Int -> Int -> Int -> Int -> Int
kpkIndex !stm !wk !bk !psq = wk .|. bk `unsafeShiftL` 6 .|. stm `unsafeShiftL` 12
                           .|. (psq .&. 3) `unsafeShiftL` 13 .|. ((psq `unsafeShiftR` 3) - 1) `unsafeShiftL` 15

-- 2 * 64 * 64 * 4 * 6 positions, 3072 words
kpkSize :: Int
kpkSize = 196608

-- | Win for the strong side in a normalised position (see 'kpkIndex')
kpkWinNorm :: Int -> Int -> Int -> Int -> Bool
kpkWinNorm !stm !wk !bk !psq = (kpkBits `unsafeAt` (i `unsafeShiftR` 6)) `testBit` (i .&. 63)
    where !i = kpkIndex stm wk bk psq

-- | Win for the side with the pawn, from real squares
kpkProbe :: Bool        -- the pawn is white
         -> Bool        -- the side with the pawn moves
         -> Int         -- king of the side with the pawn
         -> Int         -- king of the other side
         -> Int         -- pawn
         -> Bool
kpkProbe !pw !sm !sk !ok !psq = kpkWinNorm stm (sk `xor` x) (ok `xor` x) (psq `xor` x)
    where !xr  = if pw then 0 else 56                   -- black pawn: flip the ranks
          !xf  = if psq .&. 7 >= 4 then 7 else 0        -- files e-h: mirror the files
          !x   = xr .|. xf
          !stm = if sm then 0 else 1

-- | One word of the table: forcing it computes the whole table
kpkInit :: Word64
kpkInit = kpkBits `unsafeAt` 0

kpkBits :: UArray Int Word64
kpkBits = listArray (0, kpkSize `unsafeShiftR` 6 - 1) $ map word [0 .. kpkSize `unsafeShiftR` 6 - 1]
    where word w = foldr (\b acc -> if kpkStates `unsafeAt` (w * 64 + b) == stWin then setBit acc b else acc)
                         0 [0 .. 63]

stUnknown, stInvalid, stDraw, stWin :: Word8
stUnknown = 0
stInvalid = 1
stDraw    = 2
stWin     = 3

-- The king moves of every square, 8 slots per square, -1 where there is none
kingTargets :: UArray Int Int
kingTargets = listArray (0, 64 * 8 - 1) $ concatMap tgs [0 .. 63]
    where tgs sq = take 8 $ [ t | (df, dr) <- dirs, let f = sq .&. 7 + df, let r = sq `unsafeShiftR` 3 + dr,
                                  f >= 0, f < 8, r >= 0, r < 8, let t = r * 8 + f ] ++ repeat (-1)
          dirs = [ (df, dr) | df <- [-1, 0, 1], dr <- [-1, 0, 1], df /= 0 || dr /= 0 ]

dist :: Int -> Int -> Int
dist !a !b = max (abs (a .&. 7 - b .&. 7)) (abs (a `unsafeShiftR` 3 - b `unsafeShiftR` 3))

-- Squares attacked by the (white) pawn
pawnAttacks :: Int -> Int -> Bool
pawnAttacks !psq !sq = sq `unsafeShiftR` 3 == psq `unsafeShiftR` 3 + 1 && abs (sq .&. 7 - psq .&. 7) == 1

decode :: Int -> (Int, Int, Int, Int)
decode !i = (i `unsafeShiftR` 12 .&. 1, i .&. 63, i `unsafeShiftR` 6 .&. 63,
             (i `unsafeShiftR` 15 + 1) * 8 + (i `unsafeShiftR` 13 .&. 3))

-- First classification
initial :: Int -> Word8
initial !i
    | wk == bk || dist wk bk <= 1 || wk == psq || bk == psq = stInvalid
    | stm == 0 && pawnAttacks psq bk                         = stInvalid
    | stm == 0 = if psq >= 48 && promo /= wk && promo /= bk && (dist bk promo > 1 || dist wk promo == 1)
                    then stWin
                    else stUnknown
    | null bmoves                                            = stDraw   -- stalemate
    | dist bk psq == 1 && dist wk psq > 1                    = stDraw   -- takes the pawn
    | otherwise                                              = stUnknown
    where (stm, wk, bk, psq) = decode i
          !promo = psq + 8
          bmoves = weakMoves wk bk psq

-- Legal moves of the weak king (the capture of the pawn included, when the pawn is not defended)
weakMoves :: Int -> Int -> Int -> [Int]
weakMoves !wk !bk !psq = [ t | j <- [0 .. 7], let t = kingTargets `unsafeAt` (bk * 8 + j), t >= 0,
                               dist t wk > 1, not (pawnAttacks psq t), t /= psq || dist wk psq > 1 ]

-- The strong side's successors (normalised indices, weak side to move); promotions are in 'initial'
strongSuccs :: Int -> Int -> Int -> [Int]
strongSuccs !wk !bk !psq = kms ++ pms
    where kms = [ kpkIndex 1 t bk psq | j <- [0 .. 7], let t = kingTargets `unsafeAt` (wk * 8 + j), t >= 0,
                                        dist t bk > 1, t /= psq ]
          !p1 = psq + 8
          pms | psq >= 48              = []
              | p1 == wk || p1 == bk   = []
              | psq < 16 && p2 /= wk && p2 /= bk = [kpkIndex 1 wk bk p1, kpkIndex 1 wk bk p2]
              | otherwise              = [kpkIndex 1 wk bk p1]
          !p2 = psq + 16

kpkStates :: UArray Int Word8
kpkStates = runSTUArray $ do
    a <- newArray (0, kpkSize - 1) stUnknown
    let fill !i | i >= kpkSize = return ()
                | otherwise    = unsafeWrite a i (initial i) >> fill (i + 1)
        anyWin []     = return False
        anyWin (j:js) = do
            s <- unsafeRead a j
            if s == stWin then return True else anyWin js
        allWin []     = return True
        allWin (j:js) = do
            s <- unsafeRead a j
            if s == stWin then allWin js else return False
        step !i !c
            | i >= kpkSize = return c
            | otherwise    = do
                s <- unsafeRead a i
                if s /= stUnknown
                   then step (i + 1) c
                   else do
                       let (stm, wk, bk, psq) = decode i
                       w <- if stm == 0
                               then anyWin (strongSuccs wk bk psq)
                               else allWin [ kpkIndex 0 wk t psq | t <- weakMoves wk bk psq ]
                       if w then unsafeWrite a i stWin >> step (i + 1) (c + 1)
                            else step (i + 1) c
        passes = do
            c <- step 0 (0 :: Int)
            when (c > 0) passes
    fill 0
    passes
    -- What is still unknown is a draw
    let fin !i | i >= kpkSize = return ()
               | otherwise    = do
                   s <- unsafeRead a i
                   when (s == stUnknown) $ unsafeWrite a i stDraw
                   fin (i + 1)
    fin 0
    return a
