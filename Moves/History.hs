{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module Moves.History (
        History, newHist, initHist, toHist, fromHist, histSortMoves
    ) where

import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad.ST
import Control.Monad (forM_)
import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed (UArray, listArray)
import Data.Bits
#ifndef REPRO_HIST
import Data.List (unfoldr)
#endif
import Data.Ord (comparing)
import qualified Data.Vector.Algorithms.Heap as H	-- Intro sort was slower
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed         as U
import Data.Int
import Data.Word
#ifndef REPRO_HIST
import System.Random
#endif

import Struct.Struct
import Struct.Pst (pstValues)
import Moves.Core (gamePhase)

-- Int32 should be enough for now with max depth 20
type History = V.IOVector Int32

pieces, squares, vsize, bloff :: Int
pieces  = 16
squares = 64
vsize = pieces * squares
bloff = vsize `div` 2

-- The layout is per color, then per piece type, then per square
-- This should be more cache friendly for sorting the quiet moves
{-# INLINE adr #-}
adr :: Move -> Int
adr m = ofs' m + adr' m

-- These functions are used to calculate faster the addresses of a list of moves
-- as they have all the same color, so the offset needs to be calculated only once
{-# INLINE adrs #-}
adrs :: [Move] -> [Int]
adrs []     = []
adrs (m:ms) = off + adr' m : map f ms
    where !off = ofs' m
          f x = off + adr' x

{-# INLINE adr' #-}
adr' :: Move -> Int
adr' m = squares * moveHisAdr m + toSquare m

{-# INLINE ofs' #-}
ofs' :: Move -> Int
ofs' m = bloff * moveHisOfs m

-- Produce small random numbers to initialize the new history
#ifndef REPRO_HIST
smallVals :: RandomGen g => g -> [Word32]
smallVals g = concatMap chop $ randoms g
    where chop w = unfoldr f (w, 16)
          f :: (Word32, Int) -> Maybe (Word32, (Word32, Int))
          f (_, 0) = Nothing
          f (w, k) = Just (w .&. 3, (w `unsafeShiftR` 2, k-1))
#endif

newHist :: IO History
#ifdef REPRO_HIST
newHist = U.thaw $ U.replicate vsize 0
#else
newHist = do
    g <- newStdGen
    U.thaw $ U.fromList $ map fromIntegral $ take vsize $ smallVals g
#endif

-- Initialize the history for a new search, mode (bits, 0 = zero as before):
-- 1: from the compiled PST in the phase of the root position: per piece the value of the to square
--    minus the mean of that piece over the squares (so that only the square counts, not the material),
--    negated (smaller is better), shifted by the given shift (left, or right when negative)
-- 2: piece order of the move generation (knight, bishop, rook, queen, pawn, king), histOrderStep per
--    step: with mode 3 the PST orders only within a piece (the step grows with the shift, so that the
--    shifted PST values, at most about 2500 >> 4 with shift -4, stay below it)
-- Castles get the king values of the to square
-- Without REPRO_HIST the small random values are added
initHist :: Int -> Int -> MyPos -> History -> IO ()
initHist mode sh p h = do
#ifdef REPRO_HIST
    let vs = pstHistVals mode sh p
#else
    g <- newStdGen
    let vs = U.zipWith (+) (pstHistVals mode sh p) $ U.fromListN vsize $ map fromIntegral $ smallVals g
#endif
    U.copy h vs

histOrderStep :: Int -> Int32
histOrderStep sh = 1024 `unsafeShiftL` max 0 (sh + 4)

pstHistVals :: Int -> Int -> MyPos -> U.Vector Int32
pstHistVals mode sh p
    | mode == 0 = U.replicate vsize 0
    | otherwise = U.generate vsize val
    where !gph = gamePhase p
          val i
              | pc <= 5                  = pieceVal pc
              | pc == 7 && castleTo c sq = pieceVal 5
              | otherwise                = 0
              where (c, r)   = i `divMod` bloff
                    (pc, sq) = r `divMod` squares
                    pieceVal f = ordVal f + pstVal f
                    pstVal f | mode .&. 1 /= 0 = pstHist sh gph c f sq
                             | otherwise       = 0
          ordVal f | mode .&. 2 /= 0 = histOrderStep sh * (pieceOrder `unsafeAt` f)
                   | otherwise       = 0
          castleTo 0 sq = sq == 2  || sq == 6
          castleTo _ sq = sq == 58 || sq == 62

-- The history init value of a piece (0 to 5) of color c (0 white, 1 black) on square sq,
-- black from the vertically mirrored square, as in the PST
pstHist :: Int -> Int -> Int -> Int -> Int -> Int32
pstHist sh gph c pc sq = fromIntegral $ negate $ if sh >= 0 then v `unsafeShiftL` sh else v `shiftR` negate sh
    where !i  = pc * squares + (if c == 0 then sq else sq `xor` 56)
          !dm = pstHMid `unsafeAt` i - pstHMean `unsafeAt` (2 * pc)
          !de = pstHEnd `unsafeAt` i - pstHMean `unsafeAt` (2 * pc + 1)
          !v  = (dm * gph + de * (256 - gph)) `unsafeShiftR` 8

-- Rank of the pieces (pawn .. king) in the move generation order (genMoveNCapt, moveGenAscendent)
pieceOrder :: UArray Int Int32
pieceOrder = listArray (0, 5) [4, 0, 1, 2, 3, 5]

pstHMid, pstHEnd, pstHMean :: UArray Int Int
pstHMid = listArray (0, 383) $ map fst pstValues
pstHEnd = listArray (0, 383) $ map snd pstValues
-- Means (mid, end) per piece over the squares (pawns: only ranks 2 to 7)
pstHMean = listArray (0, 11) $ concatMap mean [0 .. 5]
    where mean pc = [sum (map fst vs) `div` length vs, sum (map snd vs) `div` length vs]
              where vs | pc == 0   = take 48 $ drop 8 $ pcs pc
                       | otherwise = pcs pc
          pcs pc = take squares $ drop (pc * squares) pstValues

-- History value: exponential
-- d is absolute depth, root = 1, so that cuts near root count more
-- For max depth = 20 we assume we will not get absolute depths over 30
-- (But if yes, we don't know what happens when dm is negative)
{-# INLINE histw #-}
histw :: Int -> Int32
histw !d = 1 `unsafeShiftL` dm
    where !dm = max 0 (maxd - d)
          maxd = 31

-- We don't use negative history (i.e. when move did not cut)
toHist :: Int -> History -> Move -> Int -> IO ()
toHist grav h m d = case grav of
    0 -> addHist h (adr m) (histw d)
    1 -> gravHist h (adr m) (negate $ histwc d)
    2 -> addHist h (adr m) (histwc d)
    _ -> if d > 0 then addHist h (adr m) (histw d) else return ()

-- The bonus without the overflow at the root
{-# INLINE histwc #-}
histwc :: Int -> Int32
histwc = histw . max 1

-- The malus for a move which failed low before a cut: the bonus at absolute depth d shifted right by sh
fromHist :: Int -> History -> Int -> Int -> Move -> IO ()
fromHist grav h d sh m
    | grav == 1 = gravHist h (adr m) p
    | otherwise = do
        oh <- V.unsafeRead h (adr m)
        if oh <= highLimit
           then V.unsafeWrite h (adr m) (oh + p)
           else do
               rescaleHist h
               V.unsafeWrite h (adr m) ((oh `unsafeShiftR` 1) + p)
    where !p = histwc d `unsafeShiftR` sh
          highLimit = 1 `unsafeShiftL` 29

-- Gravity: the change gets smaller when the value is already in that direction, |value| stays below 2^30
-- (b < 0 is better, as the values are sorted in increasing order)
gravHist :: History -> Int -> Int32 -> IO ()
gravHist h !ad !b = do
    o <- V.unsafeRead h ad
    let !o64 = fromIntegral o :: Int64
        !b64 = fromIntegral b :: Int64
        !n   = o64 + b64 - (o64 * abs b64) `shiftR` 30
    V.unsafeWrite h ad (fromIntegral n)

addHist :: History -> Int -> Int32 -> IO ()
addHist h !ad !p = do
    oh <- V.unsafeRead h ad
    nh <- if (oh >= lowLimit)
             then return $ oh - p -- we subtract, so that the sort is reverse (big first)
             else do
                 rescaleHist h
                 return $ (oh `unsafeShiftR` 1) - p
    V.unsafeWrite h ad nh
    where lowLimit = - (1 `unsafeShiftL` 30)

-- Rescale the whole history
rescaleHist :: History -> IO ()
rescaleHist h = forM_ [0..vsize-1] $ \i -> do
    o <- V.unsafeRead h i
    V.unsafeWrite h i (o `unsafeShiftR` 1)

-- We use a data structure to allow lazyness for the selection of the next
-- best move (by history values), because we want to use by every selected move
-- the latest history values - we do this except for depth 1, where no history changes
-- are expected to change the order between moves anymore
-- The structure has the history, a quasi zipped vector of move & history address
-- packed in a Word32 and the index of the last valid move in that vector
-- Packing 2x Word16 in a Word32 should be better because GHC implements Word16 as Word,
-- which is simply too big for what we want
type MoveVect = U.Vector Word32		-- move (higher 16 bits), history index (short form)
data MovesToSort = MTS History MoveVect !Int

-- Small helpers to pack and unpack the move & its address:
{-# INLINE moveAddr #-}
moveAddr :: Move -> Int -> Word32
moveAddr (Move w) a = (fromIntegral w `unsafeShiftL` 16) .|. fromIntegral a

{-# INLINE takeAddr #-}
takeAddr :: Word32 -> Int
takeAddr = fromIntegral . ((.&.) 0xFFFF)

{-# INLINE takeMove #-}
takeMove :: Word32 -> Word16
takeMove = fromIntegral . (`unsafeShiftR` 16)

-- We want to alloc a fix amount of memory, and although there are
-- positions which have more (quiet) moves, they are very rare
-- and we will ignore them for now
maxMoves :: Int
maxMoves = 128

-- For remaining depth 1 and 2 we can't have changed history values between the moves
-- For 1: every cut will get better history score, but that move is already out of the remaining list
-- For 2: we are trying moves of the other party, which will not be changed in depth 1
-- So for d==1 or d==2 we make direct sort, which must be very fast, as this is the most effort
-- The question is, if no direct sort is much better than the MTS method - to be tested...
{-# INLINE histSortMoves #-}
histSortMoves :: Int -> History -> [Move] -> [Move]
histSortMoves d h ms
    | null ms   = []
    | d > 2     = mtsList $ makeMTS h ms
    | otherwise = dirSort h ms

-- {-# INLINE makeMTS #-}
makeMTS :: History -> [Move] -> MovesToSort
makeMTS h ms = MTS h uc k
    where uc = U.fromListN maxMoves $ zipWith moveAddr ms $ adrs ms
          k  = U.length uc - 1

-- Transform the structure lazyli to a move list
mtsList :: MovesToSort -> [Move]
mtsList (MTS h uwa k)
    | k <  0    = []		-- no further moves
    | k == 0    = oneMove uwa	-- last move: no history value needed
    | otherwise = m : mtsList mts
    where (m, mts) = runST $ do
          uh <- unsafeIOToST $ U.unsafeFreeze h
          let -- Indirect history value:
              hival = U.unsafeIndex uh . takeAddr . U.unsafeIndex uwa
              -- To find index of min history values
              go !i !i0 !v0
                  | i > k     = i0
                  | otherwise =
                       let v = hival i	-- history value of this position
                       in if v < v0	-- we take minimum coz that trick (bigger is worse)
                             then go (i+1) i  v
                             else go (i+1) i0 v0
          let !v0 = hival 0
              !i = go 1 0 v0
              !w = takeMove $ U.unsafeIndex uwa i		-- this is the (first) best move
          -- Now swap the minimum with the last active element for both vectors
          -- to eliminate the used move (if necessary)
          if i == k	-- when last was best
             then return (Move w, MTS h uwa (k-1))
             else do
                 vwa <- U.unsafeThaw uwa		-- thaw the move/address vector
                 V.unsafeSwap vwa k i			-- swap
                 uwa'<- U.unsafeFreeze vwa		-- freeze again
                 return (Move w, MTS h uwa' (k-1))

{-# INLINE oneMove #-}
oneMove :: MoveVect -> [Move]
oneMove uwa = [ Move $ takeMove $ U.unsafeIndex uwa 0 ]

-- Direct sort, i.e. read the current history values for every move
-- and sort the moves accordigly (take care of the trick!)
-- Here we also combine the history value, which is Int32, with the move itself
-- on 16 bits, in an Int64, in the hope that the sort is quicker
dirSort :: History -> [Move] -> [Move]
dirSort h ms = runST $ do
    uh <- unsafeIOToST $ U.unsafeFreeze h
    let uz = U.fromListN maxMoves $ zipWith hivalMove ms $ map (U.unsafeIndex uh) $ adrs ms
    vz <- U.unsafeThaw uz
    H.sortBy (comparing ((.&.) mask)) vz
    uz' <- U.unsafeFreeze vz
    return $ map (Move . fromIntegral . ((.&.) 0xFFFF)) $ U.toList uz'
    where mask = complement 0xFFFF

hivalMove :: Move -> Int32 -> Int64
hivalMove (Move w) i = (fromIntegral i `unsafeShiftL` 16) .|. fromIntegral w
