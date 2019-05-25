{-# LANGUAGE BangPatterns #-}

module Moves.History (
        History, newHist, toHist, histSortMoves
    ) where

import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad.ST
import Control.Monad (forM_)
import Data.Bits
import Data.List (unfoldr)
import Data.Ord (comparing)
import qualified Data.Vector.Algorithms.Heap as H	-- Intro sort was slower
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed         as U
import Data.Int
import Data.Word
import System.Random

import Struct.Struct

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
smallVals :: RandomGen g => g -> [Word32]
smallVals g = concatMap chop $ randoms g
    where chop w = unfoldr f (w, 16)
          f :: (Word32, Int) -> Maybe (Word32, (Word32, Int))
          f (_, 0) = Nothing
          f (w, k) = Just (w .&. 3, (w `unsafeShiftR` 2, k-1))

newHist :: IO History
newHist = do
    g <- newStdGen
    U.thaw $ U.fromList $ map fromIntegral $ take vsize $ smallVals g

-- History value: exponential
-- d is absolute depth, root = 1, so that cuts near root count more
-- For max depth = 20 we assume we will not get absolute depths over 30
-- (But if yes, we don't know what happens when dm is negative)
{-# INLINE histw #-}
histw :: Int -> Int32
histw !d = 1 `unsafeShiftL` dm
    where !dm = maxd - d
          maxd = 31

-- We don't use negative history (i.e. when move did not cut)
toHist :: History -> Move -> Int -> IO ()
toHist h m d = addHist h (adr m) (histw d)

addHist :: History -> Int -> Int32 -> IO ()
addHist h !ad !p = do
    oh <- V.unsafeRead h ad
    nh <- if (oh >= lowLimit)
             then return $ oh - p -- we subtract, so that the sort is reverse (big first)
             else do
                 -- Rescale the whole history
                 forM_ [0..vsize-1] $ \i -> do
                     o <- V.unsafeRead h i
                     V.unsafeWrite h i (o `unsafeShiftR` 1)
                 return $ (oh `unsafeShiftR` 1) - p
    V.unsafeWrite h ad nh
    where lowLimit = - (1 `unsafeShiftL` 30)

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
