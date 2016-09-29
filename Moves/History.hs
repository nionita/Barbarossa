{-# LANGUAGE BangPatterns #-}

module Moves.History (
        History, newHist, toHist, histSortMoves
    ) where

import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed         as U
import Data.Int
import Data.Word

import Struct.Struct

-- Int32 should be enough for now with max depth 20
type History = V.IOVector Int32

rows, cols, vsize :: Int
rows = 12
cols = 64
vsize = rows * cols

-- Did we play with this layout? Could be some other layout better for cache?
{-# INLINE adr #-}
adr :: Move -> Int
adr m = ofs' m + adr' m

-- These functions are used to calculate faster the addresses of a list of moves
-- as they have all the same color, so the offset needs to be calsulated only once
{-# INLINE adrs #-}
adrs :: [Move] -> [Int]
adrs []       = []
adrs ms@(m:_) = map (f . adr') ms
    where off = ofs' m
          f x = off + x

{-# INLINE adr' #-}
adr' :: Move -> Int
adr' m = cols * p + t
    where p = fromEnum $ movePiece m
          t = toSquare m

{-# INLINE ofs' #-}
ofs' :: Move -> Int
ofs' m | moveColor m == White = 0
       | otherwise            = 6 * cols

newHist :: IO History
newHist = V.replicate vsize 0

{-# INLINE histw #-}
histw :: Int -> Int32
histw !d = 1 `unsafeShiftL` dm
    where !dm = maxd - d
          maxd = 20

toHist :: History -> Bool -> Move -> Int -> IO ()
toHist h True  m d = addHist h (adr m) (histw d)
toHist h False m d = subHist h (adr m) (histw d)

{--
{-# INLINE valHist #-}
valHist :: History -> Move -> IO Int32
valHist !h = V.unsafeRead h . adr
--}

addHist :: History -> Int -> Int32 -> IO ()
addHist h !ad !p = do
    a <- V.unsafeRead h ad
    let !u = a - p	-- trick here: we subtract, so that the sort is big to small
        !v = if u < lowLimit then lowHalf else u
    V.unsafeWrite h ad v
    where lowLimit = -1000000000
          lowHalf  =  -500000000

subHist :: History -> Int -> Int32 -> IO ()
subHist h !ad !p = do
    a <- V.unsafeRead h ad
    let !u = a + p	-- trick here: we add, so that the sort is big to small
        !v = if u > higLimit then higHalf else u
    V.unsafeWrite h ad v
    where higLimit = 1000000000
          higHalf  =  500000000

-- We use a data structure to allow lazyness for the selection of the next
-- best move (by history values), because we want to use by every selected move
-- the latest history values - we do this except for depth 1, where no history changes
-- are expected to change the order between moves anymore
-- The structure has the history, a zipped vector of move (as Word16 part) and
-- history address and the index of the last valid move in that vector
type MoveVect = U.Vector (Word16, Word16)	-- move, history index (short form)
data MovesToSort = MTS History MoveVect !Int

-- We want to alloc a fix amount of memory, and although there are
-- positions which have more (quiet) moves, they are very rare
-- and we will ignore them for now
maxMoves :: Int
maxMoves = 128

{-# INLINE histSortMoves #-}
histSortMoves :: History -> [Move] -> [Move]
histSortMoves h ms
    | null ms   = []
    | otherwise = mtsList $ makeMTS h ms

{-# INLINE makeMTS #-}
makeMTS :: History -> [Move] -> MovesToSort
makeMTS h ms = MTS h (U.zip uw ua) k
    where uw = U.fromListN maxMoves $ map (\(Move w) -> w) ms
          ua = U.fromListN maxMoves $ map fromIntegral $ adrs ms
          k  = U.length uw - 1

-- Transform the structure lazyli to a move list
mtsList :: MovesToSort -> [Move]
mtsList (MTS h uwa k)
    | k <  0    = []		-- no further moves
    | k == 0    = oneMove uwa	-- last move: no history value needed
    | otherwise = m : mtsList mts
    where (m, mts) = runST $ do
          uh <- unsafeIOToST $ U.unsafeFreeze h
          let (uw, ua) = U.unzip uwa
              -- Indirect history value:
              hival = U.unsafeIndex uh . fromIntegral . U.unsafeIndex ua
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
              !w = U.unsafeIndex uw i		-- this is the (first) best move
          -- Now swap the minimum with the last active element for both vectors
          -- to eliminate the used move (if necessary)
          if i == k	-- when last was best
             then return (Move w, MTS h uwa (k-1))
             else do
                 vw <- U.unsafeThaw uw			-- thaw the move vector
                 va <- U.unsafeThaw ua			-- thaw the history address vector
                 V.unsafeSwap vw k i
                 V.unsafeSwap va k i
                 uw' <- U.unsafeFreeze vw
                 ua' <- U.unsafeFreeze va
                 return (Move w, MTS h (U.zip uw' ua') (k-1))

{-# INLINE oneMove #-}
oneMove :: MoveVect -> [Move]
oneMove uwa = [ Move $ U.unsafeIndex uw 0 ]
    where (uw, _) = U.unzip uwa
