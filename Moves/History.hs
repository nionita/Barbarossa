{-# LANGUAGE BangPatterns #-}

module Moves.History (
        History,
        newHist, swiHist,
        toHist, histSortMoves
    ) where

import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad.ST
import Data.Bits
import Data.Ord (comparing)
import qualified Data.Vector.Algorithms.Heap as H	-- Intro sort was slower
import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed         as U
import Data.Int
import Data.Word

import Struct.Struct

-- Int32 should be enough for now with max depth 20
data History = History !(U.Vector Int32) !(V.IOVector Int32)

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

-- The first "old" history is all zeros
zeroHist :: U.Vector Int32
zeroHist = U.replicate vsize 0

-- We initialize a new history with the max mix draft, coz we know the old one is useless
newHist :: IO History
newHist = History zeroHist <$> V.replicate vsize 0

swiHist :: History -> IO History
swiHist (History _ v) = do
    u <- U.unsafeFreeze v
    w <- V.replicate vsize 0
    return $! History u w

{-# INLINE histw #-}
histw :: Int -> Int32
histw !d = 1 `unsafeShiftL` dm
    where !dm = maxd - d
          maxd = 20

toHist :: History -> Bool -> Move -> Int -> IO ()
toHist (History _ h) True  m d = addHist h (adr m) (histw d)
toHist (History _ h) False m d = subHist h (adr m) (histw d)

{--
{-# INLINE valHist #-}
valHist :: History -> Move -> IO Int32
valHist !h = V.unsafeRead h . adr
--}

addHist :: V.IOVector Int32 -> Int -> Int32 -> IO ()
addHist h !ad !p = do
    a <- V.unsafeRead h ad
    let !u = a - p	-- trick here: we subtract, so that the sort is big to small
        !v = if u < lowLimit then lowHalf else u
    V.unsafeWrite h ad v
    where lowLimit = -1000000000
          lowHalf  =  -500000000

subHist :: V.IOVector Int32 -> Int -> Int32 -> IO ()
subHist h !ad !p = do
    a <- V.unsafeRead h ad
    let !u = a + p	-- trick here: we add, so that the sort is big to small
        !v = if u > higLimit then higHalf else u
    V.unsafeWrite h ad v
    where higLimit = 1000000000
          higHalf  =  500000000

-- We use a data structure to allow lazyness for the selection of the next
-- best move (by history values), because we want to use for every selected move
-- the latest history values - we do this except for depths 1 and 2, where no history changes
-- are expected to change the order between moves anymore
-- We use the new history, but when the values are equal, the old history values
-- will be consulted
-- The structures has the old and new vector, a zipped vector of move (as Word16 part) and
-- history address and the index of the last valid move in that vector
type MoveVect = U.Vector (Word16, Word16)	-- move, history index (short form)
data MovesToSort = MTS (U.Vector Int32) (V.IOVector Int32) MoveVect !Int

-- We want to alloc a fix amount of memory, and although there are maybe
-- positions which have more (quiet) moves, they must be very rare
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
histSortMoves d (History hold hnew) ms
    | null ms   = []
    | d > 2     = mtsList $ makeMTS hold hnew ms
    | otherwise = dirSort hnew ms	-- direct sort has no fall back (for now)

{-# INLINE makeMTS #-}
makeMTS :: U.Vector Int32 -> V.IOVector Int32 -> [Move] -> MovesToSort
makeMTS ho hn ms = MTS ho hn (U.zip uw ua) k
    where uw = U.fromListN maxMoves $ map (\(Move w) -> w) ms
          ua = U.fromListN maxMoves $ map fromIntegral $ adrs ms
          k  = U.length uw - 1

-- Transform the structure lazyli to a move list
mtsList :: MovesToSort -> [Move]
mtsList (MTS ho hn uwa k)
    | k <  0    = []		-- no further moves
    | k == 0    = oneMove uwa	-- last move: no history value needed
    | otherwise = m : mtsList mts
    where (m, mts) = runST $ do
          uh <- unsafeIOToST $ U.unsafeFreeze hn
          let (uw, ua) = U.unzip uwa
              -- Indirect history value with fall back to old value:
              hival i = let !a = fromIntegral $ U.unsafeIndex ua i
                            !vn = U.unsafeIndex uh a
                        in (vn, a)
              -- To find index of min history values
              go !i !i0 !v0n !v0o
                  | i > k     = i0
                  | otherwise =
                       let (!vn, !a) = hival i	-- history values of move i
                       in if vn > v0n	-- we take minimum coz that trick (bigger is worse)
                             then go (i+1) i0 v0n v0o
                             else let !vo = U.unsafeIndex ho a
                                  in if vn < v0n
                                        then go (i+1) i vn vo
                                        else if vo <= v0o	-- new history values are equal
                                                then go (i+1) i  vn  vo
                                                else go (i+1) i0 v0n v0o
          let (!v0n, !a) = hival 0
              !v0o = U.unsafeIndex ho a
              !i = go 1 0 v0n v0o
              !w = U.unsafeIndex uw i		-- this is the (first) best move
          -- Now swap the minimum with the last active element for both vectors
          -- to eliminate the used move (if necessary)
          if i == k	-- when last was best
             then return (Move w, MTS ho hn uwa (k-1))
             else do
                 vw <- U.unsafeThaw uw			-- thaw the move vector
                 va <- U.unsafeThaw ua			-- thaw the history address vector
                 V.unsafeSwap vw k i
                 V.unsafeSwap va k i
                 uw' <- U.unsafeFreeze vw
                 ua' <- U.unsafeFreeze va
                 return (Move w, MTS ho hn (U.zip uw' ua') (k-1))

{-# INLINE oneMove #-}
oneMove :: MoveVect -> [Move]
oneMove uwa = [ Move $ U.unsafeIndex uw 0 ]
    where (uw, _) = U.unzip uwa

-- Direct sort, i.e. read the current history values for every move
-- and sort the moves accordigly (take care of the trick!)
dirSort :: V.IOVector Int32 -> [Move] -> [Move]
dirSort h ms = runST $ do
    uh <- unsafeIOToST $ U.unsafeFreeze h
    let uw = U.fromListN maxMoves $ map (\(Move w) -> w) ms
        uv = U.fromListN maxMoves $ map (U.unsafeIndex uh) $ adrs ms
        uz = U.zip uw uv
    vz <- U.unsafeThaw uz
    H.sortBy (comparing snd) vz
    uz' <- U.unsafeFreeze vz
    let (uw', _) = U.unzip uz'
    return $ map Move $ U.toList uw'
