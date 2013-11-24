{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
module Hash.OtherTT (
    ACache, newACache, readACache, writeACache
    -- , checkProp
    ) where

-- For other caches (for king safety, pawn structure) we use a smaller and simpler
-- cache with one entry slots, with immediate replace

import Data.Bits
import Data.Maybe (fromMaybe)
import Data.Int
import Data.Word
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
-- import Test.QuickCheck hiding ((.&.))

import Struct.Struct

type Index = Int
type Mask = Word64

-- The data type ACell is a general cell for the zkey and the user data
data ACell a = ACell !ZKey !a

zKeySize = sizeof (undefined :: ZKey)

-- actually we don't use this instance, do we?
instance Storable a => Storable (ACell a) where
    sizeOf c    = zKeySize + sizeOf c
    alignment _ = 4
    {-# INLINE peek #-}
    peek e      = let qw = castPtr e
                      qa = cellToA
                  in do zk <- peek qw
                        a  <- peek qa
                        return $! ACell zk a
    {-# INLINE poke #-}
    poke e (ACell zk a)
                = let qw = castPtr e
                      qa = cellToA
                  in do poke qw zk
                        poke qa a

data ACache a = ACache {
          mem 	 :: Ptr (ACell a),	-- the cache line aligned byte array for the data
          lomask :: Word64,		-- the mask for the index
          eshift :: Int			-- shift amount to count for the entry size
      }

-- Create a new cache with a number of entries
-- corresponding to the given (integral) number of MB
-- The number of entries will be rounded up to the next power of 2
newACache :: Int -> IO (ACache a)
newACache mb = do
    let s = nextPowOf2 $ sizeOf (undefined :: ACell a)
        c = mb * 1024 * 1024 `div` s
        ne  = nextPowOf2 c
        lom = fromIntegral $ ne - 1
        es  = popCount $ s - 1
    memc <- mallocArray ne
    return ACache { mem = memc, lomask = lom, eshift = es }

-- This computes the adress of an entry given the key
zKeyToIndex :: ACache a -> ZKey -> Ptr ZKey
zKeyToIndex tt zkey = ptr
    where idx = fromIntegral $ zkey .&. lomask tt
          -- This is the wanted calculation:
          -- base = mem tt `plusPtr` (cell * sizeOf Cell)
          -- NB: plusPtr advances Bytes!
          -- And this is how it is done efficiently:
          !ptr = mem tt `plusPtr` (idx `unsafeShiftL` eshift tt)

{-# INLINE cellToA #-}
cellToA :: Ptr ZKey -> Ptr a
cellToA ptr = castPtr $ ptr `plusPtr` zKeySize

-- Read user data in the cache given the ZKey
readACache :: ACache a -> ZKey -> IO (Maybe a)
readACache tt zkey = do
    let ptr = zKeyToIndex tt zkey
    ttkey <- peek ptr
    if ttkey /= zkey
       then return Nothing
       else Just <$> peek (cellToA ptr)

writeACache :: ACache a -> ZKey -> a -> IO ()
writeACache tt zkey a = do
    let ptr = zKeyToIndex tt zkey
    poke ptr zkey
    poke (cellToA ptr) a

nextPowOf2 :: Int -> Int
nextPowOf2 x = bit (l - 1)
    where pow2s = iterate (* 2) 1
          l = length $ takeWhile (<= x) pow2s