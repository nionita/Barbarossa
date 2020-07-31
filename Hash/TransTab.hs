{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
module Hash.TransTab (
    Cache, newCache, freeCache, retrieveEntry, readCache, writeCache, newGener,
    -- checkProp
    ) where

-- import Control.Applicative ((<$>))
import Data.Bits
import Data.Int
import Data.Word
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc (free)
import Foreign.Storable
import Foreign.Ptr
import Data.Text.Unsafe (inlinePerformIO)
-- import Test.QuickCheck hiding ((.&.))

import GHC.Exts

import Struct.Struct

-- type Index = Int
type Mask = Word64

cacheLineSize :: Int
cacheLineSize = 64	-- this should be the size in bytes of a memory cache line on modern processors

-- The data type Cell and its Storable instance is declared only for alignement purposes
-- The operations in the cell are done on PCacheEn elements
data Cell

instance Storable Cell where
    sizeOf _    = cacheLineSize
    alignment _ = cacheLineSize
    peek _      = return undefined
    poke _ _    = return ()

data Cache
    = Cache {
          mem 	 :: Ptr Cell,	-- the cache line aligned byte array for the data
          lomask, mimask,
          zemask :: !Mask,	-- masks depending on the size (in entries) of the table
          gener  :: !Word64	-- the generation of the current search
      }

data PCacheEn = PCacheEn { hi, lo :: {-# UNPACK #-} !Word64 }	-- a packed TT entry

pCacheEnSize :: Int
pCacheEnSize  = 2 * sizeOf (undefined :: Word64)	-- i.e. 16 bytes

instance Storable PCacheEn where
    sizeOf _    = pCacheEnSize
    alignment _ = alignment  (undefined :: Word64)
    {-# INLINE peek #-}
    peek p = let !q = castPtr p
             in do wh <- peek  q	-- high (with zkey) first
                   wl <- peek (q `plusPtr` 8)
                   return PCacheEn { lo = wl, hi = wh }
    {-# INLINE poke #-}
    poke p (PCacheEn { lo = wl, hi = wh })
           = let !q = castPtr p
             in do poke  q              wh
                   poke (q `plusPtr` 8) wl

{--
A packed cache entry consists of 2 Word64 parts (the order of the bit fields is fixed):
- word 1 (high) contains (ttbitlen is the number of bits to represent the table length in cells,
  i.e. for 2^18 cells, ttbitlen = 18):
	- part 1 of the ZKey: - the first (64 - ttbitlen - 2) higher bits of the ZKey
	- unused bits - variable length depending on number of tt entries (= ttbitlen - 16)
	- score - 16 bits
	- part 3 of the ZKey: the last 2 bits
- word 2 (low) contains (new):
	- generation - 8 bits
	- node type - 2 bit: exact = 2, lower = 1, upper = 0
	- depth -  6 bits
	- nodes - 32 bits
	- move  - 16 bits
It results that anyway the number of entries in the table must be at least 2^18
(i.e. 2^16 cells with 4 entries each), in which case the "unused bits" part is empty (0 bits).
Part 2 of the ZKey is the cell number where the entry resides.

These are fields of the word 1 and the masks that we keep (here for minimum of 2^18 entries):
|6   5    2         1         0|
|32109...1098765432109876543210|
|<--part 1--><----score-----><>|
|            <-----lomask----->|    lomask and mimask cover also the unused bits, if any
|...      ...<-----mimask--->..|    zemask is the complement of mimask
--}

part3Mask :: Mask
part3Mask = 0x03 :: Mask	-- the cell has 4 entries (other option: 8)

minEntries :: Int
minEntries = 0x40000	-- 2 ^ 18

-- Create a new transposition table with a number of entries
-- corresponding to the given (integral) number of MB
-- The number of entries will be rounded up to the next power of 2
newCache :: Int -> IO Cache
newCache mb = do
    let c = mb * 1024 * 1024 `div` pCacheEnSize
        nentries = max minEntries $ nextPowOf2 c
        ncells   = nentries `div` 4	-- 4 entries per cell
        lom      = fromIntegral $ nentries - 1
        mim      = lom .&. cellMask
    memc <- mallocArray ncells
    return Cache { mem = memc, lomask = lom, mimask = mim, zemask = complement mim, gener = 0 }
    where cellMask = complement part3Mask	-- for speed we keep both masks

freeCache :: Cache -> IO ()
freeCache = free . mem

generInc, generMsk :: Word64
generInc = 0x0100000000000000	-- 1 in first byte
generMsk = 0xFF00000000000000	-- to mask all except generation

-- Increase the generation by 1 for a new search, it wraps automatically, beeing in higherst 8 bits
newGener :: Cache -> Cache
newGener c = c { gener = gener c + generInc }

{--
-- This computes the adress of the first entry of the cell where an entry given by the key
-- should be stored, and the (ideal) index of that entry
-- The (low) mask of the transposition table is also used - this determines the size of the index
zKeyToCellIndex :: Cache -> ZKey -> (Ptr PCacheEn, Index)
zKeyToCellIndex tt zkey = (base, idx)
    where idx = fromIntegral $ zkey .&. lomask tt
          -- This is the wanted calculation:
          -- cell = idx `unsafeShiftR` 2				-- 2 because we have 4 entries per cell
          -- base = mem tt `plusPtr` (cell * sizeOf Cell)
          -- NB: plusPtr advances Bytes!
          -- And this is how it is done efficiently:
          -- idx is entry number, we want cell number: 4 entries per cell ==> shiftR 2
          -- plusPtr needs bytes, 16 bytes/entry * 4 entries/cell = 64 bytes/cell ==> shiftL 6
          !base = mem tt `plusPtr` ((idx `unsafeShiftR` 2) `unsafeShiftL` 6)

-- Retrieve the ZKey of a packed entry
getZKey :: Cache -> Index -> PCacheEn -> ZKey
getZKey tt !idx (PCacheEn {hi = w1}) = zkey
    where !zkey =  w1   .&. zemask tt	-- the first part of the stored ZKey
               .|. widx .&. mimask tt	-- the second part of the stored ZKey
          widx = fromIntegral idx
--}

-- This computes the adress of the first entry of the cell where an entry given by the key
-- should be stored - see the remarks from zKeyToCellIndex for the calculations
zKeyToCell :: Cache -> ZKey -> Ptr Word64
zKeyToCell tt zkey = base
    where idx = fromIntegral $ zkey .&. lomask tt
          !base = mem tt `plusPtr` ((idx `unsafeShiftR` 2) `unsafeShiftL` 6)

-- Faster check if we have the same key under the pointer
-- When we come to check this, we went through zKeyToCell with the sought key
-- Which means, we are already in the correct cell, so the mid part of the key
-- (which in TT is overwritten by the score) will be all time the same (zkey .&. lomask tt)
-- In this case we can mask the mid part of the sought key once we start the search
-- and in the search itself (up to 4 times) we have just to mask that part too,
-- and compare the result with the precomputed sought key
-- So we don't need to reconstruct the original key (which would be more expensive)
isSameKey :: Word64 -> Word64 -> Ptr Word64 -> Bool
isSameKey !mmask !mzkey !ptr =
    let w = inlinePerformIO $ peek ptr
    in w .&. mmask == mzkey

-- Search a position in table based on ZKey
-- The position ZKey determines the cell where the TT entry should be, and there we do a linear search
-- (i.e. 4 comparisons in case of a miss)
readCache :: Addr# -> IO (Maybe (Int, Int, Int, Move, Int64))
#if __GLASGOW_HASKELL__ >= 708
readCache addr = if isTrue# (eqAddr# addr nullAddr#)
#else
readCache addr = if         (eqAddr# addr nullAddr#)
#endif
                    then return Nothing
                    else Just . cacheEnToQuint <$> peek (castPtr (Ptr addr))

retrieveEntry :: Cache -> ZKey -> Addr#
retrieveEntry tt zkey =
    let bas   = zKeyToCell tt zkey
        mkey  = zkey .&. zemask tt
        lasta = bas `plusPtr` lastaAmount
    in retrieve (zemask tt) mkey lasta bas
    where retrieve mmask mkey lasta = go
              where go !crt0@(Ptr a)
                        = if isSameKey mmask mkey crt0
                             then a
                             else if crt0 >= lasta
                                     then nullAddr#
                                     else go $ crt0 `plusPtr` pCacheEnSize

-- Write the position in the table
-- We want to keep table entries that:
-- + are from the same generation, or
-- + have more nodes behind (from a previous search), or
-- + have been searched deeper, or
-- + have a more precise score (node type 2 before 1 and 0)
-- That's why we choose the order in second word like it is (easy comparison)
-- Actually we always search in the whole cell in the hope to find the zkey and replace it
-- but also keep track of the weakest entry in the cell, which will be replaced otherwise
writeCache :: Cache -> ZKey -> Int -> Int -> Int -> Move -> Int64 -> IO ()
writeCache !tt !zkey !depth !tp !score !move !nodes = do
    let bas   = zKeyToCell tt zkey
        gen   = gener tt
        pCE   = quintToCacheEn tt zkey depth tp score move nodes
        mkey  = zkey .&. zemask tt
        lasta = bas `plusPtr` lastaAmount
    store gen (zemask tt) mkey pCE lasta bas bas maxBound
    where store !gen !mmask !mkey !pCE !lasta = go
              where go !crt0 !rep0 !sco0
                       = if isSameKey mmask mkey crt0
                            then poke (castPtr crt0) pCE	 -- here we found the same entry: just update (but depth?)
                            else do
                                lowc <- peek (crt0 `plusPtr` 8)	-- take the low word
                                scoreReplaceLow gen lowc crt0 rep0 sco0
                                    (\r s -> if crt0 >= lasta
                                                then poke (castPtr r) pCE
                                                else go (crt0 `plusPtr` pCacheEnSize) r s)

lastaAmount :: Int
lastaAmount = 3 * pCacheEnSize	-- for computation of the last address in the cell

-- Here we implement the logic which decides which entry is weaker
-- the low word is the score (when the move is masked away):
-- generation (when > curr gen: whole score is 0)
-- type (2 - exact - only few entries, PV, 1 - lower bound: have good moves, 0 - upper bound)
-- depth
-- nodes
-- There is no premature termination of the loop, except when we find the exact key,
-- but this case is handled in the local go function of writeCache
scoreReplaceLow :: Word64 -> Word64 -> Ptr Word64 -> Ptr Word64 -> Word64
    -> (Ptr Word64 -> Word64 -> IO ())	-- continuation function
    -> IO ()
scoreReplaceLow gen lowc crt rep sco cont
    | cell_gen /= gen = cont crt 0		-- replace: not same generation
    | cell_sco <  sco = cont crt cell_sco	-- replace: worse score
    | otherwise  = cont rep sco
    where cell_gen = lowc .&. generMsk
          cell_sco = lowc .&. 0xFFFFFFFFFFFF0000	-- mask the move to get the cell score

quintToCacheEn :: Cache -> ZKey -> Int -> Int -> Int -> Move -> Int64 -> PCacheEn
quintToCacheEn !tt !zkey !depth !tp !score !(Move move) !nodes = pCE
    where w1 =   (zkey .&. zemask tt)
             .|. fromIntegral ((score .&. 0xFFFF) `unsafeShiftL` 2)
          w2 = gener tt
             .|. (fromIntegral tp    `unsafeShiftL` 54)
             .|. (fromIntegral depth `unsafeShiftL` 48)
             .|. (fromIntegral nodes `unsafeShiftL` 16)
             .|. (fromIntegral move)
          !pCE = PCacheEn { hi = w1, lo = w2 }

cacheEnToQuint :: PCacheEn -> (Int, Int, Int, Move, Int64)
cacheEnToQuint (PCacheEn { hi = w1, lo = w2 }) = (de, ty, sc, Move mv, no)
    where u32  = fromIntegral (w1 `unsafeShiftR` 2) :: Word32
          s16  = fromIntegral (u32 .&. 0xFFFF) :: Int16
          !sc  = fromIntegral s16
          !no  = fromIntegral $ (w2 `unsafeShiftR` 16) .&. 0xFFFFFFFF
          w2lo = fromIntegral w2 :: Word32
          !mv  = fromIntegral $ w2lo .&. 0xFFFF
          w2hi = fromIntegral   (w2   `unsafeShiftR` 32) :: Word32
          !de  = fromIntegral $ (w2hi `unsafeShiftR` 16) .&. 0x3F
          !ty  = fromIntegral $ (w2hi `unsafeShiftR` 22) .&. 0x3
          -- perhaps is not a good idea to make them dependent on each other
          -- this must be tested and optimised for speed

nextPowOf2 :: Int -> Int
nextPowOf2 x = bit (l - 1)
    where pow2s = iterate (* 2) 1
          l = length $ takeWhile (<= x) pow2s

{--
dumpTT :: Cache -> String -> IO ()
dumpTT tt fname = withFile fname WriteMode $ \h -> do
    forM [0 .. lomask tt] $ \idx -> do
        let adr = mem tt `plusPtr` idx * 16
        ent <- peek adr
        let zk = getZKey tt idx ent
            (de, ty, sc, mv, no) = cacheEnToQuint ent
        putStrLn $ show idx ++ ": (" ++ showHex (hi ent) . showHex (lo ent)
                     (show zk ++ " " ++ show de ++ " " ++ show ty ++ " " ++ show sc
                         ++ " " ++ show mv ++ " " ++ show no)
--}

----------- Test in IO -------------
{--
testIt :: IO Cache
testIt = do
    tt <- newCache 32
    let z = 118896
    putStrLn $ "tt = " ++ show (mem tt)
    putStrLn $ "z = " ++ show z
    putStrLn "Write: 5 2 124 (Move 364) 123456"
    writeCache tt z 5 2 124 (Move 364) 123456
    putStrLn "Read:"
    let ptr = retrieveEntry tt z
    mr <- readCache ptr
    putStrLn $ show mr
    return tt

----------- QuickCheck -------------
newtype Quint = Q (Int, Int, Int, Move, Int) deriving Show

mvm :: Word16
mvm = (1 `shiftL` 16) - 1

instance Arbitrary Quint where
    arbitrary = do
        sc <- choose (-20000, 20000)
        ty <- choose (0, 2)
        de <- choose (0, 63)
        mv <- arbitrary `suchThat` (<= mvm)
        no <- arbitrary `suchThat` (>= 0)
        return $ Q (de, ty, sc, Move mv, no)

prop_Inverse :: Cache -> ZKey -> Int -> Quint -> Bool
prop_Inverse tt zkey _ (Q q@(de, ty, sc, mv, no))	-- unused: gen
    = q == cacheEnToQuint (quintToCacheEn tt zkey de ty sc mv no)

checkProp :: IO ()
checkProp = do
    tt <- newCache 128
    let zkey = 0
        gen  = 0 :: Int
    putStrLn $ "Fix zkey & gen: " ++ show zkey ++ ", " ++ show gen
    -- quickCheck $ prop_Inverse tt zkey gen
    verboseCheck $ prop_Inverse tt zkey gen
    putStrLn $ "Arbitrary zkey, fixed gen = " ++ show gen
    -- quickCheck $ \z -> prop_Inverse tt z gen
    verboseCheck $ \z -> prop_Inverse tt z gen
--}
