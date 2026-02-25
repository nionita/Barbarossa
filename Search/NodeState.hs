module Search.NodeState () where

import Data.Bits

data NodeState
    = NSt {
          nstco  :: !Word,	-- encodes node types, movno, spcno, albe and score type
          cursc  :: Path,	-- current alpha value (now plus path & depth)
          killer :: Killer,	-- the current killer moves
          cpos   :: MyPos	-- current position for this node
      } deriving Show

-- Get/Set functions for the encoded nstco field of NodeState

-- Movno is encoded on 9 bits: 0 to 8
-- Operations: reset to 1, increment, read and read difference to spcno
-- When we reset movno to 1 we also reset spcno to 1 (bits 9 to 17)
{-# INLINE resetMovno #-}
resetMovno :: Word -> Word
resetMovno = setBits 0 16 0x201

-- We don't check for overflow as we can't have more than 511 moves
{-# INLINE incrementMovno #-}
incrementMovno :: Word -> Word
incrementMovno w = w + 1

{-# INLINE readMovno #-}
readMovno :: Word -> Int
readMovno = fromIntegral . getBits 0 9

{-# INLINE readMovDiff #-}
readMovDiff :: Word -> Int
readMovDiff w = m - s
    where m = readMovno w
          s = fromIntegral $ getBits 9 9

-- Spcno is the number of last special move (i.e. capture or promotion)
-- We encode it on 9 bits from 9 to 17
-- Operations: reset to movno
{-# INLINE resetSpcno #-}
resetSpcno :: Word -> Word
resetSpcno w = setBits 9 9 (getBits 0 9 w) w

-- NodeTypes are encoded on bits 18 to 19
{-# INLINE getNodeTypes #-}
getNodeTypes :: Word -> Word
getNodeTypes = getBits 18 2

{-# INLINE setNodeTypes #-}
setNodeTypes :: Word -> Word -> Word
setNodeTypes = setBits 18 2

-- The flag albe is in bit 20
-- Operations: read and set to true
{-# INLINE getAlbe #-}
getAlbe :: Word -> Bool
getAlbe = ((/=) 0) . getBits 20 1

{-# INLINE setAlbe #-}
setAlbe :: Word -> Word
setAlbe = setBits 20 1 1

-- The Score Type can be 0, 1 or 2 and is encoded in bits 21-22
-- Operations: set value, read
{-# INLINE getScoreType #-}
getScoreType :: Word -> Int
getScoreType = fromIntegral . getBits 21 2

{-# INLINE setScoreType #-}
setScoreType :: Int -> Word -> Word
setScoreType v = setBits 21 2 (fromIntegral v)

{-# INLINE getBits #-}
getBits :: Int -> Int -> Word -> Word
getBits offset width w = b
    where mask = unsafeShiftL (bit width - 1) offset
          b = unsafeShiftR (w .&. mask) offset

{-# INLINE setBits #-}
setBits :: Int -> Int -> Word -> Word -> Word
setBits offset width v w = b
    where mask = complement $ unsafeShiftL (bit width - 1) offset
          b = (w .&. mask) .|. unsafeShiftL v offset
