module Moves.ShowMe where

import Data.Array.Unboxed
import Data.Bits
import Data.List
import Data.Word

-- Help functions (good for debug)
printBB :: Word64 -> IO ()
printBB = putStr . showBB

showBB b = unlines $ map showBin
               $ reverse $ take 8 $ iterate (`shiftR` 8) b
    where showBin w = intersperse ' ' $ map sb [ w `testBit` i | i <- [0..7]]
          sb False = '0'
          sb True  = '1'

showc :: UArray Int Char
-- showc = array (0, 15) $ zip [0..] $ ['0' .. '9'] ++ ['A' .. 'F']
showc = array (0, 15) $ zip [0..] ".PNKxBRQ.pnkybrq"

showLine :: Word8 -> Word8 -> Word8 -> Word8 -> String
showLine w1 w2 w3 w4 = go w1 w2 w3 w4 8 ""
    where go :: Word8 -> Word8 -> Word8 -> Word8 -> Int -> String -> String
          go _ _ _ _ 0 cs = cs
          go x y z t n cs
                   = go (x `shift` 1) (y `shift` 1) (z `shift` 1) (t `shift` 1) (n-1) (c:' ':cs)
               where c = showc ! cap x y z t

cap x y z t = fromIntegral $ (x' .|. shiftR y' 1 .|. shiftR z' 2 .|. shiftR t' 3) `shiftR` 4
    where x' = x .&. 0x80
          y' = y .&. 0x80
          z' = z .&. 0x80
          t' = t .&. 0x80

showTab :: Word64 -> Word64 -> Word64 -> Word64 -> String
showTab w1 w2 w3 w4 = go w1 w2 w3 w4 8
    where go _ _ _ _ 0 = ""
          go x y z t n = showLine (byte x) (byte y) (byte z) (byte t) ++ "\n"
                       ++ go (next x) (next y) (next z) (next t) (n-1)
          byte u = fromIntegral $ u `shiftR` 56
          next u = u `shift` 8

printTab w1 w2 w3 w4 = putStr $ showTab w1 w2 w3 w4
