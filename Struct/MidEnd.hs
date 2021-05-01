{-# LANGUAGE BangPatterns #-}

module Struct.MidEnd (
    MidEnd(..),
    mad, tme
) where

data MidEnd = MidEnd { mid, end :: !Int } deriving Show

-- Helper for MidEnd operations:
{-# INLINE mad #-}
mad :: MidEnd -> Int -> MidEnd -> MidEnd
mad !weight !fact !acc = MidEnd { mid = mid acc + mid weight * fact, end = end acc + end weight * fact }

{-# INLINE tme #-}
tme :: Int -> Int -> MidEnd
tme a b = MidEnd a b
