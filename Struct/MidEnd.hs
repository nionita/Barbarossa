{-# LANGUAGE BangPatterns #-}

module Struct.MidEnd (
    MidEnd(..),
    mad, madm, made, tme
) where

data MidEnd = MidEnd { mid, end :: !Int } deriving Show

-- Helper for MidEnd operations:
{-# INLINE madm #-}
madm :: MidEnd -> MidEnd -> Int -> MidEnd
madm !mide0 !mide !v = mide0 { mid = mid mide0 + mid mide * v }

{-# INLINE made #-}
made :: MidEnd -> MidEnd -> Int -> MidEnd
made !mide0 !mide !v = mide0 { end = end mide0 + end mide * v }

{-# INLINE mad #-}
mad :: MidEnd -> MidEnd -> Int -> MidEnd
mad !mide0 !mide !v = MidEnd { mid = mid mide0 + mid mide * v, end = end mide0 + end mide * v }

{-# INLINE tme #-}
tme :: Int -> Int -> MidEnd
tme a b = MidEnd a b
