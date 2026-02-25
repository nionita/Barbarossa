module Encoded () where

class Orable a where
    bitsize :: a -> Int

class Embed a w where
    embed :: Int -> a -> w
    extract :: Int -> w -> a

class Orable a, Orable b => a :|: b where
