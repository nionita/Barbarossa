{-# LANGUAGE BangPatterns #-}

module Search.AlbetaTypes (
    DoResult(..),
    Comm(..),
    ABControl(..)
) where

import Struct.Struct

data ABControl = ABC {
        maxdepth  :: Int,
        lastpv    :: [Move],
        lastscore :: Maybe Int,
        rootmvs   :: [Move],
        window    :: Int,
        best      :: Bool,
        stoptime  :: Int
    } deriving Show

data DoResult = Exten !Int !Bool	-- return mit extension & special
              | Final !Int	-- return with a final score (probably draw)
              | Illegal		-- illegal move

data Comm = LogMes String
          | BestMv Int Int Int [Move]
          | CurrMv Move Int
          | InfoStr String
