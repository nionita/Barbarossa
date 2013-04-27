{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Search.AlbetaTypes (
    Node(..),
    DoResult(..),
    Comm(..),
    ABControl(..)
) where

import Control.Monad

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

-- The node class, dependent on a game monad m
class Monad m => Node m where
    staticVal :: m Int  -- static evaluation of a node
    materVal  :: m Int  -- material evaluation (for prune purpose)
    genEdges :: Int -> Int -> Bool -> m ([Move], [Move])  -- generate all legal edges
    genTactEdges :: m [Move]  -- generate all edges in tactical positions
    legalEdge :: Move -> m Bool	-- is the move legal?
    killCandEdge :: Move -> Move -> m Bool	-- is the move killer candidate?
    inSeq :: Move -> Move -> m Bool	-- can 2 moves be in sequence?
    tactical :: m Bool -- if a position is tactical, search further
    doEdge   :: Move -> Bool -> m DoResult
    undoEdge :: m ()
    betaMove :: Bool -> Int -> Int -> Move -> m ()   -- called for beta-cut moves
    nullEdge :: m ()		   -- do null move (and also undo)
    retrieve :: m (Int, Int, Int, Move, Int)   -- retrieve the position in hash
    store :: Int -> Int -> Int -> Move -> Int -> m () -- store the position in hash
    curNodes :: m Int
    inform :: Comm -> m ()		-- communicate to the world (log, current and best move)
    choose :: Bool -> [(Int, [Move])] -> m (Int, [Move])
    timeout  :: Int -> m Bool	-- check if we have to abort because of time

data DoResult = Exten !Int	-- return mit extension (evtl 0)
              | Final !Int	-- return with a final score (probably draw)
              | Illegal		-- illegal move

data Comm = LogMes String
          | BestMv Int Int Int [Move]
          | CurrMv Move Int
          | InfoStr String
