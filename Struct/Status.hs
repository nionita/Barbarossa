module Struct.Status (
    Stats(..),
    MyState(..),
    EvalState(..),
    EvalParams(..)
) where

import Data.Array.Unboxed
import Data.Word

import Struct.Struct
import Moves.History
import Hash.TransTab

data Stats = Stats {
        nodes :: !Int,
        maxmvs :: !Int
    } deriving Show

data MyState = MyState {
        stack :: [MyPos],	-- stack of played positions
        hash  :: !Cache,	-- transposition table
        hist  :: History,	-- history table
        stats :: !Stats,	-- statistics
        evalst :: EvalState	-- eval status (parameter & statistics)
    }

data EvalState = EvalState {
        esDWeights :: [Double],
        esIWeights :: [Int],
        esEParams  :: EvalParams
    } deriving Show

-- This is the parameter record for characteristics evaluation
data EvalParams
    = EvalParams {
          -- Parameters of the king placement
          epMaterMinor :: !Int,	-- = 1
          epMaterRook  :: !Int,	-- = 2
          epMaterQueen :: !Int,	-- = 5
          epMaterScale :: !Int,	-- = 0
          epMaterBonusScale :: Int,	-- 4
          epPawnBonusScale  :: !Int	-- 4
      } deriving Show
