module Struct.Status (
    Stats(..),
    MyState(..),
    EvalState(..),
    EvalParams(..)
) where

import Struct.Struct
import Moves.History
import Hash.TransTab

data Stats = Stats {
        nodes :: !Int,
        maxmvs :: !Int
    } deriving Show

data MyState = MyState {
        stack :: [MyPos],	-- stack of played positions
        hash  :: Cache,		-- transposition table
        hist  :: History,	-- history table
        stats :: !Stats,	-- statistics
        evalst :: EvalState	-- eval status (parameter & statistics)
    }

data EvalState = EvalState {
        esDWeightsM :: [Double],
        esDWeightsE :: [Double],
        esIWeightsM :: [Int],
        esIWeightsE :: [Int],
        esEParams   :: EvalParams
    } deriving Show

-- This is the parameter record for characteristics evaluation
data EvalParams
    = EvalParams {
          -- Parameters of the king placement
          epMovingMid  :: !Int,
          epMovingEnd  :: !Int,
          epMaterMinor :: !Int,
          epMaterRook  :: !Int,
          epMaterQueen :: !Int,
          epMaterScale :: !Int,
          epMaterBonusScale :: !Int,
          epPawnBonusScale  :: !Int,
          epPassKingProx    :: !Int,
          epPassBlockO :: !Int,
          epPassBlockA :: !Int,
          epPassMin    :: !Int,
          epPassMyCtrl :: !Int,
          epPassYoCtrl :: !Int
      } deriving Show
