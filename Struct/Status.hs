module Struct.Status (
    Stats(..),
    MyState(..),
    EvalState(..)
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
        esDParams :: [Double],
        esIParams :: [Int]
    } deriving Show
