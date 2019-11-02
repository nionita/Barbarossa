{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Struct.Status (
    MyState(..),
    EvalState(..),
    EvalParams(..),
    EvalWeights(..)
) where

import Struct.Struct
import Struct.Config
import Struct.Params
import Struct.MidEnd
import Moves.History
import Hash.TransTab
import Search.AlbetaTypes

$(genEvalParams)
$(genEvalWeights)

data MyState = MyState {
        stack  :: [MyPos],	-- stack of played positions
        hash   :: Cache,	-- transposition table
        hist   :: History,	-- history table
        mstats :: SStats,	-- per move search statistics
        evalst :: EvalState,	-- eval status (parameter & statistics)
        rootmn :: !Int		-- root move number
    }

data EvalState = EvalState {
        esEParams   :: EvalParams,
        esEWeights  :: EvalWeights
    } deriving Show
