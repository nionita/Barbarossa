{-# LANGUAGE TypeSynonymInstances,
             MultiParamTypeClasses,
             RankNTypes, UndecidableInstances
             #-}

module Moves.BaseTypes (
    Game
) where

import Control.Monad.IO.Class

import Search.CStateMonad (CState)
import Struct.Context
import Struct.Status
import Search.AlbetaTypes

-- This is a specialized monad transformer for state
type Game = CState MyState CtxIO

{--
class (Monad m, MonadIO m) => CtxMon m where
    tellCtx :: Comm -> m ()
    timeCtx :: m Int
--}
