module Moves.BaseTypes (
    Game
) where

import Search.CStateMonad (CState)
import Struct.Context
import Struct.Status

-- This is a specialized monad transformer for state
type Game = CState MyState CtxIO
