{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Uci.UciGlue (
    bestMoveCont
) where

import Control.Monad.State.Lazy

import qualified Search.CStateMonad as SM
import Search.AlbetaTypes
import Search.Albeta
import Struct.Struct
import Struct.Status
import Struct.Context

-- Parameter of the search at this level:
aspirWindow :: Int
aspirWindow   = 24	-- initial aspiration window

-- One iteration in the search for the best move
bestMoveCont :: Int -> Int -> MyState -> Maybe Int -> [Move] -> [Move] -> CtxIO IterResult
bestMoveCont tiefe sttime stati lastsc lpv rmvs = do
    informGuiDepth tiefe
    ctxLog LogInfo $ "start search for depth " ++ show tiefe
    let abc = ABC {
                maxdepth = tiefe,
                lastpv = lpv,
                lastscore = lastsc,
                rootmvs   = rmvs,
                window    = aspirWindow,
                best      = False,
                stoptime  = sttime
                }
    ((sc, path, rmvsf, timint), statf) <- SM.runCState (alphaBeta abc) stati
    when (sc == 0) $ return ()
    let n = nodes . stats $ statf
    informGui sc tiefe n path
    ctxLog LogInfo $ "score " ++ show sc ++ " path " ++ show path
    return (path, sc, rmvsf, timint, statf)
