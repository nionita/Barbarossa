{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Uci.UciGlue (
    bestMoveCont
) where

import Control.Monad (when)

import qualified Search.CStateMonad as SM
import Search.AlbetaTypes
import Search.Albeta
import Struct.Struct
import Struct.Status
import Struct.Context

-- Parameter of the search at this level:
aspirWindow :: Int
aspirWindow = 24	-- initial aspiration window

-- One iteration in the search for the best move
bestMoveCont :: Int -> Int -> Int -> Int -> MyState -> Maybe Int -> [Move] -> [Move] -> CtxIO IterResult
bestMoveCont depth sttime tnr mcs stati lastsc lpv rmvs = do
    when (tnr <= 1) $ do
        informGuiDepth depth
        ctxLog tnr LogInfo $ "Main thread: start search for depth " ++ show depth
    let abc = ABC {
                maxdepth  = depth,
                lastpv    = lpv,
                lastscore = lastsc,
                rootmvs   = rmvs,
                window    = aspirWindow,
                threadNr  = tnr,
                minCurSe  = mcs,
                stoptime  = sttime
                }
    ((sc, path, rmvsf, timint, ch), statf) <- SM.runCState (alphaBeta abc) stati
    -- when tnr <= 1 $ do
    --     let n = sNodes $ mstats statf
    --     informGui sc depth n path
    --     ctxLog LogInfo $ "Main thread: score " ++ show sc ++ " path " ++ show path
    return (path, sc, rmvsf, timint, statf, ch)
