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
aspirWindow   = 24	-- initial aspiration window

-- One iteration in the search for the best move
bestMoveCont :: Int -> Int -> Bool -> MyState -> Maybe Int -> [Move] -> [Move] -> CtxIO IterResult
bestMoveCont depth sttime main stati lastsc lpv rmvs = do
    when main $ do
        informGuiDepth depth
        ctxLog LogInfo $ "start search for depth " ++ show depth
    let abc = ABC {
                maxdepth  = depth,
                lastpv    = lpv,
                lastscore = lastsc,
                rootmvs   = rmvs,
                window    = aspirWindow,
                mainThrd  = main,
                stoptime  = sttime
                }
    ((sc, path, rmvsf, timint, ch), statf) <- SM.runCState (alphaBeta abc) stati
    when main $ do
        let n = sNodes $ mstats statf
        informGui sc depth n path
        ctxLog LogInfo $ "score " ++ show sc ++ " path " ++ show path
    return (path, sc, rmvsf, timint, statf, ch)
