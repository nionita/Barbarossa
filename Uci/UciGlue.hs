{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Uci.UciGlue (
    bestMoveCont
) where

-- import Control.Monad.State.Lazy

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
bestMoveCont draft sttime stati lastsc lpv rmvs = do
    informGuiDepth draft
    ctxLog LogInfo $ "start search for depth " ++ show draft
    let abc = ABC {
                maxdepth = draft,
                lastpv = lpv,
                lastscore = lastsc,
                rootmvs   = rmvs,
                window    = aspirWindow,
                stoptime  = sttime
            }
    ((sc, path, rmvsf, timint, ch), statf) <- SM.runCState (alphaBeta abc) stati
    let n = sNodes $ mstats statf
    informGui sc draft n path
    ctxLog LogInfo $ "score " ++ show sc ++ " path " ++ show path
    return (path, sc, rmvsf, timint, statf, ch)
