{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, BangPatterns,
    FlexibleInstances
  #-}
module Uci.UciGlue (
    bestMoveCont
) where

import Data.Array.IArray
import Control.Monad.State.Lazy
import Control.Monad.Reader

import qualified Search.SearchMonad as SM
import Search.AlbetaTypes
import Search.Albeta
import Struct.Struct
import Struct.Status
import Struct.Context
import Moves.Base
import Eval.Eval

instance CtxMon CtxIO where
    tellCtx = talkToContext
    timeCtx = do
        ctx <- ask
        let refs = startSecond ctx
        lift $ currMilli refs

-- Parameter of the search at this level:
aspirWindow :: Int
aspirWindow   = 24	-- initial aspiration window

showEvalStats :: Bool
showEvalStats = False	-- show eval statistics in logfile

-- One iteration in the search for the best move
bestMoveCont :: Int -> Int -> MyState -> Maybe Int -> [Move] -> [Move] -> CtxIO IterResult
bestMoveCont tiefe sttime stati lastsc lpv rmvs = do
    -- ctx <- ask
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
    ((sc, path, rmvsf), statf) <- SM.runSearch (alphaBeta abc) stati
    when (sc == 0) $ return ()
    let n = nodes . stats $ statf
    informGui sc tiefe n path
    ctxLog LogInfo $ "score " ++ show sc ++ " path " ++ show path
    return (path, sc, rmvsf, statf)

talkToContext :: Comm -> CtxIO ()
talkToContext (LogMes s)       = ctxLog LogInfo s
talkToContext (BestMv a b c d) = informGui a b c d
talkToContext (CurrMv a b)     = informGuiCM a b
talkToContext (InfoStr s)      = informGuiString s
