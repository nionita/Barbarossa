{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Uci.UciGlue (
    bestMoveCont
) where

-- import Control.Monad.State.Lazy
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)

import Moves.History (initHist, clearCont)
import Search.CStateMonad (runCState)
import Search.Core
import Search.AlbetaTypes
import Struct.Struct
import Struct.Status
import Struct.Context

-- Parameter of the search at this level:
aspirWindow :: Int
aspirWindow   = 24	-- initial aspiration window

-- One iteration in the search for the best move
-- The first iteration (draft 1) starts a new search: the history is initialized for the root position
bestMoveCont :: Bool -> Maybe Int -> Int -> Int -> Int -> MyState -> Maybe Int -> [Move] -> [Move] -> CtxIO IterResult
bestMoveCont tune mbNodes draft sttime1 sttime stati lastsc lpv rmvs = do
    case stack stati of
        p:_ -> when (draft == 1) $ liftIO $ do
            when (epCHist ep /= 0) $ clearCont (hist stati)
            initHist (epHistPst ep) (epHistPstSh ep) p (hist stati)
        []  -> return ()
    informGuiDraft draft
    ctxLog LogInfo $ "start search for depth " ++ show draft
    let abortPol | tune      = maybe NoAbort AbortByNodes mbNodes
                 | otherwise = AbortByTime { firstMoveMs = sttime1, laterMovesMs = sttime }
        abc = ABC {
                maxdepth = draft,
                lastpv = lpv,
                lastscore = lastsc,
                rootmvs   = rmvs,
                window    = aspirWindow,
                intuning  = tune,
                abortPolicy = abortPol
              }
    ((sc, path, rmvsf, timint, ch, seldepth), statf) <- runCState (alphaBeta abc) stati
    let n = sNodes $ mstats statf
    informGuiBM sc draft seldepth n path
    ctxLog LogInfo $ "seldepth " ++ show seldepth ++ " score " ++ show sc ++ " path " ++ show path
    return (path, sc, rmvsf, timint, statf, ch)
    where ep = esEParams $ evalst stati
