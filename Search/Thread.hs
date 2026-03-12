-- {-# LANGUAGE TypeFamilies #-}
module Search.Thread () where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Int
import Data.Time.Clock (UTCTime(..), getCurrentTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Search.CStateMonad (CState, runCState)
import Search.Core
import Moves.History
import Struct.Context
import Struct.Struct
import Struct.Status
import Uci.UCI

-- Result of a search
-- type IterResult = ([Move], Int, [Move], Bool, MyState, Int)

--
-- These functions are helper for the main program
--

makeSearchContext :: Context -> Changing -> String
                  -> IO (SearchContext, MVar WorkerControl, MVar WorkerStatus)
makeSearchContext ctx chg pf = do
    scco <- newEmptyMVar
    scst <- newEmptyMVar
    let sc = SearchContext {
                 scLogger = logging (logger ctx) (strttm ctx) pf,
                 scGet = tryTakeMVar scco,
                 scPut = putMVar scst
             }
    return (sc, scco, scst)

-- We could get new number of threads after a few "games" and theoretically
-- get less threads as before - here we only handle increasing no of threads
startSearchThreads :: Context -> Changing -> Int -> IO Changing
startSearchThreads ctx chg n = do
    workers <- forM [1..n] $ \i -> do
        let pf = "Th " ++ show i
        (sc, scco, scst) <- makeSearchContext ctx chg pf
        -- Start a search thread with that search context
        tid <- forkIO $ runReaderT theSearchThread sc
        let worker = Worker {
                     woThreadId = tid, woControl = scco, woStatus = scst, woSearch = wsSearchInit
                 }
        return worker
    return chg { workerThrs = workers }

-- Send them the command to begin to search
-- At least half will start with draft 1, the rest with draft 2
startSearching :: Changing -> IO ()
startSearching chg = do
    let th = length (workerThrs chg)
        n1 = (th + 1) `div` 2
        n2 = th - n1
        dr = take n1 (repeat $ WcSearch 1 (crtStatus chg))
          ++ take n2 (repeat $ WcSearch 2 (crtStatus chg))
    forM_ (zip dr $ workerThrs chg) $ \(cmd, worker) -> putMVar (woControl worker) cmd

-- Send them the command to stop searching
stopSearching :: Changing -> IO ()
stopSearching chg = forM_ (workerThrs chg) $ \w -> putMVar (woControl w) WcStop

wsSearchInit = WorkerSearch {
        wsDraft = 0, wsAlpha = minBound, wsBeta = maxBound, wsSelDe = 0,
        wsCurmv = 0, wsNodes = 0, wsPv = [], wsScore = minBound
    }

--
-- These functions will beexecuted in serach threads
--

-- A few convenience functions:
stxPutStatus :: WorkerStatus -> StxIO ()
stxPutStatus ws = ask >>= \sc -> liftIO (scPut sc ws)

theSearchThread :: StxIO ()
theSearchThread = do
    sc <- ask
    liftIO $ scLogger sc "Thread started"
    searchThreadWait

-- Waiting for Main to send us the search command
searchThreadWait :: StxIO ()
searchThreadWait = loop
    where loop = do
              sc <- ask
              mctl <- liftIO $ scGet sc
              case mctl of
                  Just ctl -> interpretControl ctl
                  Nothing  -> liftIO $ threadDelay $ msSleep * 1000
              loop
          msSleep = 10

-- Interpret control from Main (in wait state)
-- In this state we wait for the go command, anything else will be
-- just logged and ignored
interpretControl :: WorkerControl -> StxIO ()
interpretControl (WcSearch draft mystate) = startSearchLoop draft mystate
interpretControl _ = do
    sc <- ask
    liftIO $ scLogger sc $ "Waiting for search start, but got something else"

startSearchLoop :: Int -> MyState -> StxIO ()
startSearchLoop draft mystate = do
    hi <- liftIO newHist
    let iniState = mystate { hist = hi, mstats = ssts0 }
    loop draft iniState Nothing [] []
    where loop draft mystate mlsc lpv rmvs = do
              (path, sc, rmvsf, timint, finState, _) <- bestMoveCont draft mystate mlsc lpv rmvs
              when (not timint && draft < maxDraft) $ loop (draft+1) finState (Just sc) path rmvsf
          maxDraft = 20

-- One iteration in the search for the best move
bestMoveCont :: Int -> MyState -> Maybe Int -> [Move] -> [Move] -> StxIO IterResult
bestMoveCont draft stati lastsc lpv rmvs = do
    -- informGuiDraft draft
    stxLog $ "start search for depth " ++ show draft
    let abc = ABC {
                maxdepth = draft,
                lastpv = lpv,
                lastscore = lastsc,
                rootmvs   = rmvs,
                window    = aspirWindow,
                intuning  = False,
                abortPolicy = NoAbort
              }
    ((sc, path, rmvsf, timint, ch, seldepth), statf) <- runCState (alphaBeta abc) stati
    let n = sNodes $ mstats statf
    -- informGuiBM sc draft seldepth n path
    stxLog $ "seldepth " ++ show seldepth ++ " score " ++ show sc ++ " path " ++ show path
    return (path, sc, rmvsf, timint, statf, ch)
    where aspirWindow = 36
