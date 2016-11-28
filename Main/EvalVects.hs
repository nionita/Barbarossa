{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import Data.List (intersperse)
-- import Data.Monoid
-- import Network
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO
import System.Time

import Struct.Struct
import Struct.Status
import Struct.Context
import Struct.Config
import Hash.TransTab
-- import Moves.BaseTypes
-- import Search.AlbetaTypes
import Moves.Base
import Moves.Board (posFromFen, initPos)
import Moves.History
-- import Search.CStateMonad (runCState)
import Eval.FileParams (makeEvalState)
import Eval.Eval
import Uci.UciGlue

data Options = Options {
        optConfFile :: Maybe String,	-- config file
        optParams   :: [String],	-- list of eval parameter assignements
        optNThreads :: Int,		-- number of threads
        optDepth    :: Int,		-- oracle search depth
        optNSkip    :: Maybe Int,	-- number of fens to skip (Nothing = none)
        optNFens    :: Maybe Int,	-- number of fens (Nothing = all)
        optAFenFile :: FilePath,	-- fen file with usual positions
        optFOutFile :: FilePath		-- output file for filter option
    }

defaultOptions :: Options
defaultOptions = Options {
        optConfFile = Nothing,
        optParams   = [],
        optNThreads = 1,
        optDepth    = 1,
        optNSkip    = Nothing,
        optNFens    = Nothing,
        optAFenFile = "alle.epd",
        optFOutFile = "vect.txt"
    }

setConfFile :: String -> Options -> Options
setConfFile cf opt = opt { optConfFile = Just cf }

addParam :: String -> Options -> Options
addParam pa opt = opt { optParams = pa : optParams opt }

addNThrds :: String -> Options -> Options
addNThrds ns opt = opt { optNThreads = read ns }

addDepth :: String -> Options -> Options
addDepth ns opt = opt { optDepth = read ns }

addNSkip :: String -> Options -> Options
addNSkip ns opt = opt { optNSkip = Just $ read ns }

addNFens :: String -> Options -> Options
addNFens ns opt = opt { optNFens = Just $ read ns }

addIFile :: FilePath -> Options -> Options
addIFile fi opt = opt { optAFenFile = fi }

addOFile :: FilePath -> Options -> Options
addOFile fi opt = opt { optFOutFile = fi }

options :: [OptDescr (Options -> Options)]
options = [
        Option "c" ["config"]  (ReqArg setConfFile "STRING") "Configuration file",
        Option "p" ["param"]   (ReqArg addParam "STRING")    "Eval/search/time params: name=value,...",
        Option "i" ["input"]   (ReqArg addIFile "STRING")     "Input (fen) file",
        Option "o" ["output"]  (ReqArg addOFile "STRING")    "Output file",
        Option "d" ["depth"]   (ReqArg addNThrds "STRING")  "Oracle search depth",
        Option "t" ["threads"] (ReqArg addNThrds "STRING")  "Number of threads",
        Option "f" ["fens"]    (ReqArg addNFens "STRING")      "Number of fens",
        Option "s" ["skip"]    (ReqArg addNSkip "STRING")      "Number of fens to skip"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName
              ++ " [-c CONF] [-l LEV] [-p name=val[,...]] [-a AFILE] [-o OFILE] [-t THREADS]"
          idName = "EvalVects"

initContext :: Options -> IO Context
initContext opts = do
    clktm <- getClockTime
    lchan <- newChan
    wchan <- newChan
    ichan <- newChan
    ha <- newCache 1	-- it will take the minimum number of entries
    hi <- newHist
    let paramList = stringToParams $ concat $ intersperse "," $ optParams opts
    (parc, evs) <- makeEvalState (optConfFile opts) paramList "progver" "progsuf"
    let chg = Chg {
            working = False,
            compThread = Nothing,
            crtStatus = posToState initPos ha hi evs,
            forGui = Nothing,
            srchStrtMs = 0,
            myColor = White,
            prvMvInfo = Nothing,
            realPly = Nothing
         }
    ctxVar <- newMVar chg
    let context = Ctx {
            logger = lchan,
            writer = wchan,
            inform = ichan,
            strttm = clktm,
            change = ctxVar,
            loglev = LogNever,
            evpid  = parc,
            batch  = True,
            tipars = npSetParm (colParams paramList :: CollectFor TimeParams)
         }
    return context

-- Client/server works only for analysis, filter is always local
main :: IO ()
main = do
    (opts, _) <- theOptions
    ctx <- initContext opts
    runReaderT
        (filterFile (optAFenFile opts) (optFOutFile opts) (optDepth opts) (optNSkip opts) (optNFens opts))
        ctx

filterFile :: FilePath -> FilePath -> Int -> Maybe Int -> Maybe Int -> CtxIO ()
filterFile fi fo depth ms mn = do
    chg <- readChanging
    let crts = crtStatus chg
        mfi = markerEval (evalst crts) initPos
    lift $ do
        putStrLn $ "Vectorizing " ++ fi
        putStrLn $ "Results to  " ++ fo
        putStrLn $ "Marker: " ++ show mfi
    hi <- liftIO $ openFile fi ReadMode
    ho <- liftIO $ openFile fo WriteMode
    case ms of
        Just m  -> loopCount $ skipLines hi m
        Nothing -> return ()
    loopCount $ oracleAndFeats depth crts hi ho mn
    liftIO $ do
        hClose ho
        hClose hi

loopCount :: Monad m => (Int -> m Bool) -> m ()
loopCount act = go 1
    where go !k = do
              r <- act k
              when r $ go (k+1)

skipLines :: Handle -> Int -> Int -> CtxIO Bool
skipLines hi m k = do
    end <- if k <= m then lift $ hIsEOF hi else return True
    if end
       then return False
       else do
           _ <- lift $ hGetLine hi
           when (k `mod` 10000 == 0) $ lift $ do
               putStrLn $ "Positions skipped: " ++ show k
               hFlush stdout
           return True

oracleAndFeats :: Int -> MyState -> Handle -> Handle -> Maybe Int -> Int -> CtxIO Bool
oracleAndFeats depth crts hi ho mn k = do
    end <- case mn of
               Nothing -> lift $ hIsEOF hi
               Just n  -> if k <= n then lift $ hIsEOF hi else return True
    if end
       then return False
       else do
           fen <- lift $ hGetLine hi
           when (k `mod` 10000 == 0) $ lift $ do
               putStrLn $ "Positions completed: " ++ show k
               hFlush stdout
           let pos = posFromFen fen
               mystate = posToState pos (hash crts) (hist crts) (evalst crts)
           -- Search to depth:
           (path, sc, _, _, fstate) <- bestMoveCont depth 0 mystate Nothing [] []
           when (not $ null path) $ do
               let (Feats ph fts) = featsEval (evalst crts) pos
                   sts = evalState (posEval pos) (evalst crts)
                   n   = nodes $ stats fstate
               lift $ hPutStrLn ho $ show sts ++ " " ++ show ph ++ " "
                   ++ show n ++ " " ++ show sc ++ " " ++ show fts
           return True

{--
-- Some utilities:
debugMes, logmes :: String -> Game ()
logmes = lift . lift . putStrLn
debugMes _ = return ()

-- With QS this is a mess, coz we don't know where the QS stops and miss the signs!
searchQ :: Move -> Game (Maybe Score)
searchQ m = do
    debugMes $ "  --> SearchQ move: " ++ show m
    r <- doMove m False
    case r of
        Illegal -> return Nothing
        Final s -> do
            p <- getPos
            undoMove
            return $ Just $ Score { score = -s, pos = p }
        _       -> do
            s <- negateScore <$> pvQSearch 1 minBound maxBound
            undoMove
            debugMes $ "  <-- SearchQ move: " ++ show m ++ " score = " ++ show s
            return $ Just s

data Score = Score { score :: !Int, pos :: MyPos } deriving Show

instance Eq Score where
   s1 == s2 = score s1 == score s2

-- We need this so that we can negate safely:
instance Bounded Score where
    minBound = Score { score = minBound+2000, pos = initPos }
    maxBound = Score { score = maxBound-2000, pos = initPos }

instance Ord Score where
    compare = comparing score

negateScore :: Score -> Score
negateScore s = s { score = - score s }

{-# INLINE pvQLoop #-}
pvQLoop :: Int -> Score -> Score -> [Move] -> Game Score
pvQLoop lev b = go
    where go s []     = return s
          go s (m:ms) = do
             (cut, s') <- pvQInnerLoop lev b s m
             if cut then return s'
                    else go s' ms

spaces :: Int -> String
spaces l = take n $ repeat ' '
    where n = l * 2

pvQInnerLoop :: Int -> Score -> Score -> Move -> Game (Bool, Score)
pvQInnerLoop lev !b !a m = do
    debugMes $ spaces lev ++ "--> pvQInnerLoop a = " ++ show a ++ " b = " ++ show b ++ " move: " ++ show m
    r <- doMove m True
    case r of
        Illegal -> return (False, a)
        Final s -> do
            p <- getPos
            undoMove
            let s1 = Score { score = -s, pos = p }
            debugMes $ spaces lev ++ "<-- pvQInnerLoop s = -" ++ show s
            if s1 >= b
               then return (True, b)
               else if s1 > a
                       then return (False, s1)
                       else return (False, a)
        _       -> do
            s1 <- negateScore <$> pvQSearch (lev+1) (negateScore b) (negateScore a)
            undoMove
            debugMes $ spaces lev ++ "<-- pvQInnerLoop s = " ++ show s1
            if s1 >= b
               then return (True, b)
               else if s1 > a
                       then return (False, s1)
                       else return (False, a)

-- Fail-Hard
pvQSearch :: Int -> Score -> Score -> Game Score
pvQSearch !lev !a !b = do
    debugMes $ spaces lev ++ "--> pvQSearch a = " ++ show a ++ " b = " ++ show b
    tact <- tacticalPos
    if tact
       then do
           mvs <- genTactMoves
           if null mvs
              then do
                  p <- getPos
                  return $! minBound { pos = p }	-- mated
              else pvQLoop lev b a mvs
       else do
           sta <- staticVal
           if sta >= score b
              then return b
              else do	-- no delta cut
                  mvs <- genTactMoves
                  p   <- getPos
                  if null mvs
                     then return $! Score { score = sta, pos = p }
                     else if sta > score a
                             then pvQLoop lev b (Score { score = sta, pos = p}) mvs
                             else pvQLoop lev b a                               mvs
--}
