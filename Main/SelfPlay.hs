{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where
import Control.Monad.Reader
import Control.Monad (when, void)
import Control.Concurrent
import Control.Exception
import Data.Char (isSpace)
import Data.List (intersperse)
import Data.Maybe (fromMaybe, isJust)
import Foreign hiding (void)
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
-- import System.Time

import Struct.Struct
import Struct.Status
import Struct.Context
import Struct.Config
import Hash.TransTab
import Search.AlbetaTypes (SStats(..), ssts0)
import Moves.Core
import Moves.Internal.Base
import Moves.Notation
import Moves.History
import Search.CStateMonad (execCState)
import Eval.FileParams (makeEvalState)
-- import Eval.Eval	-- not yet needed
import Uci.UciGlue

debug :: Bool
debug = False

data Options = Options {
        optPlayer1  :: Maybe String,	-- player 1 config file
        optPlayer2  :: Maybe String,	-- player 2 config file
        optConfFile :: Maybe String,	-- config file
        optParams   :: [String],	-- list of eval parameter assignements
        -- optNThreads :: Int,		-- number of threads - not used for self play now
        optDepth    :: Int,		-- search depth for self play
        optLogLev   :: LogLevel,		-- log level: 0 (debug) to 5 (never)
        optNodes    :: Maybe Int,	-- search nodes per move for self play
        optNodeMargin :: Int,	-- percentual safety margin for nodes passed to search
        optNSkip    :: Maybe Int,	-- number of fens to skip (Nothing = none)
        optNFens    :: Maybe Int,	-- number of fens (Nothing = all)
        optMatch    :: Maybe String,	-- match between configs in the given directory
        optPerfTest :: Bool,		-- perf test on input fen file
        optBuildInfo :: Bool,		-- print build flavor and opposite build command
        optSprtAlpha :: Maybe Double,	-- SPRT alpha
        optSprtBeta  :: Maybe Double,	-- SPRT beta
        optSprtElo0  :: Maybe Double,	-- SPRT elo0
        optSprtElo1  :: Maybe Double,
        -- SPRT elo1
        optFenPrintEvery :: Int,
        -- print every Nth input fen
        optAFenFile :: FilePath,	-- fen file with start positions
        optFOutFile :: FilePath		-- output file for filter option
    }

defaultOptions :: Options
defaultOptions = Options {
        optPlayer1  = Nothing,
        optPlayer2  = Nothing,
        optConfFile = Nothing,
        optParams   = [],
        -- optNThreads = 1,
        optDepth    = 1,
        optLogLev   = LogNever,		-- default: never log
        optNodes    = Nothing,
        optNodeMargin = 10,
        optNSkip    = Nothing,
        optNFens    = Nothing,
        optMatch    = Nothing,
        optPerfTest = False,
        optBuildInfo = False,
        optSprtAlpha = Nothing,
        optSprtBeta  = Nothing,
        optSprtElo0  = Nothing,
        optSprtElo1  = Nothing,
        optFenPrintEvery = 10,
        optAFenFile = "alle.epd",
        optFOutFile = "vect.txt"
    }
setPlayer1 :: String -> Options -> Options
setPlayer1 cf opt = opt { optPlayer1 = Just cf }

setPlayer2 :: String -> Options -> Options
setPlayer2 cf opt = opt { optPlayer2 = Just cf }

setConfFile :: String -> Options -> Options
setConfFile cf opt = opt { optConfFile = Just cf }

addParam :: String -> Options -> Options
addParam pa opt = opt { optParams = pa : optParams opt }

-- addNThrds :: String -> Options -> Options
-- addNThrds ns opt = opt { optNThreads = read ns }

addDepth :: String -> Options -> Options
addDepth ns opt = opt { optDepth = read ns }

-- When we use nodes, set the depth high enough to ignore it
-- If we want to limit also the depth: set depth after nodes
addNodes :: String -> Options -> Options
addNodes ns opt = opt { optNodes = Just $ read ns, optDepth = 40 }

addNodeMargin :: String -> Options -> Options
addNodeMargin ns opt = opt { optNodeMargin = read ns }

addNSkip :: String -> Options -> Options
addNSkip ns opt = opt { optNSkip = Just $ read ns }

addNFens :: String -> Options -> Options
addNFens ns opt = opt { optNFens = Just $ read ns }

addMatch :: String -> Options -> Options
addMatch ns opt = opt { optMatch = Just ns }

setPerfTest :: Options -> Options
setPerfTest opt = opt { optPerfTest = True }

setBuildInfo :: Options -> Options
setBuildInfo opt = opt { optBuildInfo = True }

addSprtAlpha :: String -> Options -> Options
addSprtAlpha ns opt = opt { optSprtAlpha = Just $ read ns }

addSprtBeta :: String -> Options -> Options
addSprtBeta ns opt = opt { optSprtBeta = Just $ read ns }

addSprtElo0 :: String -> Options -> Options
addSprtElo0 ns opt = opt { optSprtElo0 = Just $ read ns }

addSprtElo1 :: String -> Options -> Options
addSprtElo1 ns opt = opt { optSprtElo1 = Just $ read ns }

addFenPrintEvery :: String -> Options -> Options
addFenPrintEvery ns opt = opt { optFenPrintEvery = read ns }

addIFile :: FilePath -> Options -> Options
addIFile fi opt = opt { optAFenFile = fi }

addOFile :: FilePath -> Options -> Options
addOFile fi opt = opt { optFOutFile = fi }

setLogLev :: String -> Options -> Options
setLogLev lv opt = opt { optLogLev = llev }
    where llev = case levi of
                     0 -> DebugSearch
                     1 -> DebugUci
                     2 -> LogInfo
                     3 -> LogWarning
                     4 -> LogError
                     _ -> if levi < 0 then DebugSearch else LogNever
          levi = read lv :: Int

options :: [OptDescr (Options -> Options)]
options = [
        Option "a" ["player1"] (ReqArg setPlayer1 "STRING") "Configuration file for player 1",
        Option "b" ["player2"] (ReqArg setPlayer2 "STRING") "Configuration file for player 2",
        Option "c" ["config"]  (ReqArg setConfFile "STRING") "Configuration file",
        Option "p" ["param"]   (ReqArg addParam "STRING") "Eval/search/time params: name=value,...",
        Option "m" ["match"]   (ReqArg addMatch "STRING") "Match between 2 configs in the given directory",
        Option "P" ["perf"]    (NoArg setPerfTest) "Performance test on input FEN file",
        Option "B" ["build-info"] (NoArg setBuildInfo) "Show build flavor and opposite build command",
        Option "" ["sprt-alpha"] (ReqArg addSprtAlpha "DOUBLE") "SPRT alpha (default 0.05 when SPRT is enabled)",
        Option "" ["sprt-beta"]  (ReqArg addSprtBeta  "DOUBLE") "SPRT beta (default 0.05 when SPRT is enabled)",
        Option "" ["sprt-elo0"]  (ReqArg addSprtElo0  "DOUBLE") "SPRT H0 elo bound (default 0.0 when SPRT is enabled)",
        Option "" ["sprt-elo1"]  (ReqArg addSprtElo1  "DOUBLE") "SPRT H1 elo bound (default 2.0 when SPRT is enabled)",
        Option "" ["print-fen-every"] (ReqArg addFenPrintEvery "INT") "Print every Nth input fen before pair play (default 10)",
        Option "i" ["input"]   (ReqArg addIFile "STRING") "Input (fen) file",
        Option "o" ["output"]  (ReqArg addOFile "STRING") "Output file",
        Option "d" ["depth"]   (ReqArg addDepth "STRING") "Search depth",
        Option "n" ["nodes"]   (ReqArg addNodes "STRING") "Search nodes budget per move",
        Option "M" ["node-margin"] (ReqArg addNodeMargin "INT") "Safety margin percent for node budget (default 10)",
        -- Option "t" ["threads"] (ReqArg addNThrds "STRING") "Number of threads",
        Option "s" ["skip"]    (ReqArg addNSkip "STRING")  "Number of fens to skip",
        Option "f" ["fens"]    (ReqArg addNFens "STRING")  "Number of fens to play",
        Option "l" ["log"]     (ReqArg setLogLev "STRING") "Log leven from 0 to 5 (debug to never)"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName
              ++ " [-c CONF] [-m DIR [-a CFILE1] -b CFILE2] [-P] [-B] [-i FENFILE [-s SKIP][-f FENS]] [-o OUTFILE] [-d DEPTH]"
          idName = "SelfPlay"

data SprtConfig = SprtConfig {
        sprtAlpha :: !Double,
        sprtBeta  :: !Double,
        sprtElo0  :: !Double,
        sprtElo1  :: !Double
    }

sprtEnabled :: Options -> Bool
sprtEnabled opts = optMatch opts /= Nothing && any isJust
    [optSprtAlpha opts, optSprtBeta opts, optSprtElo0 opts, optSprtElo1 opts]

getSprtConfig :: Options -> SprtConfig
getSprtConfig opts = SprtConfig {
        sprtAlpha = fromMaybe 0.05 (optSprtAlpha opts),
        sprtBeta  = fromMaybe 0.05 (optSprtBeta opts),
        sprtElo0  = fromMaybe 0.0  (optSprtElo0 opts),
        sprtElo1  = fromMaybe 2.0  (optSprtElo1 opts)
    }

validateOptions :: Options -> IO ()
validateOptions opts
    | optPerfTest opts && optMatch opts /= Nothing
        = ioError $ userError "--perf cannot be combined with --match"
    | optPerfTest opts && optFOutFile opts /= optFOutFile defaultOptions
        = ioError $ userError "--perf cannot be combined with --output"
    | optPerfTest opts && (optPlayer1 opts /= Nothing || optPlayer2 opts /= Nothing)
        = ioError $ userError "--perf cannot be combined with --player1/--player2"
    | optNodeMargin opts < 0 || optNodeMargin opts > 99
        = ioError $ userError "--node-margin must be in [0,99]"
    | optFenPrintEvery opts <= 0
        = ioError $ userError "--print-fen-every must be > 0"
    | sprtEnabled opts && optMatch opts == Nothing
        = ioError $ userError "SPRT options require --match"
    | sprtEnabled opts && (sprtAlpha cfg <= 0 || sprtAlpha cfg >= 1)
        = ioError $ userError "SPRT alpha must be between 0 and 1"
    | sprtEnabled opts && (sprtBeta cfg <= 0 || sprtBeta cfg >= 1)
        = ioError $ userError "SPRT beta must be between 0 and 1"
    | sprtEnabled opts && sprtAlpha cfg + sprtBeta cfg >= 1
        = ioError $ userError "SPRT requires alpha + beta < 1"
    | sprtEnabled opts && sprtElo0 cfg >= sprtElo1 cfg
        = ioError $ userError "SPRT requires elo0 < elo1"
    | otherwise = return ()
    where cfg = getSprtConfig opts

buildFlavor, otherBuildCmd :: String
#ifdef REPRO_HIST
buildFlavor = "reproducible history (REPRO_HIST enabled)"
otherBuildCmd = "stack build Barbarossa:exe:SelfPlay"
#else
buildFlavor = "default history (randomized small init)"
otherBuildCmd = "stack build --flag Barbarossa:reproselfplay Barbarossa:exe:SelfPlay"
#endif

showBuildInfo :: IO ()
showBuildInfo = do
    putStrLn $ "SelfPlay build flavor: " ++ buildFlavor
    putStrLn $ "To build the other flavor use:"
    putStrLn $ "  " ++ otherBuildCmd

initContext :: Options -> IO Context
initContext opts = do
    clktm <- getMyTime
    lchan <- newChan
    wchan <- newChan
    ha <- newCache 1	-- it will take the minimum number of entries
    hi <- newHist
    let paramList
            | null $ optParams opts = []
            | otherwise             = stringToParams $ concat $ intersperse "," $ optParams opts
    (parc, evs) <- makeEvalState (optConfFile opts) paramList "progver" "progsuf"
    let chg = Chg {
            working = False,
            compThread = Nothing,
            crtStatus = posToState initPos ha hi evs,
            realPly = Nothing,
            forGui = Nothing,
            srchStrtMs = 0,
            myColor = White,
            totBmCh = 0, lastChDr = 0, lmvScore = Nothing
         }
    ctxVar <- newMVar chg
    let context = Ctx {
            logger = lchan,
            writer = wchan,
            strttm = clktm,
            change = ctxVar,
            loglev = if debug then DebugSearch else optLogLev opts,
            evpid  = parc
         }
    return context

main :: IO ()
main = do
    (opts, _) <- theOptions
    if optBuildInfo opts
       then showBuildInfo
       else do
           validateOptions opts
           ctx <- initContext opts
           if optPerfTest opts
              then runReaderT (perfTestFile opts) ctx
              else case optMatch opts of
                        Nothing  -> runReaderT (filterFile opts) ctx
                        Just dir -> do
                            GameScore w d l <- runReaderT (matchFile opts dir) ctx
                            putStrLn $ "End result: (" ++ show w ++ "," ++ show d ++ "," ++ show l ++ ")"

filterFile :: Options -> CtxIO ()
filterFile opts = do
    ctx <- ask
    let logFileName = "selfplay-" ++ show (startSecond ctx) ++ ".log"
    startLogger logFileName
    lift $ do
        putStrLn $ "Vectorizing " ++ optAFenFile opts
        putStrLn $ "Play depth  " ++ show (optDepth opts)
        putStrLn $ "Results to  " ++ optFOutFile opts
        -- putStrLn $ "Marker: " ++ show markerEval
    hi <- liftIO $ openFile (optAFenFile opts) ReadMode
    ho <- liftIO $ openFile (optFOutFile opts) WriteMode
    case optNSkip opts of
        Just m  -> loopCount (skipLines hi m) ()
        Nothing -> return ()
    -- loopCount (oracleAndFeats (optDepth opts) hi ho (optNFens opts)) ()
    loopCount (balancedPos hi ho (optNFens opts)) ()
    liftIO $ do
        hClose ho
        hClose hi

data PerfAcc = PerfAcc {
        perfValid   :: !Int,
        perfInvalid :: !Int,
        perfNodes   :: !Int
    }

perfTestFile :: Options -> CtxIO ()
perfTestFile opts = do
    ctx <- ask
    let logFileName = "selfplay-" ++ show (startSecond ctx) ++ ".log"
    startLogger logFileName
    lift $ do
        putStrLn $ "Performance test from " ++ optAFenFile opts
        putStrLn $ "Search depth  " ++ show (optDepth opts)
    hi <- liftIO $ openFile (optAFenFile opts) ReadMode
    startMs <- liftIO $ currMilli (strttm ctx)
    acc <- loopCount (perfLoop hi (optDepth opts)) (PerfAcc 0 0 0)
    endMs <- liftIO $ currMilli (strttm ctx)
    let elapsedMs = max 1 (endMs - startMs)
        nps = perfNodes acc * 1000 `div` elapsedMs
    liftIO $ do
        putStrLn $ "Positions searched: " ++ show (perfValid acc)
        putStrLn $ "Positions skipped:  " ++ show (perfInvalid acc)
        putStrLn $ "Total nodes:        " ++ show (perfNodes acc)
        putStrLn $ "Total time (ms):    " ++ show elapsedMs
        putStrLn $ "Nodes/second:       " ++ show nps
        hClose hi

perfLoop :: Handle -> Int -> Int -> PerfAcc -> CtxIO (Bool, PerfAcc)
perfLoop hi depth _k acc = do
    end <- lift $ hIsEOF hi
    if end
       then return (False, acc)
       else do
           fen <- lift $ hGetLine hi
           if all isSpace fen
              then return (True, acc { perfInvalid = perfInvalid acc + 1 })
              else do
                  posRes <- liftIO $ try (evaluate (posFromFen fen)) :: CtxIO (Either SomeException MyPos)
                  case posRes of
                      Left _ -> return (True, acc { perfInvalid = perfInvalid acc + 1 })
                      Right pos -> do
                          chg <- readChanging
                          let crts = crtStatus chg
                              sini = posToState pos (hash crts) (hist crts) (evalst crts)
                          modifyChanging $ \c -> c { crtStatus = sini }
                          (_, _, nodes) <- iterativeDeepening depth Nothing
                          let acc' = PerfAcc {
                                  perfValid = perfValid acc + 1,
                                  perfInvalid = perfInvalid acc,
                                  perfNodes = perfNodes acc + nodes
                              }
                          return (True, acc')

matchFile :: Options -> String -> CtxIO GameScore
matchFile opts dir = do
    liftIO $ setCurrentDirectory dir
    ctx <- ask
    let logFileName = "selfplay-" ++ show (startSecond ctx) ++ ".log"
    startLogger logFileName
    liftIO $ do
        putStrLn $ "Playing games from " ++ optAFenFile opts
        putStrLn $ "Play depth  " ++ show (optDepth opts)
        putStrLn $ "Play nodes  " ++ show (optNodes opts)
        putStrLn $ "Node margin " ++ show (optNodeMargin opts) ++ "%"
    let mids = (,) <$> optPlayer1 opts <*> optPlayer2 opts
    case mids of
        Nothing -> do
            liftIO $ putStrLn "For a match we need 2 configs as players"
            return (GameScore 0 0 0)
        Just (id1, id2) -> do
            ctxLog LogWarning $ "Players from directory " ++ dir
            ctxLog LogWarning $ "Player 1 " ++ id1
            ctxLog LogWarning $ "Player 2 " ++ id2
            fens <- getFens (optAFenFile opts) (fromMaybe 0 (optNSkip opts)) (fromMaybe 1 (optNFens opts))
            (eval1, eval2) <- liftIO $ do
                (_, eval1) <- makeEvalState (Just id1) [] "progver" "progsuf"
                (_, eval2) <- makeEvalState (Just id2) [] "progver" "progsuf"
                return (eval1, eval2)
            when debug $ do
                ctxLog LogInfo $ "Player 1 config: " ++ show eval1
                ctxLog LogInfo $ "Player 2 config: " ++ show eval2
            let msprt = if sprtEnabled opts then Just $ mkSprtState (getSprtConfig opts) else Nothing
            when (isJust msprt) $ liftIO $ do
                let scfg = getSprtConfig opts
                putStrLn $ "SPRT enabled: alpha=" ++ show (sprtAlpha scfg)
                    ++ " beta=" ++ show (sprtBeta scfg)
                    ++ " elo0=" ++ show (sprtElo0 scfg)
                    ++ " elo1=" ++ show (sprtElo1 scfg)
            let startLine = fromMaybe 0 (optNSkip opts) + 1
            acc <- playMatchPairs (optDepth opts) (optNodes opts) (optNodeMargin opts) (optFenPrintEvery opts)
                                 (id1, eval1) (id2, eval2) msprt (zip [startLine..] fens)
            liftIO $ printMatchSummary msprt acc
            return (matWdl acc)

readAllLinesStrict :: FilePath -> IO [String]
readAllLinesStrict fileName = withFile fileName ReadMode $ \hi -> go hi []
    where
      go h acc = do
          eof <- hIsEOF h
          if eof
             then return $ reverse acc
             else do
                 ln <- hGetLine h
                 ln `seq` go h (ln : acc)

-- Read a chunk of fens strictly into memory, then cycle deterministically.
getFens :: MonadIO m => String -> Int -> Int -> m [String]
getFens fileName skip count = liftIO $ do
    allFens <- readAllLinesStrict fileName
    if null allFens
       then ioError $ userError $ "Empty fen input file: " ++ fileName
       else do
           let total = length allFens
               start = if skip >= total then 0 else max 0 skip
               rotated = drop start allFens ++ take start allFens
           return $ take count $ cycle rotated
-- If the fen file has an index, use it to skip to the wanted line
-- The index must be created with binary IndexTxt from Bartest
skipWithIndex :: MonadIO m => String -> Handle -> Int -> m Bool
skipWithIndex fn h k = liftIO $ do
    let fni = replaceExtension fn "idx"
    fniex <- doesFileExist fni
    if fniex
       then do
           withBinaryFile fni ReadMode $ \ih ->
               allocaBytes 4 $ \ptr -> do
                   when debug $ putStrLn $ "Skip to index entry " ++ show k
                   hSeek ih AbsoluteSeek (fromIntegral k * 4)
                   rb <- hGetBuf ih ptr 4
                   if rb < 4
                      then error "Unexpected EOF in index file"
                      else do
                          wo <- peek ptr :: IO Word32
                          when debug $ putStrLn $ "Skip to file byte " ++ show wo
                          hSeek h AbsoluteSeek (fromIntegral wo)
                          return True
       else return False

loopCount :: Monad m => (Int -> a -> m (Bool, a)) -> a -> m a
loopCount act = go 1
    where go !k a = do
              (r, b) <- act k a
              if r then go (k+1) b else return b

skipLines :: MonadIO m => Handle -> Int -> Int -> () -> m (Bool, ())
skipLines hi m k () = do
    end <- if k <= m then liftIO $ hIsEOF hi else return True
    if end
       then return (False, ())
       else do
           _ <- liftIO $ hGetLine hi
           when (k `mod` 100000 == 0) $ liftIO $ do
               putStrLn $ "Positions skipped: " ++ show k
               hFlush stdout
           return (True, ())

-- We want positions which are not very imbalanced (after 1 ply search)
balancedPos :: Handle -> Handle -> Maybe Int -> Int -> () -> CtxIO (Bool, ())
balancedPos hi ho mn k () = do
    end <- case mn of
               Nothing -> lift $ hIsEOF hi
               Just n  -> if k <= n then lift $ hIsEOF hi else return True
    if end
       then return (False, ())
       else do
           fen <- lift $ hGetLine hi
           when debug $ lift $ do
               putStrLn $ "Fen: " ++ fen
               hFlush stdout
           when (k `mod` 100000 == 0) $ do
               ctx <- ask
               currms <- lift $ currMilli (strttm ctx)
               lift $ do
                   putStrLn $ "Positions completed: " ++ show k ++ " ("
                       ++ show (k `div` currms) ++ " positions per ms)"
                   hFlush stdout
           let pos = posFromFen fen
           chg <- readChanging
           let crts = crtStatus chg
               sini = posToState pos (hash crts) (hist crts) (evalst crts)
           modifyChanging $ \c -> c { crtStatus = sini }
           (sc, path, _) <- iterativeDeepening 1 Nothing
           when (not (null path) && abs sc <= 150) $ lift $ hPutStrLn ho fen
           return (True, ())

oracleAndFeats :: Int -> Handle -> Handle -> Maybe Int -> Int -> () -> CtxIO (Bool, ())
oracleAndFeats depth hi _ho mn k () = do	-- not functional yet
    end <- case mn of
               Nothing -> lift $ hIsEOF hi
               Just n  -> if k <= n then lift $ hIsEOF hi else return True
    if end
       then return (False, ())
       else do
           fen <- lift $ hGetLine hi
           when debug $ lift $ do
               putStrLn $ "Fen: " ++ fen
               hFlush stdout
           when (k `mod` 10 == 0) $ do
               ctx <- ask
               currms <- lift $ currMilli (strttm ctx)
               lift $ do
                   putStrLn $ "Positions completed: " ++ show k ++ " ("
                       ++ show (currms `div` k) ++ " ms per position)"
                   hFlush stdout
           let pos = posFromFen fen
           msc <- autoPlayToEnd depth pos
           when debug $ lift $ do
               putStrLn $ "Rez of auto play: " ++ show msc
               hFlush stdout
-- This part can't work now as we do not have featsEval
{-
           case msc of
               Just sc' -> do
                    let (ph, fts) = featsEval pos
                        sc | moving pos == White =  sc'	-- score autoPlayToEnd is from White p.o.v.
                           | otherwise           = -sc'
                    lift $ hPutStrLn ho $ show ph ++ " " ++ show sc ++ " " ++ show fts
               Nothing -> return ()
-}
           return (True, ())

-- Every fen is played twice, reversing the colors
data PairWdl = PairWin | PairDraw | PairLoss deriving Eq

data PentaScore = PentaScore {
        pWW :: !Int,
        pWD :: !Int,
        pWL :: !Int,
        pDD :: !Int,
        pLD :: !Int,
        pLL :: !Int
    }

data MatchTerm = MatchTermSprt SprtResult Double | MatchTermMaxPairs

data MatchAcc = MatchAcc {
        matWdl       :: !GameScore,
        matPenta     :: !PentaScore,
        matPairsDone :: !Int,
        matPairsTried :: !Int,
        matLastLLR   :: !(Maybe Double),
        matTerm      :: MatchTerm
    }

data PairResult = PairIncomplete | PairCompleted !GameScore !PentaScore

emptyPenta :: PentaScore
emptyPenta = PentaScore 0 0 0 0 0 0

addPentaScore :: PentaScore -> PentaScore -> PentaScore
addPentaScore (PentaScore ww1 wd1 wl1 dd1 ld1 ll1) (PentaScore ww2 wd2 wl2 dd2 ld2 ll2) =
    PentaScore (ww1 + ww2) (wd1 + wd2) (wl1 + wl2) (dd1 + dd2) (ld1 + ld2) (ll1 + ll2)

pairWdl :: String -> GameResult -> Maybe PairWdl
pairWdl player (GameWin plwin _) = Just $ if player == plwin then PairWin else PairLoss
pairWdl _ (GameRemis _)          = Just PairDraw
pairWdl _ _                      = Nothing

classifyPair :: PairWdl -> PairWdl -> PentaScore
classifyPair r1 r2 = case (w, d, l) of
    (2,0,0) -> PentaScore 1 0 0 0 0 0
    (1,1,0) -> PentaScore 0 1 0 0 0 0
    (1,0,1) -> PentaScore 0 0 1 0 0 0
    (0,2,0) -> PentaScore 0 0 0 1 0 0
    (0,1,1) -> PentaScore 0 0 0 0 1 0
    (0,0,2) -> PentaScore 0 0 0 0 0 1
    _       -> emptyPenta
    where
        w = fromEnum (r1 == PairWin) + fromEnum (r2 == PairWin)
        d = fromEnum (r1 == PairDraw) + fromEnum (r2 == PairDraw)
        l = fromEnum (r1 == PairLoss) + fromEnum (r2 == PairLoss)

playOnePair
    :: Int
    -> Maybe Int
    -> Int
    -> (String, EvalState)
    -> (String, EvalState)
    -> String
    -> CtxIO PairResult
playOnePair depth maybeNodes nodeMarginPc (id1, eval1) (id2, eval2) fen = do
    let pos = posFromFen fen
    gr1 <- playGame depth maybeNodes nodeMarginPc pos (id1, eval1) (id2, eval2)
    gr2 <- playGame depth maybeNodes nodeMarginPc pos (id2, eval2) (id1, eval1)
    case (pairWdl id1 gr1, pairWdl id1 gr2) of
        (Just r1, Just r2) -> do
            let wdl1  = scoreGameResult id1 gr1
                wdl2  = scoreGameResult id1 gr2
                wdlp  = addGameScores wdl1 wdl2
                penta = classifyPair r1 r2
            return $ PairCompleted wdlp penta
        _ -> return PairIncomplete

playMatchPairs
    :: Int
    -> Maybe Int
    -> Int
    -> Int
    -> (String, EvalState)
    -> (String, EvalState)
    -> Maybe SprtState
    -> [(Int, String)]
    -> CtxIO MatchAcc
playMatchPairs depth maybeNodes nodeMarginPc printEvery (id1, eval1) (id2, eval2) msprt fens =
    go (MatchAcc (GameScore 0 0 0) emptyPenta 0 0 Nothing MatchTermMaxPairs) fens
    where
      go acc [] = return acc { matTerm = MatchTermMaxPairs }
      go acc ((lineNo, fen):rest) = do
          when (lineNo `mod` printEvery == 0) $ liftIO $ do
              putStrLn $ show lineNo ++ ": " ++ fen
              hFlush stdout
          pres <- playOnePair depth maybeNodes nodeMarginPc (id1, eval1) (id2, eval2) fen
          let accTried = acc { matPairsTried = matPairsTried acc + 1 }
          case pres of
              PairIncomplete -> go accTried rest
              PairCompleted wdlp pp -> do
                  let penta' = addPentaScore (matPenta accTried) pp
                      wdl' = addGameScores (matWdl accTried) wdlp
                      done' = matPairsDone accTried + 1
                      llrM = fmap (`sprtPentaLLR` penta') msprt
                      acc' = accTried { matWdl = wdl', matPenta = penta', matPairsDone = done', matLastLLR = llrM }
                  case (msprt, llrM) of
                      (Just ss, Just llr) -> case sprtResult ss llr of
                          SprtContinue -> go acc' rest
                          res          -> return acc' { matTerm = MatchTermSprt res llr }
                      _ -> go acc' rest

printMatchSummary :: Maybe SprtState -> MatchAcc -> IO ()
printMatchSummary _ acc = do
    let GameScore w d l = matWdl acc
        PentaScore ww wd wl dd ld ll = matPenta acc
    putStrLn $ "Completed pairs: " ++ show (matPairsDone acc) ++ " / " ++ show (matPairsTried acc)
    putStrLn $ "WDL: (" ++ show w ++ "," ++ show d ++ "," ++ show l ++ ")"
    putStrLn $ "Penta (WW,WD,WL,DD,LD,LL): (" ++ show ww ++ "," ++ show wd ++ "," ++ show wl
        ++ "," ++ show dd ++ "," ++ show ld ++ "," ++ show ll ++ ")"
    case matLastLLR acc of
        Nothing -> return ()
        Just llr -> putStrLn $ "Final LLR: " ++ show llr
    case matTerm acc of
        MatchTermMaxPairs -> putStrLn "Termination: max pairs reached"
        MatchTermSprt SprtH0 _ -> putStrLn "Termination: SPRT accepted H0"
        MatchTermSprt SprtH1 _ -> putStrLn "Termination: SPRT accepted H1"
        MatchTermSprt SprtContinue _ -> putStrLn "Termination: max pairs reached"

-- The logger will be startet anyway, but will open a file
-- only when it has to write the first message
-- When it cannot open that file, it should at least consume the messages
-- so that the channel does not get stuck
data LoggerState = LoggerFile String
                 | LoggerHandle Handle
                 | LoggerError

startLogger :: String -> CtxIO ()
startLogger file = do
    ctx <- ask
    void $ liftIO $ forkIO $ catch (theLogger (logger ctx) (LoggerFile file)) collectError
    ctxLog LogInfo "Logger started"

-- Here the logger has a problem: at the end it does not flush the logfile
-- This is important only for debug (for normal operation there will be no log)
theLogger :: Chan String -> LoggerState -> IO ()
theLogger lchan lst = do
    s <- readChan lchan
    case lst of
        LoggerError  -> theLogger lchan lst
        LoggerFile f -> handle collectError $ do
            h <- openFile f AppendMode
            hPutStrLn h s
            hFlush h
            theLogger lchan (LoggerHandle h)
        LoggerHandle h -> do
            hPutStrLn h s
            hFlush h
            theLogger lchan lst

newThread :: CtxIO () -> CtxIO ThreadId
newThread a = do
    ctx <- ask
    liftIO $ forkIO $ runReaderT a ctx

-- We play the position to the end using fixed depth for now
-- which means, this function can be used only to optimize eval weights but not
-- time or search parameters
-- The result score is from White p.o.v.:
--  0: remis
-- +1: white wins
-- -1: black wins
autoPlayToEnd :: Int -> MyPos -> CtxIO (Maybe Int)
autoPlayToEnd d pos = do
    chg <- readChanging
    let crts = crtStatus chg
        sini = posToState pos (hash crts) (hist crts) (evalst crts)
    modifyChanging $ \c -> c { crtStatus = sini }
    go (0::Int)
    where go i = do
              -- Search to depth:
              (sc, path, _) <- iterativeDeepening d Nothing
              case path of
                 []  -> do
                     when (i>0) $ ctxLog LogError $ "Empty path when playing"
                     return Nothing	-- should not happen when i > 0
                 m:_ -> do
                     let j = i+1
                     ctxLog LogInfo $ "Real move " ++ show j ++ ": " ++ show m
                     chg   <- readChanging
                     sfin' <- execCState (doRealMove m) (crtStatus chg)
                     case stack sfin' of
                         []  -> do
                             ctxLog LogError $ "Empty path when playing"
                             return Nothing
                         p:_ -> if sc == 19999	-- mate in 1
                                   then if tacticalPos p
                                           then do
                                               let r = if moving p == White then -1 else 1
                                               ctxLog LogInfo $ "Mate (" ++ show r ++ ")"
                                               return $ Just r
                                           else do
                                               ctxLog LogError $ "Mate announced, not in check!"
                                               return Nothing
                                   else if remis50Moves p
                                           then do
                                               ctxLog LogInfo $ "Remis 50 moves"
                                               return $ Just 0
                                           else do
                                               hi <- liftIO newHist
                                               let sfin = sfin' { hist = hi }
                                               modifyChanging $ \s -> s { crtStatus = sfin }
                                               go j

-- Status kept for each "player" during a game
data Player = Player {
        plName  :: String,
        plChg   :: Changing,
        plNodes :: Int
    }

-- We may play with or without a search nodes budget, the we must be able to
-- make some operations with ints & maybe ints
aloNodes :: Maybe Int -> Int -> Maybe Int
aloNodes Nothing   _  = Nothing
aloNodes (Just n1) _  = Just n1

subNodes :: Maybe Int -> Int -> Int
subNodes Nothing   _  = 0
subNodes (Just _)  _  = 0

stopNodes :: Maybe Int -> Int -> Bool
stopNodes Nothing _    = False
stopNodes (Just n1) n2 = n2 >= n1

nodesForSearch :: Int -> Maybe Int -> Maybe Int
nodesForSearch _ Nothing = Nothing
nodesForSearch _ (Just n) | n <= 0 = Just n
nodesForSearch pc (Just n) = Just $ max 1 $ n - n * pc `div` 100

-- Play the given position to the end using node budget or fixed depth with 2 configurations
-- It can be used only to optimize eval weights but not time or search parameters
-- The result contains the winner (if any) and a reason for termination
playGame :: Int -> Maybe Int -> Int -> MyPos -> (String, EvalState) -> (String, EvalState) -> CtxIO GameResult
playGame d maybeNodes nodeMarginPc pos (ide1, eval1) (ide2, eval2) = do
    ctxLog LogWarning "--------------------------"
    ctxLog LogWarning $ "Setup new game between " ++ ide1 ++ " and " ++ ide2
    chg <- readChanging
    let oldHash = hash $ crtStatus chg
    (hash1, hash2, hist1, hist2) <- liftIO $ do
        freeCache oldHash
        (,,,) <$> newCache 1 <*> newCache 1 <*> newHist <*> newHist
    let state1 = posToState pos hash1 hist1 eval1
        state2 = posToState pos hash2 hist2 eval2
        color1 = moving pos
        color2 = other color1
        -- Maybe set realPly? (what is this good for if not for time management?)
        chg1 = chg { working = False, compThread = Nothing,
                     crtStatus = state1, forGui = Nothing, srchStrtMs = 0,
                     myColor = color1, totBmCh = 0, lastChDr = 0, lmvScore = Nothing }
        chg2 = chg { working = False, compThread = Nothing,
                     crtStatus = state2, forGui = Nothing, srchStrtMs = 0,
                     myColor = color2, totBmCh = 0, lastChDr = 0, lmvScore = Nothing }
        player1 = Player { plName = ide1, plChg = chg1, plNodes = 0 }
        player2 = Player { plName = ide2, plChg = chg2, plNodes = 0 }
    ctxLog LogWarning $ "Color for " ++ ide1 ++ ": " ++ show color1
    ctxLog LogWarning $ "Starting position: " ++ posToFen pos
    go (0::Int) player1 player2
    where go i player1 player2 = do
              start  <- asks strttm
              currms <- lift $ currMilli start
              let j = i + 1
              -- Prepare for chg1 to search:
              modifyChanging $ const (plChg player1) { forGui = Nothing, srchStrtMs = currms,
                                            totBmCh = 0, lastChDr = 0 }
              case (stack . crtStatus . plChg $ player1) of
                  []   -> return $ GameAborted "Empty stack in crtStatus"
                  p0:_ -> do
                      let mbNodes = aloNodes maybeNodes (plNodes player1)
                          searchNodes = nodesForSearch nodeMarginPc mbNodes
                          curfen = posToFen p0
                      ctxLog LogInfo $ "Real ply " ++ show j ++ " engine " ++ plName player1
                          ++ " (nodes budget: " ++ show mbNodes ++ ", search budget: " ++ show searchNodes ++ ")"
                      ctxLog LogInfo $ "Current fen: " ++ curfen
                      -- Search to depth or node budget:
                      (sc, path, nodes) <- iterativeDeepening d searchNodes
                      ctxLog LogInfo $ "Real ply " ++ show j ++ " returns " ++ show sc ++ " / " ++ show path
                          ++ " / " ++ show nodes
                      case path of
                        [] -> if sc == 0 && nodes == 0	-- can't make move: mated or stale mated
                                 then if tacticalPos p0
                                         then do
                                             ctxLog LogWarning $ "Mated (" ++ plName player2 ++ " wins)"
                                             ctxLog LogWarning $ "Fen: " ++ curfen
                                             ctxLog LogWarning $ "Rezult: ply " ++ show j ++ " sc " ++ show sc
                                                  ++ " path " ++ show path ++ " nodes " ++ show nodes
                                             return $ GameWin (plName player2) "Mate"
                                         else do
                                             ctxLog LogWarning $ "Remis: patt"
                                             ctxLog LogWarning $ "Fen: " ++ curfen
                                             ctxLog LogWarning $ "Rezult: ply " ++ show j ++ " sc " ++ show sc
                                                  ++ " path " ++ show path ++ " nodes " ++ show nodes
                                             return $ GameRemis "Remis (patt)"
                                 else do
                                     ctxLog LogError $ "Aborted: unexpected empty path when playing"
                                     ctxLog LogError $ "Fen: " ++ curfen
                                     ctxLog LogError $ "Rezult: ply " ++ show j ++ " sc " ++ show sc
                                          ++ " path " ++ show path ++ " nodes " ++ show nodes
                                     return $ GameAborted "Empty path when playing"
                        m:_ -> do
                             ctxLog LogInfo $ "Real move " ++ show j
                                  ++ " from " ++ plName player1 ++ ": " ++ show m
                             chg1f <- readChanging
                             s1fin <- execCState (doRealMove m) (crtStatus chg1f)
                             s2ini <- execCState (doRealMove m) (crtStatus (plChg player2))
                             case stack s1fin of
                               []  -> return $ GameAborted "Empty stack in s1fin"
                               p:_ -> do
                                 -- Using length path here could be a problem
                                 -- in case of a TT cut which is on path
                                 if | sc == mateScore && length path == 1	-- mate in 1
                                      -> if tacticalPos p
                                            then do
                                                ctxLog LogWarning $ "Mate (" ++ plName player1 ++ " wins)"
                                                ctxLog LogWarning $ "Fen: " ++ curfen
                                                ctxLog LogWarning $ "Rezult: ply " ++ show j ++ " sc " ++ show sc
                                                     ++ " path " ++ show path ++ " nodes " ++ show nodes
                                                return $ GameWin (plName player1) "Mate"
                                            else do
                                                ctxLog LogError $ "Aborted: mate announced, but not in check!"
                                                ctxLog LogError $ "Fen: " ++ curfen
                                                ctxLog LogError $ "Rezult: ply " ++ show j ++ " sc " ++ show sc
                                                     ++ " path " ++ show path ++ " nodes " ++ show nodes
                                                return $ GameAborted "Mate announced, but not in check"
                                    | remis50Moves p -> do
                                         ctxLog LogWarning $ "Remis: 50 moves"
                                         return $ GameRemis "Remis 50 moves rule"
                                    | remis3Repetitions p $ stack s1fin -> do
                                         ctxLog LogWarning $ "Remis: 3 repetitions"
                                         return $ GameRemis "Remis 3 repetitions rule"
                                    | noMatingMaterial p -> do
                                         ctxLog LogWarning $ "Remis: no mating material"
                                         ctxLog LogWarning $ "Fen: " ++ curfen
                                         ctxLog LogWarning $ "Rezult: ply " ++ show j ++ " sc " ++ show sc
                                              ++ " path " ++ show path ++ " nodes " ++ show nodes
                                         return $ GameRemis "Remis no mating material"
                                    -- Under 1200 cp we consider resignation
                                    | sc < -1200 -> do
                                         ctxLog LogWarning $ "Resign (" ++ plName player2 ++ " wins)"
                                         ctxLog LogWarning $ "Fen: " ++ curfen
                                         ctxLog LogWarning $ "Rezult: ply " ++ show j ++ " sc " ++ show sc
                                              ++ " path " ++ show path ++ " nodes " ++ show nodes
                                         return $ GameWin (plName player2) "Resignation"
                                    | otherwise -> do
                                         hi <- liftIO newHist
                                         let state2 = s2ini { hist = hi, mstats = ssts0 }
                                             chg1n  = chg1f { crtStatus = s1fin }
                                             chg2n  = (plChg player2) { crtStatus = state2 }
                                         go j (player2 { plChg = chg2n })
                                              (player1 { plChg = chg1n, plNodes = subNodes mbNodes nodes})

iterativeDeepening :: Int -> Maybe Int -> CtxIO (Int, [Move], Int)
iterativeDeepening depth maybeMaxNodes = do
    --when debug $ lift $ do
    --    putStrLn $ "In iter deep: " ++ show depth
    --    hFlush stdout
    chg <- readChanging
    go 1 (crtStatus chg) Nothing [] []
    where go d sini lsc lpv rmvs = do
              --when debug $ lift $ do
              --    putStrLn $ "In iter deep go: " ++ show d
              --    hFlush stdout
              (path, sc, rmvsf, _timint, sfin, _) <- bestMoveCont True maybeMaxNodes d 0 0 sini lsc lpv rmvs
              let nodes = fromIntegral $ sNodes $ mstats sfin
              -- We don't want to search less than depth 2, because depth 1 delivers
              -- erroneus moves by currently not updating the best score
              if d > 1 && (null path || d >= depth || stopNodes maybeMaxNodes nodes)
                 then return (sc, path, nodes)
                 else go (d+1) sfin (Just sc) path rmvsf

-- Append error info to error file:
collectError :: SomeException -> IO ()
collectError e = handle cannot $ do
    let efname = "Barbarossa_collected_errors.txt"
    tm <- getMyTime
    ef <- openFile efname AppendMode
    hPutStrLn ef $ formatMyTime tm ++ " selfplay: " ++ show e
    hClose ef
    where cannot :: IOException -> IO ()
          cannot _ = return ()

data GameResult = GameAborted String
                | GameWin String String
                | GameRemis String
                deriving Show

data GameScore = GameScore !Int !Int !Int

scoreGameResult :: String -> GameResult -> GameScore
scoreGameResult player (GameWin plwin _)
    | player == plwin = GameScore 1 0 0
    | otherwise       = GameScore 0 0 1
scoreGameResult _ (GameRemis _) = GameScore 0 1 0
scoreGameResult _ _             = GameScore 0 0 0

addGameScores :: GameScore -> GameScore -> GameScore
addGameScores (GameScore w1 d1 l1) (GameScore w2 d2 l2) = GameScore (w1+w2) (d1+d2) (l1+l2)

data SprtResult = SprtH0 | SprtH1 | SprtContinue
    deriving (Eq, Show)

data SprtState = SprtState {
        sprtCfgState :: !SprtConfig,
        sprtLowerBound :: !Double,
        sprtUpperBound :: !Double
    }

mkSprtState :: SprtConfig -> SprtState
mkSprtState cfg = SprtState {
        sprtCfgState = cfg,
        sprtLowerBound = log (sprtBeta cfg / (1 - sprtAlpha cfg)),
        sprtUpperBound = log ((1 - sprtBeta cfg) / sprtAlpha cfg)
    }

sprtResult :: SprtState -> Double -> SprtResult
sprtResult ss llr
    | llr >= sprtUpperBound ss = SprtH1
    | llr <= sprtLowerBound ss = SprtH0
    | otherwise                = SprtContinue

sprtPentaLLR :: SprtState -> PentaScore -> Double
sprtPentaLLR ss (PentaScore ww wd wl dd ld ll) =
    getLLRNormalized total scores probs t0 t1
    where
      reg x = if x == 0 then 1e-3 else fromIntegral x
      ll' = reg ll
      ld' = reg ld
      wldd' = reg (wl + dd)
      wd' = reg wd
      ww' = reg ww
      total = ll' + ld' + wldd' + wd' + ww'
      probs = map (/ total) [ll', ld', wldd', wd', ww']
      scores = [0.0, 0.25, 0.5, 0.75, 1.0]
      cfg = sprtCfgState ss
      t0 = sqrt 2.0 * sprtElo0 cfg / (800.0 / log 10.0)
      t1 = sqrt 2.0 * sprtElo1 cfg / (800.0 / log 10.0)

meanVals :: [Double] -> [Double] -> Double
meanVals xs ps = sum $ zipWith (*) xs ps

meanVarVals :: [Double] -> [Double] -> (Double, Double)
meanVarVals xs ps = (mu, var)
    where
      mu = meanVals xs ps
      var = sum $ zipWith (\x p -> p * (x - mu) * (x - mu)) xs ps

itpRoot
    :: (Double -> Double)
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
itpRoot f a0 b0 fa0 fb0 k1 k2 n0 eps = go (0 :: Int) a1 b1 fa1 fb1
    where
      (a1, b1, fa1, fb1)
          | fa0 > 0 = (b0, a0, fb0, fa0)
          | otherwise = (a0, b0, fa0, fb0)
      nHalf = ceiling (logBase 2 (abs (b1 - a1) / (2.0 * eps))) :: Int
      nMax = fromIntegral nHalf + n0
      signOrOne x
          | x < 0 = -1
          | x > 0 = 1
          | otherwise = 1
      go i a b fa fb
          | abs (b - a) <= 2.0 * eps = (a + b) / 2.0
          | otherwise =
              let xHalf = (a + b) / 2.0
                  r = eps * (2.0 ** (nMax - fromIntegral i)) - (b - a) / 2.0
                  delta = k1 * ((b - a) ** k2)
                  xF = (fb * a - fa * b) / (fb - fa)
                  sigma = signOrOne (xHalf - xF)
                  xT = if delta <= abs (xHalf - xF) then xF + sigma * delta else xHalf
                  xITP = if abs (xT - xHalf) <= r then xT else xHalf - sigma * r
                  fITP = f xITP
              in if abs fITP < 1e-15
                    then xITP
                    else if fITP < 0
                            then go (i + 1) xITP b fITP fb
                            else go (i + 1) a xITP fa fITP

mleNormalized :: [Double] -> [Double] -> Double -> Double -> [Double]
mleNormalized scores probs muRef tStar = go 0 (replicate (length scores) (1.0 / fromIntegral (length scores)))
    where
      thetaEps = 1e-7
      mleEps = 1e-4
      go :: Int -> [Double] -> [Double]
      go i p
          | i >= 10 = p
          | otherwise =
              let (mu, var) = meanVarVals scores p
                  sigma = sqrt var
                  phi = map (\a ->
                          let z = (a - mu) / sigma
                          in a - muRef - 0.5 * tStar * sigma * (1.0 + z * z)
                        ) scores
                  u = minimum phi
                  v = maximum phi
                  minTheta = -1.0 / v
                  maxTheta = -1.0 / u
                  fTheta x = sum $ zipWith (\ph phat -> phat * ph / (1.0 + x * ph)) phi probs
                  theta = itpRoot fTheta minTheta maxTheta (1 / 0) (-1 / 0) 0.1 2.0 0.99 thetaEps
                  pNew = zipWith (\ph phat -> phat / (1.0 + theta * ph)) phi probs
                  maxDiff = maximum $ zipWith (\a b -> abs (a - b)) pNew p
              in if maxDiff < mleEps then pNew else go (i + 1) pNew

getLLRNormalized :: Double -> [Double] -> [Double] -> Double -> Double -> Double
getLLRNormalized total scores probs t0 t1 = total * meanVals lpr probs
    where
      p0 = mleNormalized scores probs 0.5 t0
      p1 = mleNormalized scores probs 0.5 t1
      lpr = zipWith (\a b -> log a - log b) p1 p0

remis3Repetitions :: MyPos -> [MyPos] -> Bool
remis3Repetitions p ps
    | _:_:_:_ <- filter (== zobkey p)
        $ map zobkey $ takeWhile isReversible ps = True
    | otherwise                                  = False

noMatingMaterial :: MyPos -> Bool
noMatingMaterial p
    | occup p == kings p .|. knights p = True	-- one side should have only king
    | occup p == kings p .|. bishops p
        && popCount (occup p) == 3     = True
    | otherwise                        = False
