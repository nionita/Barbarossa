{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Main where
import Control.Monad.Reader
-- import Control.Monad.State
import Control.Concurrent
import Control.Exception
-- import Data.Bits (popCount, (.|.))
import Data.List (intersperse)
-- import Data.Monoid
-- import Network
import Foreign hiding (void)
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.Time

import Struct.Struct
import Struct.Status
import Struct.Context
import Struct.Config
import Hash.TransTab
import Search.AlbetaTypes (ssts0)
import Moves.Base
import Moves.Fen
import Moves.Notation
import Moves.History
import Search.CStateMonad (execCState)
import Eval.FileParams (makeEvalState)
import Eval.BasicEval (mateScore)
import Uci.UciGlue

debug :: Bool
debug = False

data Options = Options {
        optPlayer1  :: Maybe String,	-- player 1 config file
        optPlayer2  :: Maybe String,	-- player 2 config file
        optConfFile :: Maybe String,	-- config file
        optParams   :: [String],	-- list of eval parameter assignements
        optNThreads :: Int,		-- number of threads - not used for self play now
        optDepth    :: Int,		-- search depth for self play
        optNSkip    :: Maybe Int,	-- number of fens to skip (Nothing = none)
        optNFens    :: Maybe Int,	-- number of fens (Nothing = all)
        optMatch    :: Maybe String,	-- match between configs in the given directory
        optAFenFile :: FilePath,	-- fen file with start positions
        optFOutFile :: FilePath		-- output file for filter option
    }

defaultOptions :: Options
defaultOptions = Options {
        optPlayer1  = Nothing,
        optPlayer2  = Nothing,
        optConfFile = Nothing,
        optParams   = [],
        optNThreads = 1,
        optDepth    = 1,
        optNSkip    = Nothing,
        optNFens    = Nothing,
        optMatch    = Nothing,
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

addNThrds :: String -> Options -> Options
addNThrds ns opt = opt { optNThreads = read ns }

addDepth :: String -> Options -> Options
addDepth ns opt = opt { optDepth = read ns }

addNSkip :: String -> Options -> Options
addNSkip ns opt = opt { optNSkip = Just $ read ns }

addNFens :: String -> Options -> Options
addNFens ns opt = opt { optNFens = Just $ read ns }

addMatch :: String -> Options -> Options
addMatch ns opt = opt { optMatch = Just ns }

addIFile :: FilePath -> Options -> Options
addIFile fi opt = opt { optAFenFile = fi }

addOFile :: FilePath -> Options -> Options
addOFile fi opt = opt { optFOutFile = fi }

options :: [OptDescr (Options -> Options)]
options = [
        Option "a" ["player1"] (ReqArg setPlayer1 "STRING") "Configuration file for player 1",
        Option "b" ["player2"] (ReqArg setPlayer2 "STRING") "Configuration file for player 2",
        Option "c" ["config"]  (ReqArg setConfFile "STRING") "Configuration file",
        Option "p" ["param"]   (ReqArg addParam "STRING")    "Eval/search/time params: name=value,...",
        Option "m" ["match"]   (ReqArg addMatch "STRING") "Start match between 2 configs in the given directory",
        Option "i" ["input"]   (ReqArg addIFile "STRING")     "Input (fen) file",
        Option "o" ["output"]  (ReqArg addOFile "STRING")    "Output file",
        Option "d" ["depth"]   (ReqArg addDepth "STRING")  "Search depth",
        Option "t" ["threads"] (ReqArg addNThrds "STRING")  "Number of threads",
        Option "s" ["skip"]    (ReqArg addNSkip "STRING")      "Number of fens to skip",
        Option "f" ["fens"]    (ReqArg addNFens "STRING")      "Number of fens to play"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName
              ++ " [-c CONF] [-m DIR [-a CFILE1] -b CFILE2] [-t THREADS] [-i FENFILE [-s SKIP][-f FENS]] [-o OUTFILE] [-d DEPTH]"
          idName = "SelfPlay"

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
            inform = ichan,
            strttm = clktm,
            change = ctxVar,
            loglev = if debug then DebugSearch else LogNever,
            evpid  = parc,
            tipars = npSetParm (colParams paramList :: CollectFor TimeParams)
         }
    return context

main :: IO ()
main = do
    (opts, _) <- theOptions
    ctx <- initContext opts
    case optMatch opts of
       Nothing  -> runReaderT (filterFile opts) ctx
       Just dir -> do
           wdl <- runReaderT (matchFile  opts dir) ctx
           putStrLn $ "End result: " ++ show wdl

filterFile :: Options -> CtxIO ()
filterFile opts = do
    ctx <- ask
    let logFileName = "selfplay-" ++ show (startSecond ctx) ++ ".log"
    startLogger logFileName
    startInformer
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

matchFile :: Options -> String -> CtxIO (Int, Int, Int)
matchFile opts dir = do
    ctx <- ask
    let logFileName = "selfplay-" ++ show (startSecond ctx) ++ ".log"
    startLogger logFileName
    startInformer
    liftIO $ do
        putStrLn $ "Playing games from " ++ optAFenFile opts
        putStrLn $ "Play depth  " ++ show (optDepth opts)
        putStrLn $ "Results to  " ++ optFOutFile opts
    let mids = (,) <$> optPlayer1 opts <*> optPlayer2 opts
    case mids of
        Nothing -> do
            liftIO $ putStrLn "For a match we need 2 configs as players"
            return (0, 0, 0)
        Just (id1, id2) -> do
            ctxLog LogInfo $ "Players from directory " ++ dir
            ctxLog LogInfo $ "Player 1 " ++ id1
            ctxLog LogInfo $ "Player 2 " ++ id2
            (hi, ho) <- liftIO $ do
                hi <- openFile (optAFenFile opts) ReadMode
                ho <- openFile (optFOutFile opts) WriteMode
                return (hi, ho)
            case optNSkip opts of
                Just m  -> do
                    ispos <- skipWithIndex (optAFenFile opts) hi m
                    when (not ispos) $ loopCount (skipLines hi m) ()
                Nothing -> return ()
            (eval1, eval2) <- liftIO $ do
                setCurrentDirectory dir
                (_, eval1) <- liftIO $ makeEvalState (Just id1) [] "progver" "progsuf"
                (_, eval2) <- liftIO $ makeEvalState (Just id2) [] "progver" "progsuf"
                return (eval1, eval2)
            let act = playEveryGame (optDepth opts) hi ho (optNFens opts) (id1, eval1) (id2, eval2)
            wdl <- loopCount act (0, 0, 0)
            liftIO $ do
                hClose ho
                hClose hi
            return wdl

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
               currms <- lift $ currMilli (startSecond ctx)
               lift $ do
                   putStrLn $ "Positions completed: " ++ show k ++ " ("
                       ++ show (k `div` currms) ++ " positions per ms)"
                   hFlush stdout
           let pos = posFromFen fen
           chg <- readChanging
           let crts = crtStatus chg
               sini = posToState pos (hash crts) (hist crts) (evalst crts)
           modifyChanging $ \c -> c { crtStatus = sini }
           (msc, path) <- iterativeDeepening 1
           case msc of
               Nothing -> return ()
               Just sc -> if null path
                             then return ()
                             else when (abs sc <= 150) $ lift $ hPutStrLn ho fen
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
               currms <- lift $ currMilli (startSecond ctx)
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

playEveryGame :: Int -> Handle -> Handle -> Maybe Int
              -> (String, EvalState)	-- "player" 1
              -> (String, EvalState)	-- "player" 2
              -> Int
              -> (Int, Int, Int)
              -> CtxIO (Bool, (Int, Int, Int))
playEveryGame depth hi _ho mn (id1, eval1) (id2, eval2) k wdl = do
    end <- case mn of
               Nothing -> lift $ hIsEOF hi
               Just n  -> if k <= n then lift $ hIsEOF hi else return True
    if end
       then return (False, wdl)
       else do
           fen <- lift $ hGetLine hi
           when debug $ lift $ do
               putStrLn $ "Fen: " ++ fen
               hFlush stdout
           when (k `mod` 10 == 0) $ do
               ctx <- ask
               currms <- lift $ currMilli (startSecond ctx)
               lift $ do
                   putStrLn $ "Positions completed: " ++ show k ++ " ("
                       ++ show (currms `div` k) ++ " ms per position)"
                   hFlush stdout
           let pos = posFromFen fen
           gr1 <- playGame depth pos (id1, eval1) (id2, eval2)
           gr2 <- playGame depth pos (id2, eval2) (id1, eval1)
           let wdl1 = scoreGameResult id1 gr1	-- game result from
               wdl2 = scoreGameResult id1 gr2	-- POV of id1
               wdla = addGameScores wdl  wdl1
               wdlb = addGameScores wdla wdl2
           when debug $ lift $ do
               putStrLn $ "Rez of auto play: " ++ show gr1 ++ " / " ++ show gr2
               putStrLn $ "Score so far: " ++ show wdlb
               hFlush stdout
           -- lift $ hPutStrLn ho $ show ph ++ " " ++ show sc ++ " " ++ show fts
           return (True, wdlb)

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

-- The informer is getting structured data
-- Because we do not have a Gui here, we discard the messages
startInformer :: CtxIO ()
startInformer = do
    ctx <- ask
    void $ newThread (theInformer (inform ctx))
    return ()

theInformer :: Chan InfoToGui -> CtxIO ()
theInformer ichan = forever $ void $ liftIO $ readChan ichan

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
              (msc, path) <- iterativeDeepening d
              if null path
                 then do
                     when (i>0) $ ctxLog LogError $ "Empty path when playing"
                     return Nothing	-- should not happen when i > 0
                 else case msc of
                          Nothing -> do
                              when (i>0) $ ctxLog LogError $ "No score when playing"
                              return Nothing
                          Just sc -> do
                              let j = i+1
                                  m = head path
                              ctxLog LogInfo $ "Real move " ++ show j ++ ": " ++ show m
                              chg   <- readChanging
                              sfin' <- execCState (doRealMove m) (crtStatus chg)
                              let p = head $ stack sfin'
                              if sc == 19999	-- mate in 1
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

-- Play the given position to the end using fixed depth with 2 configurations
-- This function can be used only to optimize eval weights but not time or search parameters
-- The result score is from White p.o.v.:
--  0: remis
-- +1: white wins
-- -1: black wins
playGame :: Int -> MyPos -> (String, EvalState) -> (String, EvalState) -> CtxIO GameResult
playGame d pos (ide1, eval1) (ide2, eval2) = do
    ctxLog LogInfo $ "Setup new game between " ++ ide1 ++ " and " ++ ide2
    chg <- readChanging
    let crts = crtStatus chg
    (hash1, hash2, hist0) <- liftIO $ do
        freeCache $ hash crts
        -- Alloc only one history, as this will be anyway re-allocated for every move
        (,,) <$> newCache 1 <*> newCache 1 <*> newHist
    let state1 = posToState pos hash1 hist0 eval1
        state2 = posToState pos hash2 hist0 eval2
        color1 = moving pos
        color2 = other color1
        -- Maybe set realPly? (what is this good for if not for time management?)
        chg1 = chg { crtStatus = state1, forGui = Nothing, srchStrtMs = 0,
                     myColor = color1, totBmCh = 0, lastChDr = 0, lmvScore = Nothing }
        chg2 = chg { crtStatus = state2, forGui = Nothing, srchStrtMs = 0,
                     myColor = color2, totBmCh = 0, lastChDr = 0, lmvScore = Nothing }
    ctxLog LogInfo $ "Color for " ++ ide1 ++ ": " ++ show color1
    ctxLog LogInfo $ "Starting position: " ++ posToFen pos
    go (0::Int) (ide1, chg1) (ide2, chg2)
    where go i (id1, chg1) (id2, chg2) = do
              start  <- asks startSecond
              currms <- lift $ currMilli start
              let j = i + 1
              -- Prepare for chg1 to search:
              modifyChanging $ const chg1 { forGui = Nothing, srchStrtMs = currms,
                                            totBmCh = 0, lastChDr = 0 }
              ctxLog LogInfo $ "Real ply " ++ show j ++ " engine " ++ id1
              -- Search to depth:
              (msc, path) <- iterativeDeepening d
              ctxLog LogInfo $ "Real ply " ++ show j ++ " returns " ++ show msc ++ " / " ++ show path
              if null path
                 then do
                     ctxLog LogError $ "Empty path when playing"
                     return $ GameAborted "Empty path when playing"
                 else case msc of
                          Nothing -> do
                              ctxLog LogError $ "No score when playing"
                              return $ GameAborted "No score when playing"
                          Just sc -> do
                              let m = head path
                              ctxLog LogInfo $ "Real move " ++ show j ++ " from " ++ id1 ++ ": " ++ show m
                              chg1f <- readChanging
                              s1fin <- execCState (doRealMove m) (crtStatus chg1f)
                              s2ini <- execCState (doRealMove m) (crtStatus chg2)
                              let p = head $ stack s1fin
                              -- Using length path here could be a problem
                              -- in case of a TT cut which is on path
                              if | sc == mateScore && length path == 1	-- mate in 1
                                   -> if tacticalPos p
                                         then do
                                             ctxLog LogInfo $ "Mate (" ++ id1 ++ " wins)"
                                             return $ GameWin id1 "Mate"
                                         else do
                                             ctxLog LogError $ "Mate announced, but not in check!"
                                             return $ GameAborted "Mate announced, but not in check"
                                 | remis50Moves p -> do
                                      ctxLog LogInfo $ "Remis 50 moves"
                                      return $ GameRemis "Remis 50 moves rule"
                                 | remis3Repetitions p $ stack s1fin -> do
                                      ctxLog LogInfo $ "Remis 3 repetitions"
                                      return $ GameRemis "Remis 3 repetitions rule"
                                 | sc == 0 && length path == 1 -> do	-- this should be patt
                                      ctxLog LogInfo $ "Remis (patt)"
                                      return $ GameRemis "Remis (patt)"
                                 | noMatingMaterial p -> do
                                      ctxLog LogInfo $ "Remis no mating material"
                                      return $ GameRemis "Remis no mating material"
                                 | otherwise -> do
                                      hi <- liftIO newHist
                                      let state2 = s2ini { hist = hi, mstats = ssts0 }
                                          chg1n  = chg1f { crtStatus = s1fin }
                                          chg2n  = chg2  { crtStatus = state2 }
                                      go j (id2, chg2n) (id1, chg1n)

iterativeDeepening :: Int -> CtxIO (Maybe Int, [Move])
iterativeDeepening depth = do
    --when debug $ lift $ do
    --    putStrLn $ "In iter deep: " ++ show depth
    --    hFlush stdout
    chg <- readChanging
    go 1 (crtStatus chg) Nothing [] []
    where go d _    lsc lpv _    | d > depth = return (lsc, lpv)
          go d sini lsc lpv rmvs = do
              --when debug $ lift $ do
              --    putStrLn $ "In iter deep go: " ++ show d
              --    hFlush stdout
              (path, sc, rmvsf, _timint, sfin, _) <- bestMoveCont d 0 sini lsc lpv rmvs
              if null path
                 then return (Just sc, path)
                 else go (d+1) sfin (Just sc) path rmvsf

-- Append error info to error file:
collectError :: SomeException -> IO ()
collectError e = handle cannot $ do
    let efname = "Barbarossa_collected_errors.txt"
    TOD tm _ <- getClockTime
    ef <- openFile efname AppendMode
    hPutStrLn ef $ show tm ++ " selfplay: " ++ show e
    hClose ef
    where cannot :: IOException -> IO ()
          cannot _ = return ()

data GameResult = GameAborted String
                | GameWin String String
                | GameRemis String
                deriving Show

scoreGameResult :: String -> GameResult -> (Int, Int, Int)
scoreGameResult player (GameWin plwin _)
    | player == plwin = (1, 0, 0)
    | otherwise       = (0, 0, 1)
scoreGameResult _ (GameRemis _) = (0, 1, 0)
scoreGameResult _ _             = (0, 0, 0)

addGameScores :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addGameScores (w1, d1, l1) (w2, d2, l2) = (w1+w2, d1+d2, l1+l2)

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
