{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import Control.Monad
import Control.Monad.Reader
-- import Control.Monad.State
import Control.Concurrent
-- import Control.Applicative ((<$>))
import Data.List (intersperse)
-- import Data.List.Split (splitOn)
import Data.Maybe
import Data.Ord
-- import Data.Monoid
-- import Network
import Numeric (showHex)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO
import System.Time

import Struct.Struct
import Struct.Status
import Struct.Context
import Struct.Config
import Hash.TransTab
import Moves.BaseTypes
import Search.AlbetaTypes
import Moves.Base
import Moves.Board (posFromFen, initPos)
import Moves.History
-- import Moves.Notation
import Search.CStateMonad (runCState)
import Eval.FileParams (makeEvalState)
import Eval.Eval

data Options = Options {
        optConfFile :: Maybe String,	-- config file
        optParams   :: [String],	-- list of eval parameter assignements
        optLogging  :: LogLevel,	-- logging level
        optNThreads :: Int,		-- number of threads
        optNFens    :: Maybe Int,	-- number of fens (Nothing = all)
        optAFenFile :: FilePath,	-- fen file with usual positions
        optFOutFile :: FilePath		-- output file for filter option
    }

defaultOptions :: Options
defaultOptions = Options {
        optConfFile = Nothing,
        optParams   = [],
        optLogging  = DebugUci,
        optNThreads = 1,
        optNFens    = Nothing,
        optAFenFile = "fen.fen",
        optFOutFile = "vect.bin"
    }

setConfFile :: String -> Options -> Options
setConfFile cf opt = opt { optConfFile = Just cf }

addParam :: String -> Options -> Options
addParam pa opt = opt { optParams = pa : optParams opt }

setLogging :: String -> Options -> Options
setLogging lev opt = opt { optLogging = llev }
    where llev = case levi of
                   0 -> DebugSearch
                   1 -> DebugUci
                   2 -> LogInfo
                   3 -> LogWarning
                   4 -> LogError
                   _ -> if levi < 0 then DebugSearch else LogNever
          levi = read lev :: Int

addNThrds :: String -> Options -> Options
addNThrds ns opt = opt { optNThreads = read ns }

addNFens :: String -> Options -> Options
addNFens ns opt = opt { optNFens = Just $ read ns }

addIFile :: FilePath -> Options -> Options
addIFile fi opt = opt { optAFenFile = fi }

addOFile :: FilePath -> Options -> Options
addOFile fi opt = opt { optFOutFile = fi }

options :: [OptDescr (Options -> Options)]
options = [
        Option "c" ["config"]  (ReqArg setConfFile "STRING") "Configuration file",
        Option "l" ["loglev"]  (ReqArg setLogging "STRING")  "Logging level from 0 (debug) to 5 (never)",
        Option "p" ["param"]   (ReqArg addParam "STRING")    "Eval/search/time params: name=value,...",
        Option "i" ["input"]   (ReqArg addIFile "STRING")     "Input (fen) file",
        Option "o" ["output"]  (ReqArg addOFile "STRING")    "Output file",
        Option "t" ["threads"] (ReqArg addNThrds "STRING")  "Number of threads",
        Option "f" ["fens"]    (ReqArg addNFens "STRING")      "Number of fens"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName
              ++ " [-c CONF] [-l LEV] [-p name=val[,...]] [-a AFILE] [-f OFILE] [-t THREADS]"
          idName = "EvalVects"

initContext :: Options -> IO Context
initContext opts = do
    clktm <- getClockTime
    let llev = optLogging opts
    lchan <- newChan
    wchan  <- newChan
    ichan <- newChan
    ha <- newCache 1	-- it will take the minimum number of entries
    hi <- newHist
    let paramList = stringToParams $ concat $ intersperse "," $ optParams opts
    -- putStrLn "Before eval state"
    (parc, evs) <- makeEvalState (optConfFile opts) paramList "progver" "progsuf"
    -- putStrLn "eval state:"
    -- putStrLn $ show evs
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
            loglev = llev,
            evpid  = parc,
            tipars = npSetParm (colParams paramList :: CollectFor TimeParams)
         }
    return context

-- Client/server works only for analysis, filter is always local
main :: IO ()
main = do
    (opts, _) <- theOptions
    ctx <- initContext opts
    runReaderT (filterFile (optAFenFile opts) (optFOutFile opts) (optNFens opts)) ctx

filterFile :: FilePath -> FilePath -> Maybe Int -> CtxIO ()
filterFile fi fo mn = do
    inp <- liftIO $ readFile fi
    lift $ do
        putStrLn $ "Vectorizing " ++ fi
        putStrLn $ "Results to  " ++ fo
    chg <- readChanging
    let crts = crtStatus chg
    h <- liftIO $ openFile fo WriteMode
    case mn of
        Nothing -> mapM_ (makeMovePos crts (Just h)) $ lines inp
        Just n  -> mapM_ (makeMovePos crts (Just h)) $ take n $ lines inp
    liftIO $ hClose h

makeMovePos :: MyState -> Maybe Handle -> String -> CtxIO ()
makeMovePos crts mh fen = do
    let pos  = posFromFen fen
        mystate = posToState pos (hash crts) (hist crts) (evalst crts)
    mfs <- runCState (searchTestPos (evalst crts)) mystate
    case fst mfs of
        Nothing       -> return ()
        Just (f0, fi) -> case mh of
            Nothing -> liftIO $ do
                putStrLn $ serialize f0
                mapM_ (putStrLn . (("  - " ++) . serialize)) fi
            Just h  -> liftIO $ do
                hPutStrLn h $ serialize f0
                mapM_ (hPutStrLn h . (("  - " ++) . serialize)) fi

serialize :: Feats -> String
serialize = show

-- Some utilities:
debugMes, logmes :: String -> Game ()
logmes = lift . lift . putStrLn
-- debugMes = lift . lift . putStrLn
debugMes _ = return ()

dumpMove :: Move -> String
dumpMove m@(Move w) = show m ++ " (0x" ++ showHex w ")"

-- We analyse the current position and the positions after every legal move
-- Analise in this context means:
-- Make a QS without any pruning and return the best score together with the position
-- which got it, then extract the features from the position
-- We write the features of the basic position, followed by number of nodes,
-- followed by features of every node, in binary format, to the output file
-- The optimisation procedure (done in a different programm) will minimise the function
--
-- e(x) = sum (abs (x * f0 + min (x * fi)))
--
-- minimum is done for all children of position f0, sum is done over all position
-- x is the parameter vector, * is scalar product
searchTestPos :: EvalState -> Game (Maybe (Feats, [Feats]))
searchTestPos est = do
    mvs <- uncurry (++) <$> genMoves
    -- We ignore positions with more than 63 descendents, cause we want to write a fix
    -- file format in the output with max 64 vectors (f0, f1, ..., f63)
    -- Ok, sometimes there are pseudo-legal move there, for simplity we ignore this
    if length mvs > 63
       then return Nothing
       else do
    forM_ mvs $ \e -> debugMes $ "move: " ++ dumpMove e
    -- n0 <- gets $ nodes . stats
    f0 <- featsEval est . pos <$> pvQSearch 0 minBound maxBound
    fi <- map (featsEval est . pos) . catMaybes <$> mapM searchQ mvs
    -- n1 <- gets $ nodes . stats
    return $ Just (f0, fi)

{--
searchAB :: Move -> Game (Maybe Int)
searchAB m = do
    debugMes $ "--> SearchAB move: " ++ show m
    r <- doMove m False
    case r of
        Illegal -> return Nothing
        _       -> do
            mvs <- uncurry (++) <$> genMoves
            !s <- negate <$> foldM searchQ minScore mvs
            undoMove
            debugMes $ "<-- SearchAB move: " ++ show m ++ " score = " ++ show s
            return $ Just s
--}

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
