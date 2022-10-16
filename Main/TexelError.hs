{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent
import Control.Applicative ((<$>))
import Data.List (intersperse, delete, isPrefixOf, stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Monoid
-- import Network
import Numeric (showHex)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO
-- import System.Time

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
import Moves.Notation
import Search.CStateMonad (runCState)
import Eval.FileParams (makeEvalState)
import Eval.Eval (posEval)
import Moves.Fen (posFromFen)

data Options = Options {
        optConfFile :: Maybe String,	-- config file
        optParams   :: [String],	-- list of eval parameter assignements
        optLogging  :: LogLevel,	-- logging level
        optNThreads :: Int,		-- number of threads
        optAFenFile :: Maybe FilePath,	-- annotated fen file for self analysis
        optFOutFile :: Maybe FilePath,	-- output file for filter option
        optServMode :: Bool,		-- run as server
        optClientOf :: [String]		-- run as client with given servers
    }

defaultOptions :: Options
defaultOptions = Options {
        optConfFile = Nothing,
        optParams   = [],
        optLogging  = DebugUci,
        optNThreads = 1,
        optAFenFile = Nothing,
        optFOutFile = Nothing,
        optServMode = False,
        optClientOf = []
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

addAFile :: FilePath -> Options -> Options
addAFile fi opt = opt { optAFenFile = Just fi }

addOFile :: FilePath -> Options -> Options
addOFile fi opt = opt { optFOutFile = Just fi }

addServ :: Options -> Options
addServ opt = opt { optServMode = True }

addHost :: String -> Options -> Options
addHost cl opt = opt { optClientOf = cl : optClientOf opt }

options :: [OptDescr (Options -> Options)]
options = [
        Option "c" ["config"] (ReqArg setConfFile "STRING") "Configuration file",
        Option "l" ["loglev"] (ReqArg setLogging "STRING")  "Logging level from 0 (debug) to 5 (never)",
        Option "p" ["param"]  (ReqArg addParam "STRING")    "Eval/search/time parameters: name=value,...",
        Option "a" ["analyse"] (ReqArg addAFile "STRING")   "Analysis file",
        Option "t" ["threads"] (ReqArg addNThrds "STRING")  "Number of threads",
        Option "f" ["filter"] (ReqArg addOFile "STRING")    "Filter output file",
        Option "s" ["server"] (NoArg  addServ          )    "Run as server",
        Option "h" ["hosts"]  (ReqArg addHost "STRING")     "Run as client with list of comma separated servers"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName
              ++ " [-c CONF] [-l LEV] [-p name=val[,...]] [-a AFILE [-f OFILE | -s | h host,host,...]"
          idName = "MMTO"

{--
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
            prvMvInfo = Nothing
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
--}

-- Client/server works only for analysis, filter is always local
main :: IO ()
-- main = withSocketsDo $ do
main = do
    (opts, _) <- theOptions
    case optAFenFile opts of
        Nothing -> error "EPD input file is require"
        Just fi -> do
            let paramList
                    | null (optParams opts) = []
                    | otherwise             = stringToParams $ concat $ intersperse "," $ optParams opts
            (_, es)   <- makeEvalState (optConfFile opts) paramList "progver" "progsuf"
            optFromFile fi es

optFromFile :: FilePath -> EvalState -> IO ()
optFromFile fi es = do
    inp <- readFile fi
    putStrLn $ "Optimizing over " ++ fi
    let pvs = map makePosVal (lines inp)
        err = sum $ map (posError es) pvs
        cou = length pvs
        ave = sqrt(err / fromIntegral cou)
    putStrLn $ "Texel error (cnt/sum/avg): " ++ show cou ++ " / " ++ show err ++ " / " ++ show ave

{--

filterFile :: FilePath -> FilePath -> CtxIO ()
filterFile fi fo = do
    inp <- liftIO $ readFile fi
    lift $ putStrLn $ "Filtering " ++ fi
    chg <- readChanging
    let crts = crtStatus chg
    h <- liftIO $ openFile fo WriteMode
    mapM_ (makeMovePos crts (Just h)) $ lines inp
    liftIO $ hClose h

showAgr :: Agreg -> IO ()
showAgr agr = do
    putStrLn $ "Function value: " ++ show (agrCumErr agr)
    putStrLn $ "Total nodes:    " ++ show (agrCumNds agr)

-- Agregate function values in parallel by many threads
parallelAgregate :: [a] -> (MVar Agreg -> a -> CtxIO ()) -> CtxIO Agreg
parallelAgregate es act = do
    vs <- forM es $ \e -> do
        v <- lift $ newEmptyMVar
        void $ newThread $ act v e
        return v
    as <- forM vs $ \v -> liftIO $ takeMVar v
    return $! foldr mappend mempty as

-- For client mode we need to know which hosts & port we should use
myServerPort :: PortID
myServerPort = PortNumber 4321

serverPrefix :: String
serverPrefix = "Server value: "

endOfParams :: String
endOfParams = "EOP"


clientMode :: [String] -> [String] -> CtxIO ()
clientMode hostsstrs params = do
    let hosts = splitOn "," $ concat $ intersperse "," hostsstrs
    liftIO $ setNumCapabilities $ length hosts + 1
    agr <- parallelAgregate hosts (askServer params)
    liftIO $ showAgr agr
    -- liftIO $ putStrLn $ "Function value: " ++ show r

-- Currently only parameters on command line will be sent over
-- Config file is not supported over network!!
askServer :: [String] -> MVar Agreg -> HostName -> CtxIO ()
askServer params mvar host = liftIO $ do
    h <- connectTo host myServerPort
    hSetBuffering h LineBuffering
    forM_ params $ \ps -> hPutStrLn h ps
    hPutStrLn h endOfParams
    resline <- lineUntil False h (isPrefixOf serverPrefix)
    let vs = maybe nl (splitOn " ") $ stripPrefix serverPrefix resline
        vd:vn:_ = vs ++ nl
        nl = ["0", "0"]
        !d = read vd
        !n = read vn
    putMVar mvar $! Agreg { agrCumErr = d, agrCumNds = n }

serverMode :: FilePath -> Int -> CtxIO ()
serverMode fi n = do
    lift $ putStrLn $ "Optimizing over " ++ fi
    liftIO $ setNumCapabilities $ n + 1
    chg <- readChanging
    let crts = crtStatus chg
    ts <- (spread n . catMaybes) <$> (liftIO (readFile fi) >>= mapM (makeMovePos crts Nothing) . lines)
    sock <- liftIO $ listenOn myServerPort
    forever $ do
        (h, es) <- liftIO $ do
            (h, host, _) <- accept sock
            putStrLn $ "Accepting request from host " ++ host
            (params, _) <- accumLines False h (== endOfParams) (:) []
            let paramList = stringToParams $ concat $ intersperse "," params
            (_, evs) <- makeEvalState Nothing paramList "progver" "progsuf"	-- no config file
            return (h, evs)
        agr <- parallelAgregate ts (agregMVar $ Just es)
        liftIO $ hPutStrLn h $ serverPrefix ++ show (agrCumErr agr) ++ " " ++ show (agrCumNds agr)

-- Read lines until one condition occurs
lineUntil :: Bool -> Handle -> (String -> Bool) -> IO String
lineUntil verb h p = go
    where go = do l <- hGetLine h
                  when verb $ putStrLn $ "lU: " ++ l
                  if p l then return l
                         else go

-- Read lines until one condition occurs, accumulating some information from the read lines
-- (except the one with the condition, which is returned together with the accumulator)
accumLines :: Bool -> Handle -> (String -> Bool) -> (String -> a -> a) -> a -> IO (a, String)
accumLines verb h p f = go
    where go a = do
             l <- hGetLine h
             when verb $ putStrLn $ "aL: " ++ l
             if p l then return (a, l)
                    else go $! f l a

-- Spread a list over many lists
spread :: Int -> [a] -> [[a]]
spread n = go $ take n $ repeat []
    where go (l:ls) (a:as) = go (ls ++ [a:l]) as
          go acc    _      = acc

data Agreg = Agreg {
         agrCumErr :: !Double,	-- accumulated error
         agrCumNds :: !Int
     } deriving Show

instance Monoid Agreg where
    mempty = Agreg { agrCumErr = 0, agrCumNds = 0 }
    a `mappend` b = Agreg { agrCumErr = agrCumErr a + agrCumErr b,
                            agrCumNds = agrCumNds a + agrCumNds b }
--}

makePosVal :: String -> (MyPos, Double)
makePosVal fenval = (pos, val)
    where (fen:sval:_) = splitOn "," fenval
          pos = posFromFen fen
          val = read sval

-- Calculate evaluation error for one position
-- The game result is in val and it is from white point of view
-- Our static score is from side to move point of view, so we have to change sign if black is to move
posError :: EvalState -> (MyPos, Double) -> Double
posError es (pos, val) = vdiff * vdiff
    where !stc = posEval pos es
          !myval | moving pos == White =   logisticFunction stc
                 | otherwise           = - logisticFunction stc
          vdiff = val - myval

-- Logistic growth parameter is such that the probability of win for 400 cp advantage is 98%
-- More important is that the error grows much less if we miss higher score by an amount of centipawns
-- as when we miss lower scores by the same amount
logisticGrowth :: Double
logisticGrowth = 0.01

-- We chose the codomain between -1 and 1
logisticFunction :: Int -> Double
logisticFunction score = 2 / (1 + exp (-logisticGrowth * fromIntegral score)) - 1

{--
makeMovePos :: MyState -> Maybe Handle -> String -> CtxIO (Maybe (Move, MyState))
makeMovePos crts mh fenLine = do
    let (refmv:fen:_) = splitOn "\t" fenLine
        pos  = posFromFen fen
        euci = fromNiceNotation pos refmv
    case euci of
        Left s  -> do
            lift $ do
                putStrLn $ "Move: " ++ refmv ++ " fen " ++ fen
                putStrLn $ "Wrong move: " ++ show s
            return Nothing
        Right m -> do
            let mystate = posToState pos (hash crts) (hist crts) (evalst crts)
            (ok, _) <- runCState (canDoMove m) mystate
            if ok
               then do
                  case mh of
                      Nothing -> return ()
                      Just h  -> lift $ hPutStrLn h fenLine
                  return $ Just (m, mystate)
               else do
                   lift $ do
                       putStrLn $ "Move: " ++ refmv ++ " fen " ++ fen
                       putStrLn $ "Illegal move"
                   return Nothing

agregAll :: Maybe EvalState -> [(Move, MyState)] -> CtxIO Agreg
agregAll mest = case mest of
    Nothing -> foldM agregPos mempty
    Just es -> foldM agregPos mempty . map (\(m, ms) -> (m, ms { evalst = es }))

agregPos :: Agreg -> (Move, MyState) -> CtxIO Agreg
agregPos agr (m, mystate) = do
    (e, s) <- runCState (searchTestPos m) mystate
    return $! aggregateError agr e (nodes $ stats s)

agregMVar :: Maybe EvalState -> MVar Agreg -> [(Move, MyState)] -> CtxIO ()
agregMVar mest mvar mss = do
    myd <- liftIO $ myThreadId
    liftIO $ putStrLn $ "Thread " ++ show myd ++ " started"
    agr <- agregAll mest mss
    liftIO $ putMVar mvar agr
    liftIO $ putStrLn $ "Thread " ++ show myd ++ " ended"

aggregateError :: Agreg -> Double -> Int -> Agreg
aggregateError agr e n = agr { agrCumErr = agrCumErr agr + e, agrCumNds = agrCumNds agr + n }

newThread :: CtxIO () -> CtxIO ThreadId
newThread a = do
    ctx <- ask
    liftIO $ forkIO $ runReaderT a ctx
--}
