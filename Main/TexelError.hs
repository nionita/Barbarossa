{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
-- import Control.Monad
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Control.Concurrent
-- import Control.Applicative ((<$>))
-- import Data.List (intersperse, delete, isPrefixOf, stripPrefix, foldl')
import Data.Bits ((.|.), (.&.), popCount)
import Data.List (intersperse, foldl')
import Data.List.Split (splitOn)
-- import Data.Maybe
-- import Data.Monoid
-- import Data.ByteString (ByteString(..))
import qualified Data.ByteString as B
import qualified Data.Serialize as S
import Data.Serialize.Get
-- import Network
-- import Numeric (showHex)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.FilePath
import System.Directory
import System.IO
-- import System.Time

import Struct.Struct
import Struct.Status
import Struct.Context
import Struct.Config
-- import Hash.TransTab
-- import Moves.BaseTypes
-- import Search.AlbetaTypes
-- import Moves.Base
-- import Moves.Board (posFromFen, initPos)
-- import Moves.History
-- import Moves.Notation
-- import Search.CStateMonad (runCState)
import Eval.FileParams (makeEvalState)
import Eval.Eval (posEval)
import Moves.Fen (posFromFen)

data Options = Options {
        optConfFile :: Maybe String,	-- config file
        optParams   :: [String],	-- list of eval parameter assignements
        optLogging  :: LogLevel,	-- logging level
        optNThreads :: Int,		-- number of threads
        optAFenFile :: Maybe FilePath,	-- annotated fen file for self analysis
        optConvert  :: Bool,	        -- convert epd to bin
        optServMode :: Bool,		-- run as server
        optWorkDir  :: Maybe FilePath	-- working dir
    }

defaultOptions :: Options
defaultOptions = Options {
        optConfFile = Nothing,
        optParams   = [],
        optLogging  = DebugUci,
        optNThreads = 1,
        optAFenFile = Nothing,
        optConvert  = False,
        optServMode = False,
        optWorkDir = Nothing
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

addConvert :: Options -> Options
addConvert opt = opt { optConvert  = True }

addServ :: Options -> Options
addServ opt = opt { optServMode = True }

addWDir :: String -> Options -> Options
addWDir cl opt = opt { optWorkDir = Just cl }

options :: [OptDescr (Options -> Options)]
options = [
        Option "c" ["config"]  (ReqArg setConfFile "STRING") "Configuration file",
        Option "l" ["loglev"]  (ReqArg setLogging "STRING")  "Logging level from 0 (debug) to 5 (never)",
        Option "p" ["param"]   (ReqArg addParam "STRING")    "Eval/search/time parameters: name=value,...",
        Option "a" ["analyse"] (ReqArg addAFile "STRING")   "Analysis file",
        Option "t" ["threads"] (ReqArg addNThrds "STRING")  "Number of threads",
        Option "o" ["convert"] (NoArg  addConvert)  "Convert epd file to bin",
        Option "s" ["server"]  (NoArg  addServ)     "Run as server",
        Option "w" ["workdir"] (ReqArg addWDir "STRING")     "Change working directory"
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
    case optWorkDir opts of
        Nothing -> return ()
        Just wd -> setCurrentDirectory wd
    case optAFenFile opts of
        Nothing -> error "EPD input file is require"
        Just fi -> do
            let ext = takeExtension fi
            if length ext == 0
                then error "Cannot process input without extension"
                else if ext == ".epd"
                        then if optConvert opts
                                then epdToBin fi
                                else scoreAllFile fi (optConfFile opts) (optParams opts) False
                        else if ext == ".bin"
                                then scoreAllFile fi (optConfFile opts) (optParams opts) True
                                else error $ "Cannot process input with extension " ++ ext

scoreAllFile :: FilePath -> Maybe String -> [String] -> Bool -> IO ()
scoreAllFile fi mconf params bin = do
    let paramList
            | null params = []
            | otherwise   = stringToParams $ concat $ intersperse "," params
    (_, es)   <- makeEvalState mconf paramList "progver" "progsuf"
    if bin
       then optFromBinFile fi es
       else optFromEpdFile fi es

optFromEpdFile :: FilePath -> EvalState -> IO ()
optFromEpdFile fi es = do
    putStrLn $ "Optimizing over " ++ fi
    (cou, err) <- foldl' accumErrorCnt (0, 0) . map (posError es . makePosVal) . lines <$> readFile fi
    let ave = sqrt(err / fromIntegral cou)
    putStrLn $ "Texel error (cnt/sum/avg): " ++ show cou ++ " / " ++ show err ++ " / " ++ show ave

accumErrorCnt :: (Int, Double) -> Double -> (Int, Double)
accumErrorCnt (cnt, acc) err = (cnt + 1, err + acc)

epdToBin :: FilePath -> IO ()
epdToBin fi = do
    let fo = addExtension (dropExtension fi) ".bin"
    putStrLn $ "Transform " ++ fi ++ " to " ++ fo
    h <- openFile fo WriteMode
    readFile fi >>= \cnt -> return (map (S.encode . makePosVal) (lines cnt)) >>= mapM_ (B.hPut h)
    hClose h

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

-- Use the incremental interface of the Get monad
optFromBinFile :: FilePath -> EvalState -> IO ()
optFromBinFile fi es = do
    putStrLn $ "Optimizing over " ++ fi
    h <- openFile fi ReadMode
    (cou, err) <- go (0::Int) 0 B.empty h
    let ave = sqrt(err / fromIntegral cou)
    putStrLn $ "Texel error (cnt/sum/avg): " ++ show cou ++ " / " ++ show err ++ " / " ++ show ave
    hClose h
    where bufsize = 1024 * 8
          iterGet result h =
              case result of
                  Fail msg _   -> error msg
                  Partial cont -> do
                      nbs <- B.hGet h bufsize
                      iterGet (cont nbs) h
                  Done r rbs   -> return (r, rbs)
          go !cnt !err bs h = do
             (diri, bsne) <- if bs == B.empty
                                then do
                                    bs1 <- B.hGet h bufsize
                                    return (True, bs1)
                                else return (False, bs)
             if diri && bsne == B.empty
                then return (cnt, err)
                else do
                    let result = runGetPartial S.get bsne
                    (r, rbs) <- iterGet result h
                    go (cnt + 1) (err + posError es r) rbs h

-- Calculate evaluation error for one position
-- The game result is in val and it is from white point of view
-- Our static score is from side to move point of view, so we have to change sign if black is to move
posError :: EvalState -> (MyPos, Double) -> Double
posError es (pos, val) = vdiff * vdiff / complexity pos
    where !stc = posEval pos es
          !myval | moving pos == White =   logisticFunction stc
                 | otherwise           = - logisticFunction stc
          vdiff = val - myval

-- For complex positions with many tactical moves we cannot expect the eval to be very accurate
-- To take this into account we calculate the number of attacks in a position and diminuate the
-- error more when this number is bigger
complexity :: MyPos -> Double
complexity pos = 1 + coeff * fromIntegral atcs
    where atcs = popCount $ (myAttacs pos .&. yo pos) .|. (yoAttacs pos .&. me pos)
          coeff = 0.1

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
