{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import Control.Monad (when)
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Control.Concurrent
-- import Control.Applicative ((<$>))
-- import Data.List (intersperse, delete, isPrefixOf, stripPrefix, foldl')
import Data.Bits
import Data.List (intercalate, foldl')
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
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
-- import Struct.Context
import Struct.Config
-- import Hash.TransTab
-- import Moves.BaseTypes
-- import Search.AlbetaTypes
-- import Moves.Base
import Moves.Fen (fenFromString)
-- import Moves.Board (posFromFen, initPos)
-- import Moves.History
import Moves.Notation
-- import Search.CStateMonad (runCState)
import Eval.FileParams (makeEvalState)
import Eval.Eval (posEval)
import Moves.Fen (posFromFen)

debug :: Bool
debug = False

data Options = Options {
        optConfFile :: Maybe String,	-- config file
        optKFactor  :: Double,		-- minimum denominator
        optParams   :: [String],	-- list of eval parameter assignements
        -- optLogging  :: LogLevel,	-- logging level
        optNThreads :: Int,		-- number of threads
        optAFenFile :: Maybe FilePath,	-- annotated fen file for self analysis
        optConvert  :: Bool,	        -- convert epd to bin
        optReverse  :: Bool,	        -- reverse fens & check eval
        optServMode :: Bool,		-- run as server
        optWorkDir  :: Maybe FilePath	-- working dir
    } deriving Show

defaultOptions :: Options
defaultOptions = Options {
        optConfFile = Nothing,
        optKFactor  = 100,	-- minimum denominator, to limit the relative error around 0 scores
        optParams   = [],
        -- optLogging  = DebugUci,
        optNThreads = 1,
        optAFenFile = Nothing,
        optConvert  = False,
        optReverse  = False,
        optServMode = False,
        optWorkDir = Nothing
    }

setConfFile :: String -> Options -> Options
setConfFile cf opt = opt { optConfFile = Just cf }

setKFactor :: String -> Options -> Options
setKFactor kf opt = opt { optKFactor = read kf }

addParam :: String -> Options -> Options
addParam pa opt = opt { optParams = pa : optParams opt }

-- setLogging :: String -> Options -> Options
-- setLogging lev opt = opt { optLogging = llev }
--     where llev = case levi of
--                    0 -> DebugSearch
--                    1 -> DebugUci
--                    2 -> LogInfo
--                    3 -> LogWarning
--                    4 -> LogError
--                    _ -> if levi < 0 then DebugSearch else LogNever
--           levi = read lev :: Int

addNThrds :: String -> Options -> Options
addNThrds ns opt = opt { optNThreads = read ns }

addAFile :: FilePath -> Options -> Options
addAFile fi opt = opt { optAFenFile = Just fi }

addConvert :: Options -> Options
addConvert opt = opt { optConvert  = True }

setReverse :: Options -> Options
setReverse opt = opt { optReverse  = True }

addServ :: Options -> Options
addServ opt = opt { optServMode = True }

addWDir :: String -> Options -> Options
addWDir cl opt = opt { optWorkDir = Just cl }

options :: [OptDescr (Options -> Options)]
options = [
        Option "c" ["config"]  (ReqArg setConfFile "STRING") "Configuration file",
        Option "k" ["kfactor"] (ReqArg setKFactor "STRING") "K Factor",
        -- Option "l" ["loglev"]  (ReqArg setLogging "STRING")  "Logging level from 0 (debug) to 5 (never)",
        Option "p" ["param"]   (ReqArg addParam "STRING")    "Eval/search/time parameters: name=value,...",
        Option "a" ["analyse"] (ReqArg addAFile "STRING")   "Analysis file",
        Option "t" ["threads"] (ReqArg addNThrds "STRING")  "Number of threads",
        Option "o" ["convert"] (NoArg  addConvert)  "Convert epd file to bin",
        Option "r" ["reverse"] (NoArg  setReverse)  "Check epd file with the reverse fens",
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
              ++ " [-c CONF] [-l LEV] [-k kfact] [-p name=val[,...]] -a AFILE [-o|-r] [-w WORKDIR]"
          idName = "BCE"

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
    let paramList = stringToParams $ intercalate "," $ optParams opts
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
    when debug $ putStrLn $ "Options: " ++ show opts
    case optWorkDir opts of
        Nothing -> return ()
        Just wd -> setCurrentDirectory wd
    case optAFenFile opts of
        Nothing -> error "EPD input file is require"
        Just fi -> do
            let ext = takeExtension fi
            if ext == ".epd"
               then if optConvert opts
                       then epdToBin fi
                       else if optReverse opts
                               then checkAllFile fi
                               else scoreAllFile fi (optKFactor opts) (optConfFile opts) (optParams opts) False
               else if ext == ".bin"
                       then scoreAllFile fi (optKFactor opts) (optConfFile opts) (optParams opts) True
                       else do
                           isdir <- doesDirectoryExist fi
                           if isdir
                              then scoreAllDirFiles fi (optKFactor opts) (optConfFile opts) (optParams opts)
                              else error $ "Cannot process input " ++ fi

checkAllFile :: FilePath -> IO ()
checkAllFile fi = do
    putStrLn $ "Check reverse fens over " ++ fi
    (_, es) <- makeEvalState Nothing [] "progver" "progsuf"
    -- probs <- catMaybes . map (checkPosRev es) . take 100 . lines <$> readFile fi
    probs <- catMaybes . map (checkPosRev es) . lines <$> readFile fi
    putStrLn "Problems:"
    mapM_ (\p -> putStrLn (show p)) probs

scoreAllFile :: FilePath -> Double -> Maybe String -> [String] -> Bool -> IO ()
scoreAllFile fi kfactor mconf params bin = do
    let paramList
            | null params = []
            | otherwise   = stringToParams $ intercalate "," params
    (_, es) <- makeEvalState mconf paramList "progver" "progsuf"
    if bin
       then optFromBinFile fi es kfactor
       else optFromEpdFile fi es kfactor

scoreAllDirFiles :: FilePath -> Double -> Maybe String -> [String] -> IO ()
scoreAllDirFiles fi kfactor mconf params = do
    let paramList
            | null params = []
            | otherwise   = stringToParams $ intercalate "," params
    (_, es) <- makeEvalState mconf paramList "progver" "progsuf"
    optFromDirBinFiles fi es kfactor

optFromEpdFile :: FilePath -> EvalState -> Double -> IO ()
optFromEpdFile fi es kfactor = do
    putStrLn $ "Optimizing over " ++ fi
    (cou, err) <- foldl' accumErrorCnt (0, 0) . map (posRegrError es kfactor . makePosVal) . lines <$> readFile fi
    let ave = err / fromIntegral cou
    putStrLn $ "BCE error (cnt/sum/avg): " ++ show cou ++ " / " ++ show err ++ " / " ++ show ave

accumErrorCnt :: (Int, Double) -> Maybe (Double, Int) -> (Int, Double)
accumErrorCnt (cnt, acc) Nothing         = (cnt,     acc      )
accumErrorCnt (cnt, acc) (Just (err, _)) = (cnt + 1, acc + err)

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
    let hosts = splitOn "," $ intercalate "," hostsstrs
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
            let paramList = stringToParams $ intercalate "," params
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
optFromBinFile :: FilePath -> EvalState -> Double -> IO ()
optFromBinFile fi es kfactor = do
    putStrLn $ "Optimizing over " ++ fi
    hFlush stdout
    h <- openFile fi ReadMode
    (cou, err) <- go (0::Int) 0 B.empty h
    let ave = err / fromIntegral cou
    putStrLn $ "BCE error (cnt/sum/avg): " ++ show cou ++ " / " ++ show err ++ " / " ++ show ave
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
                    -- Ignore evaluations with special functions
                    -- putStrLn $ "Debug: " ++ show r ++ "/" ++ show kfactor
                    let mpe = posRegrError es kfactor r
                    case mpe of
                        Nothing       -> go  cnt       err       rbs h
                        Just (er, st) -> do
                            when (debug && er > 1000000) $ do
                                putStrLn $ "Debug: " ++ posToFen (fst r)
                                putStrLn $ "Debug: " ++ show (snd r) ++ " / " ++ show st ++ " --> " ++ show er
                            go (cnt + 1) (err + er) rbs h

-- Use the incremental interface of the Get monad, for all bin files in a directory
optFromDirBinFiles :: FilePath -> EvalState -> Double -> IO ()
optFromDirBinFiles fi es kfactor = do
    putStrLn $ "Optimizing over directory " ++ fi
    allfiles <- listDirectory fi
    -- putStrLn $ "All files " ++ show allfiles
    let binfiles = filter ((".bin" ==) . takeExtension) allfiles
    -- putStrLn $ "Bin files " ++ show binfiles
    (cnt, err) <- goFile (0::Int) 0 binfiles
    let ave = err / fromIntegral cnt
    putStrLn $ "BCE error (cnt/sum/avg): " ++ show cnt ++ " / " ++ show err ++ " / " ++ show ave
    where goFile !cnt !err []     = return (cnt, err)
          goFile !cnt !err (f:fs) = do
              let fullf = fi ++ "/" ++ f
              putStrLn $ "Bin file " ++ fullf
              hFlush stdout
              h <- openFile fullf ReadMode
              (cnt1, err1) <- go cnt err B.empty h
              hClose h
              goFile cnt1 err1 fs
          bufsize = 1024 * 8
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
                    -- Ignore evaluations with special functions
                    -- putStrLn $ "Debug: " ++ show r ++ "/" ++ show kfactor
                    let mpe = posRegrError es kfactor r
                    when debug $ putStrLn $ "Debug: " ++ show mpe ++ "/" ++ show cnt ++ "/" ++ show err
                    case mpe of
                        Nothing       -> go  cnt       err       rbs h
                        Just (er, _t) -> go (cnt + 1) (err + er) rbs h

-- Calculate evaluation error for one position - binary cross entropy
-- Because this is a binary classification it is very important that we have only 2 classes of positions:
-- won and lost. The game result can be only 1 (won) and -1 (lost)
-- The game result is in val and it is from white point of view
-- Our static score is from side to move point of view, so we have to change sign if black is to move
-- We don't use for tuning special eval (i.e. a specific evaluation function, like passed pawns or so)
posError :: EvalState -> Double -> (MyPos, Double) -> Maybe Double
posError es kfactor (pos, val)
    | spec      = Nothing
    -- | val ==  1 = Just $ - log myval / complexity pos
    -- | val == -1 = Just $ - log (1 - myval) / complexity pos
    | val ==  1 = Just $ (1 - myval) * (1 - myval) / complexity pos
    | val == -1 = Just $ myval       * myval       / complexity pos
    | otherwise = error $ "Position has wrong result: " ++ show val
    where (!stc, spec) = posEval pos es
          !myval | moving pos == White =     logisticFunction stc kfactor
                 | otherwise           = 1 - logisticFunction stc kfactor

-- We chose the codomain between 0 and 1
logisticFunction :: Int -> Double -> Double
logisticFunction score kfactor = 1 / (1 + exp (-kfactor * fromIntegral score))

-- For complex positions with many tactical moves we cannot expect the eval to be very accurate
-- We consider 2 different factors for complexity:
-- - total number of pieces (probably far away from final)
-- - number of attacked pieces (position is unquiet)
complexity :: MyPos -> Double
complexity pos = 1 + atcoeff * fromIntegral atcs + pccoeff * fromIntegral pces
    where atcs = popCount $ (myAttacs pos .&. yo pos) .|.  (yoAttacs pos .&. me pos)
          pces = popCount $ occup pos
          atcoeff = 0.1
          pccoeff = 0.01

-- Calculate evaluation error for one position - regression error
-- The value is the one obtained by a search to some depth, in centipawns, from p.o.v. of side to move
-- Our static score is also from side to move point of view
-- We don't use for tuning special eval (i.e. a specific evaluation function, like passed pawns or so)
-- We take the relative squared error and bound it around 0
posRegrError :: EvalState -> Double -> (MyPos, Double) -> Maybe (Double, Int)
posRegrError es kfactor (pos, val)
    | mate || spec = Nothing
    | otherwise    = Just (erro * erro, stc)
    where (stc, spec) = posEval pos es
          diff = val - fromIntegral stc
          erro = diff / deno
          deno | val >= den_min && val <= den_max = den_max
               | otherwise                        = val
          mate = val >= 20000 || val <= -20000
          den_min = -kfactor
          den_max =  kfactor

-- Reverse a fen: white <-> black
reverseFen :: String -> String
reverseFen fen = intercalate " " [fen1r, fstmr, fcstr, fenpr, f50m]
    where fen1:fstm:fcst:fenp:f50m:_ = fenFromString fen
          fen1r = map revUpLow $ intercalate "/" $ reverse $ splitFenLines fen1
          revUpLow 'p' = 'P'
          revUpLow 'P' = 'p'
          revUpLow 'r' = 'R'
          revUpLow 'R' = 'r'
          revUpLow 'n' = 'N'
          revUpLow 'N' = 'n'
          revUpLow 'b' = 'B'
          revUpLow 'B' = 'b'
          revUpLow 'q' = 'Q'
          revUpLow 'Q' = 'q'
          revUpLow 'k' = 'K'
          revUpLow 'K' = 'k'
          revUpLow x   = x
          -- This should be a split:
          splitFenLines s = l : if s' == "" then [] else splitFenLines (tail s') where (l, s') = break ((==) '/') s
          fstmr | fstm == "w" = "b"
                | fstm == "b" = "w"
                | otherwise   = error $ "Wrong sinde to move: " ++ show fstm
          fcstr = map revUpLow fcst
          fenpr | f:r:_ <- fenp, f `elem` "abcdefgh", r `elem` "36"
                            = f : reve r : []
                | otherwise = fenp
          reve '3' = '6'
          reve '6' = '3'
          reve x   = x

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

checkPosRev :: EvalState -> String -> Maybe (String, Int, String, Int)
checkPosRev es fenval
    | eo == er  = Nothing
    | otherwise = Just (fen, eo, fenr, er)
    where (fen:_) = splitOn "," fenval
          pos  = posFromFen fen
          !fenr = reverseFen fen
          !posr = posFromFen fenr
          (!eo, _) = posEval pos  es
          (!er, _) = posEval posr es
