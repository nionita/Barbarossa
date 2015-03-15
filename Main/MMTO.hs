{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Applicative ((<$>))
-- import Control.Exception
-- import Data.Foldable (foldrM)
import Data.List (intersperse, delete, isPrefixOf, stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Monoid
import Network
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
-- import Uci.UCI
import Moves.BaseTypes
import Search.AlbetaTypes
import Moves.Base
import Moves.Board (posFromFen, initPos)
import Moves.History
import Moves.Notation
import Search.CStateMonad (runCState)
import Eval.FileParams (makeEvalState)

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
        Option "h" ["hosts"]  (ReqArg addHost "STRING")     "Run as client with this list of servers, comma separated"
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

-- Client/server works only for analysis, filter is always local
main :: IO ()
main = withSocketsDo $ do
    (opts, _) <- theOptions
    ctx <- initContext opts
    let action = case optAFenFile opts of
            Nothing -> if null (optClientOf opts)
                then error "Analyse file (-a) or host list (-h) is required"
                else clientMode (optClientOf opts) (optParams opts)
            Just fi -> case optFOutFile opts of
                Just fo -> filterFile fi fo
                Nothing -> if optServMode opts
                    then serverMode fi (optNThreads opts)
                    else optFromFile fi (optNThreads opts)
    -- hSetBuffering stdout LineBuffering
    runReaderT action ctx

filterFile :: FilePath -> FilePath -> CtxIO ()
filterFile fi fo = do
    inp <- liftIO $ readFile fi
    lift $ putStrLn $ "Filtering " ++ fi
    chg <- readChanging
    let crts = crtStatus chg
    h <- liftIO $ openFile fo WriteMode
    mapM_ (makeMovePos crts (Just h)) $ lines inp
    liftIO $ hClose h

optFromFile :: FilePath -> Int -> CtxIO ()
optFromFile fi n = do
    inp <- liftIO $ readFile fi
    lift $ putStrLn $ "Optimizing over " ++ fi
    chg <- readChanging
    let crts = crtStatus chg
    mss <- catMaybes <$> mapM (makeMovePos crts Nothing) (lines inp)
    if n > 1
       then do
           liftIO $ setNumCapabilities $ n + 1
           agr <- parallelAgregate (spread n mss) (agregMVar Nothing)
           liftIO $ showAgr agr
           -- liftIO $ putStrLn $ "Function value: " ++ show r
       else do
           liftIO $ setNumCapabilities 2
           liftIO $ putStrLn $ "Optimise with 1 thread"
           agr <- agregAll Nothing mss
           liftIO $ showAgr agr

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

-- Some utilities:
debugMes, logmes :: String -> Game ()
logmes = lift . lift . putStrLn
-- debugMes = lift . lift . putStrLn
debugMes _ = return ()

dumpMove :: Move -> String
dumpMove m@(Move w) = show m ++ " (0x" ++ showHex w ")"

heaviside :: Int -> Double
heaviside x = 1 / (1 + exp (a * fromIntegral x))
    where a = 0.0273

correctMove :: MyPos -> Move -> Move
correctMove p m'
    | moveIsNormal m = moveAddPiece pc m
    | otherwise      = m
    where m = moveAddColor c $ checkCastle (checkEnPas m' p) p
          f = fromSquare m
          Busy _ pc = tabla p f
          c = moving p

canDoMove :: Move -> Game Bool
canDoMove m = do
    r <- doMove False m False
    case r of
        Illegal -> return False
        _       -> return True

-- We change the error definition and do not use the heaviside function
-- becase we dont need in our optimisation method (ASA) smooth functions
-- So we penalize just the number of moves better then our best move
-- by more then a threshold
-- We have 2 possibilities:
-- 1. the threshold can be fixed for one optimisation
-- 2. the threshold can depend on number of nodes searched in that position
-- When we take the number of searched nodes we can give less weight
-- to tensioned positions, which are more probably tactical
searchTestPos :: Move -> Game Double
searchTestPos m = do
    mvs' <- uncurry (++) <$> genMoves 0 0 False	-- don't need sort
    let mvs = delete m mvs'
    -- liftIO $ putStrLn $ "Pref move: " ++ dumpMove m
    debugMes $ "Pref move: " ++ dumpMove m
    forM_ mvs $ \e -> debugMes $ "rest move: " ++ dumpMove e
    -- We do not generate all promotions, and now have also some seldom bugs
    -- about castle, so we will ignore those cases:
    if length mvs == length mvs'
       then do
            -- p <- getPos
            -- liftIO $ do
            --     putStrLn $ "Prefered move not in move list"
            --     putStrLn $ "  Position: " ++ posToFen p
            --     putStrLn $ "  Prf.move: " ++ dumpMove m
            --     forM_ mvs $ \e -> putStrLn
            --              $ "  nrm move: " ++ dumpMove e
            return 0
       else do
           ms <- searchAB m
           case ms of
               Nothing -> do
                   -- liftIO $ putStrLn $ "Prefered move is illegal? " ++ dumpMove m
                   return 0
               Just s  -> do
                  ss <- mapM searchAB mvs
                  -- return $! sum $ map (\x -> heaviside (s - x)) $ catMaybes ss
                  let s' = s + bestThreshold
                  return $! sum $ map (\x -> if x > s' then 1 else 0) $ catMaybes ss
    where bestThreshold = 100

-- We need this so that we can negate safely:
minScore, maxScore :: Int
minScore = minBound + 2000
maxScore = maxBound - 2000

searchAB :: Move -> Game (Maybe Int)
searchAB m = do
    debugMes $ "--> SearchAB move: " ++ show m
    r <- doMove False m False
    case r of
        Illegal -> return Nothing
        _       -> do
            mvs <- uncurry (++) <$> genMoves 0 0 False
            !s <- negate <$> foldM searchQ minScore mvs
            undoMove
            debugMes $ "<-- SearchAB move: " ++ show m ++ " score = " ++ show s
            return $ Just s

searchQ :: Int -> Move -> Game Int
searchQ !a m = do
    debugMes $ "  --> SearchQ move: " ++ show m ++ " (" ++ show a ++ ")"
    r <- doMove False m False
    case r of
        Illegal -> return a
        _       -> do
            !s <- negate <$> pvQSearch 3 minScore (-a)
            undoMove
            -- let !a' = if s > a then s else a	-- this is already so
            debugMes $ "  <-- SearchQ move: " ++ show m ++ " score = " ++ show s
            return $! s

{-# INLINE pvQLoop #-}
pvQLoop :: Int -> Int -> Int -> [Move] -> Game Int
pvQLoop lev b = go
    where go !s []     = return s
          go !s (m:ms) = do
             (!cut, !s') <- pvQInnerLoop lev b s m
             if cut then return s'
                    else go s' ms

spaces :: Int -> String
spaces l = take n $ repeat ' '
    where n = l * 2

pvQInnerLoop :: Int -> Int -> Int -> Move -> Game (Bool, Int)
pvQInnerLoop lev !b !a m = do
    debugMes $ spaces lev ++ "--> pvQInnerLoop a = " ++ show a ++ " b = " ++ show b ++ " move: " ++ show m
    r <- doMove False m True
    case r of
        Illegal -> return (False, a)
        Final s -> do
            undoMove
            debugMes $ spaces lev ++ "<-- pvQInnerLoop s = -" ++ show s
            return $ trimaxCut (-s) b a
        _       -> do
            s <- negate <$> pvQSearch (lev+1) (-b) (-a)
            undoMove
            debugMes $ spaces lev ++ "<-- pvQInnerLoop s = " ++ show s
            return $ trimaxCut s b a

pvQSearch :: Int -> Int -> Int -> Game Int
pvQSearch !lev !a !b = do
    debugMes $ spaces lev ++ "--> pvQSearch a = " ++ show a ++ " b = " ++ show b
    sta <- staticVal
    tact <- tacticalPos
    if tact
       then do
           mvs <- genTactMoves
           if null mvs
              then return $ trimax minScore a b	-- mated
              else pvQLoop lev b a mvs
       else if sta >= b
               then return b
               else do	-- no delta cut
                   mvs <- genTactMoves
                   if null mvs
                      then return sta
                      else if sta > a
                              then pvQLoop lev b sta mvs
                              else pvQLoop lev b a   mvs

trimaxCut :: Int -> Int -> Int -> (Bool, Int)
trimaxCut !s !b !a
    | s >= b    = (True,  b)
    | s >  a    = (False, s)
    | otherwise = (False, a)

trimax :: Int -> Int -> Int -> Int
trimax !s !a !b
    | s >= b    = b
    | s >  a    = s
    | otherwise = a

{--
type ABPos = (Move, MyPos)
data TestPos = TP ABPos [ABPos]

-- Function to be minimised:
-- We need to calculate the objectiv function, but even more so,
-- the partial derivatives of it on the parameters, in the current point
-- We have 2 kind of problems with this:
-- First: our evaluation function is inhomogen, there are some special cases
-- (like for example finals KMK, or KBNK) which do not depend on the eval
-- parameters.
-- Second: the partial derivatives are easier for the weigths, but very complicated
-- for the parameters (like epMovingMid a.s.o.)
-- Options for first problem:
-- 1. ignore this - see what is happening (noise -> less accurate)
-- 2. do not consider such posiions: we need to filter them out
-- 3. make those eval cases also depend on parameters - objective function
--    surface gets degenerated on some portions
-- Options for second problem:
-- 1. optimise only the weights
-- 2. derivate by hand for the parameters
-- 3. Write AD functions for eval
-- For now we choose: 1 and 1 (the simplest resolution)
objFunc :: Int -> EvalState -> [Double] -> [TestPos] -> (Double, [Double])
objFunc n est pars tps = (v, ds)
    where vds = map f tps
          v = sum $ map fst vds
          ds = foldr (zipWith (+)) (repeat 0) $ map snd vds
          f (TP (_, p) mps) = (fv, dvs)
              where (psc, pfeats) = eval p
                    tt s = (t, t')
                        where x = fromIntegral $ psc - s
                              t = 1 / (1 + exp (a * x))
                              t' = a * x * exp (a * x) * t * t
                    a = 0.0273	-- why? what are acceptable values here?
                    sfs = map (eval . snd) mps
                    tts = map (tt . fst) sfs
                    fv  = sum $ map fst tt
                    dvs = foldr (zipWith (+)) (repeat 0)
                          $ zipWith (\(_, t') fs -> map (t' *) $ zipWith subtract pfeats fs) tts $
                          $ map (\l -> if null l then repeat 0 else l) $ map snd sfs
    (pars', pars'') = splitAt n pars	-- cause we have mid & end parameters
    est' = est { esIWeightsM = pars', esIWeightsE = pars'' }
    eval p = evalState (posEval p) est'
--}
