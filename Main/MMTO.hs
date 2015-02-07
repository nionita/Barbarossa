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
import Data.Foldable (foldrM)
import Data.List (intersperse, delete)
import Data.Maybe
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
import Uci.UCI
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
        optFOutFile :: Maybe FilePath	-- output file for filter option
    }

defaultOptions :: Options
defaultOptions = Options {
        optConfFile = Nothing,
        optParams   = [],
        optLogging  = DebugUci,
        optNThreads = 1,
        optAFenFile = Nothing,
        optFOutFile = Nothing
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

options :: [OptDescr (Options -> Options)]
options = [
        Option "c" ["config"] (ReqArg setConfFile "STRING") "Configuration file",
        Option "l" ["loglev"] (ReqArg setLogging "STRING")  "Logging level from 0 (debug) to 5 (never)",
        Option "p" ["param"]  (ReqArg addParam "STRING")    "Eval/search/time parameters: name=value,...",
        Option "a" ["analyse"] (ReqArg addAFile "STRING")   "Analysis file",
        Option "t" ["threads"] (ReqArg addNThrds "STRING")  "Number of threads",
        Option "f" ["filter"] (ReqArg addOFile "STRING")    "Filter output file"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName ++ " [-c CONF] [-l LEV] [-p name=val[,...]] [-a AFILE [-f OFILE]]"
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
    (parc, evs) <- makeEvalState (optConfFile opts) paramList "progver" "progsuf"
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

main :: IO ()
main = do
    (opts, _) <- theOptions
    ctx <- initContext opts
    case optAFenFile opts of
        Nothing -> error "Analyse file have to be give as parameter"
        Just fi -> case optFOutFile opts of
                       Nothing -> runReaderT (optFromFile fi (optNThreads opts)) ctx	-- optimise mode
                       Just fo -> runReaderT (filterFile fi fo) ctx

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
           vs <- forM (spread n mss) $ \ts -> do
               v <- lift $ newEmptyMVar
               newThread $ agregMVar v ts
               return v
           as <- forM vs $ \v -> liftIO $ takeMVar v
           let a = foldr (\(Agreg { agrCumErr = e }) s -> s + e) 0 as
           liftIO $ putStrLn $ "Error: " ++ show a
       else do
           liftIO $ setNumCapabilities 2
           liftIO $ putStrLn $ "Optimise with 1 thread"
           agr <- agregAll mss
           liftIO $ putStrLn $ "Error: " ++ show (agrCumErr agr)

-- Spread a list over many lists
spread :: Int -> [a] -> [[a]]
spread n = go $ take n $ repeat []
    where go acc [] = acc
          go (l:ls) (a:as) = go (ls ++ [a:l]) as

data Agreg = Agreg {
         agrCumErr :: !Double	-- accumulated error
     } deriving Show

makeMovePos :: MyState -> Maybe Handle -> String -> CtxIO (Maybe (Move, MyState))
makeMovePos crts mh fenLine = do
    let (refmv, fen') = break ((==) '\t') fenLine
        fen = tail fen'	-- it has the \t in front
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
            case mh of
                Nothing -> return ()
                Just h  -> lift $ hPutStrLn h fenLine
            return $ Just (m, mystate)

agregAll :: [(Move, MyState)] -> CtxIO Agreg
agregAll = foldM agregPos Agreg { agrCumErr = 0 }

agregPos :: Agreg -> (Move, MyState) -> CtxIO Agreg
agregPos agr (m, mystate) = do
    (e, _) <- runCState (searchTestPos m) mystate
    return $! aggregateError agr e

agregMVar :: MVar Agreg -> [(Move, MyState)] -> CtxIO ()
agregMVar mvar mss = do
    myd <- liftIO $ myThreadId
    liftIO $ putStrLn $ "Thread " ++ show myd ++ " started"
    agr <- agregAll mss
    liftIO $ putMVar mvar agr
    liftIO $ putStrLn $ "Thread " ++ show myd ++ " ended"

aggregateError :: Agreg -> Double -> Agreg
aggregateError agr e = agr { agrCumErr = agrCumErr agr + e }

-- Some parameters (until we have a good solution)
clearHash :: Bool
clearHash = False

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

searchTestPos :: Move -> Game Double
searchTestPos m = do
    -- m <- flip correctMove m' <$> getPos	-- conform new coding
    mvs' <- uncurry (++) <$> genMoves 0 0 False	-- don't need sort
    let mvs = delete m mvs'
    -- logmes $ "Pref move: " ++ dumpMove m
    -- forM_ mvs $ \e -> logmes $ "rest move: " ++ dumpMove e
    s' <- searchAB m
    let s = fromJust s'
    ss <- mapM searchAB mvs
    return $! sum $ map (\x -> heaviside (s - x)) $ catMaybes ss

searchAB :: Move -> Game (Maybe Int)
searchAB m = do
    debugMes $ "--> SearchAB move: " ++ show m
    r <- doMove False m False
    case r of
        Illegal -> return Nothing
        _       -> do
            -- (mvs1, mvs2) <- uncurry (++) <$> genMoves 0 0 False
            -- s <- negate <$> foldM searchQ minBound $ mvs1 ++ mvs2
            mvs <- uncurry (++) <$> genMoves 0 0 False
            s <- negate <$> foldM searchQ minBound mvs
            undoMove
            debugMes $ "--> SearchAB move: " ++ show m ++ " score = " ++ show s
            return $! Just s

searchQ :: Int -> Move -> Game Int
searchQ !a m = do
    debugMes $ "  --> SearchQ move: " ++ show m ++ " (" ++ show a ++ ")"
    r <- doMove False m False
    case r of
        Illegal -> return a
        _       -> do
            !s <- negate <$> pvQSearch 3 minBound maxBound
            undoMove
            let !a' = if s > a then s else a
            debugMes $ "  <-- SearchQ: " ++ show a'
            return a'

pvQLoop :: Int -> Int -> Int -> [Move] -> Game Int
pvQLoop lev b = go
    where go !s []     = return s
          go !s (m:ms) = do
             (!cut, !s') <- pvQInnerLoop lev b s m
             if cut then return s'
                    else go s' ms

spaces :: Int -> String
spaces n = take n $ repeat ' '

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
              then return $ trimax minBound a b	-- mated
              else pvQLoop lev b a mvs
       else if sta >= b
               then return b
               else do	-- no delta cut
                   mvs <- genTactMoves
                   if null mvs
                      then return $ trimax sta a b
                      else if sta > a
                              then pvQLoop lev b sta mvs
                              else pvQLoop lev b a   mvs

trimaxCut :: Int -> Int -> Int -> (Bool, Int)
trimaxCut !s !b !a = if s >= b
                     then (True, b)
                     else if s > a then (False, s) else (False, a)

trimax :: Int -> Int -> Int -> Int
trimax !s !a !b = if s > b then b else if s > a then s else a

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
