{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
-- import Control.Exception
-- import Data.Array.Unboxed
import Data.Foldable (foldrM)
import Data.List (intersperse, delete)
import Data.Maybe
-- import Data.Typeable
import System.Console.GetOpt
import System.Environment (getArgs)
-- import System.IO
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
-- import Moves.Moves (movesInit)
import Moves.Board (posFromFen, initPos)
import Moves.History
import Search.CStateMonad (runCState)
-- import Eval.Eval (weightNames)
import Eval.FileParams (makeEvalState)

data Options = Options {
        optConfFile :: Maybe String,	-- config file
        optParams   :: [String],	-- list of eval parameter assignements
        optLogging  :: LogLevel,	-- logging level
        optAFenFile :: Maybe FilePath	-- annotated fen file for self analysis
    }

defaultOptions :: Options
defaultOptions = Options {
        optConfFile = Nothing,
        optParams   = [],
        optLogging  = DebugUci,
        optAFenFile = Nothing
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

addAFile :: FilePath -> Options -> Options
addAFile fi opt = opt { optAFenFile = Just fi }

options :: [OptDescr (Options -> Options)]
options = [
        Option "c" ["config"] (ReqArg setConfFile "STRING") "Configuration file",
        Option "l" ["loglev"] (ReqArg setLogging "STRING")  "Logging level from 0 (debug) to 5 (never)",
        Option "p" ["param"]  (ReqArg addParam "STRING")    "Eval/search/time parameters: name=value,...",
        Option "a" ["analyse"] (ReqArg addAFile "STRING")   "Analysis file"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName ++ " [-c CONF] [-l LEV] [-p name=val[,...]] [-a AFILE]"
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
        Just fi -> runReaderT (analysingMachine fi) ctx	-- the analysis mode

analysingMachine :: FilePath -> CtxIO ()
analysingMachine fi = do
    -- ctx <- ask
    -- let logFileName = progLogName ++ "-" ++ show (startSecond ctx) ++ ".log"
    -- startLogger logFileName
    -- startWriter False
    -- startInformer
    -- beforeReadLoop
    fileReader fi
    -- whatever to do when ending:
    -- beforeProgExit

stateFromFen :: Pos -> Cache -> History -> EvalState -> MyState
stateFromFen StartPos  c h es = posToState initPos c h es
stateFromFen (Pos fen) c h es = posToState (posFromFen fen) c h es

movingColor :: Pos -> Color
movingColor fen
    | Pos str <- fen
        = case words str of
              _ : (c:_) : _ -> case c of
                                 'w' -> White
                                 'b' -> Black
                                 _   -> error $ "Wrong fen: " ++ str
              _ -> error $ "Wrong fen: " ++ str
    | otherwise = White     -- startposition

data Agreg = Agreg {
         agrCumErr :: !Double,	-- accumulated error
         agrFenOk  :: !Int	-- number of fens analysed
     } deriving Show

-- The file reader reads an annotated analysis file
-- and analyses every fen, cummulating the error
-- and reporting it
fileReader :: FilePath -> CtxIO ()
fileReader fi = do
    inp <- liftIO $ readFile fi
    let header : fens = lines inp
        -- "Reference" : "depth" : sdepth : _ = words header
    lift $ putStrLn $ "Start analysing from: " ++ header
    agr <- foldrM perFenLine (Agreg 0 0) fens
    lift $ putStrLn $ show agr

perFenLine :: String -> Agreg -> CtxIO Agreg
perFenLine fenLine agr = do
    let (refmv, fen') = break ((==) '\t') fenLine
        fen = tail fen'	-- it has the \t in front
--    let pstr = "position fen " ++ fen ++ " moves " ++ refmv ++ "<<--"
    lift $ putStrLn $ "Move: " ++ refmv ++ " fen " ++ fen
{--
    let euci = parseUciStr pstr
        mv = case euci of
                 Left s    -> error $ "Wrong fen & move: " ++ show s
                 Right (Position _ mvs) -> head mvs
                 Right _   -> error "Wrong fen & move, unexpected!"
--}
    let euci = parseMoveStr refmv
        mv = case euci of
                 Left s   -> error $ "Wrong move: " ++ show s
                 Right mv -> mv
    chg <- readChanging
    let crts = crtStatus chg
        mystate = stateFromFen (Pos fen) (hash crts) (hist crts) (evalst crts)
    -- modifyChanging $ \c -> c { working = True }
    -- sc <- searchTheTree 1 dpt 0 0 0 0 Nothing [] []
    (e, _) <- runCState (searchTestPos mv) mystate
    return $ aggregateError agr e

aggregateError :: Agreg -> Double -> Agreg
aggregateError agr e
    = agr { agrCumErr = agrCumErr agr + e, agrFenOk = agrFenOk agr + 1 }

-- Some parameters (until we have a good solution)
clearHash :: Bool
clearHash = False

newThread :: CtxIO () -> CtxIO ThreadId
newThread a = do
    ctx <- ask
    liftIO $ forkIO $ runReaderT a ctx

heaviside :: Int -> Double
heaviside x = 1 / (1 + exp (a * fromIntegral x))
    where a = 0.0273

searchTestPos :: Move -> Game Double
searchTestPos m = do
    (mvs', mvs'') <- genMoves 0 0 False	-- don't need sort
    let mvs = delete m $ mvs' ++ mvs''
    s' <- searchAB m
    let s = fromJust s'
    ss <- mapM searchAB mvs
    return $! sum $ map (\x -> heaviside (s - x)) $ catMaybes ss

searchAB :: Move -> Game (Maybe Int)
searchAB m = do
    lift . lift $ putStrLn $ "--> SearchAB move: " ++ show m
    r <- doMove False m False
    case r of
        Illegal -> return Nothing
        _       -> do
            (mvs1, mvs2) <- genMoves 0 0 False
            s <- foldM searchQ minBound $ mvs1 ++ mvs2
            undoMove
            lift . lift $ putStrLn $ "<-- SearchAB: " ++ show (-s)
            return $! Just (-s)

searchQ :: Int -> Move -> Game Int
searchQ a m = do
    r <- doMove False m False
    case r of
        Illegal -> return a
        _       -> do
            s <- pvQSearch minBound (-a)
            undoMove
            return (-s)

pvQLoop :: Int -> Int -> [Move] -> Game Int
pvQLoop b = go
    where go !s []     = return s
          go !s (m:ms) = do
             (!cut, !s') <- pvQInnerLoop b s m
             if cut then return s'
                    else go s' ms

pvQInnerLoop :: Int -> Int -> Move -> Game (Bool, Int)
pvQInnerLoop b a m = do
    r <- doMove False m True
    case r of
        Illegal -> return (False, a)
        Final s -> do
            undoMove
            return $ trimax (-s) b a
        _       -> do
            s <- pvQSearch (-b) (-a)
            undoMove
            return $ trimax (-s) b a

pvQSearch :: Int -> Int -> Game Int
pvQSearch a b = do
    sta <- staticVal
    tact <- tacticalPos
    if tact
       then do
           mvs <- genTactMoves
           if null mvs
              then return minBound	-- mated
              else pvQLoop b a mvs
       else if sta >= b
               then return b
               else do	-- no delta cut
                   mvs <- genTactMoves
                   if null mvs
                      then return sta
                      else if sta > a
                              then pvQLoop b sta mvs
                              else pvQLoop b a   mvs

trimax :: Int -> Int -> Int -> (Bool, Int)
trimax s b a = if s >= b
                  then (True, b)
                  else if s > a then (False, s) else (False, a)

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
