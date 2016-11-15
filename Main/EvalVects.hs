{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import Control.Monad.Reader
import Control.Concurrent
import Data.List (intersperse)
import Data.Maybe
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
        optFOutFile :: FilePath,	-- output file for filter option
        optMinMid   :: Int,		-- minimum phase for mid parameters
        optMaxEnd   :: Int		-- maximum phase for end parameters
    }

defaultOptions :: Options
defaultOptions = Options {
        optConfFile = Nothing,
        optParams   = [],
        optLogging  = DebugUci,
        optNThreads = 1,
        optNFens    = Nothing,
        optAFenFile = "alle.epd",
        optFOutFile = "vect",
        optMinMid   = 156,
        optMaxEnd   = 100
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

addMinMid :: String -> Options -> Options
addMinMid ns opt = opt { optMinMid = read ns }

addMaxEnd :: String -> Options -> Options
addMaxEnd ns opt = opt { optMaxEnd = read ns }

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
        Option "f" ["fens"]    (ReqArg addNFens "STRING")      "Number of fens",
        Option "m" ["mid"] (ReqArg addMinMid "STRING")  "Threshold for mid",
        Option "e" ["end"] (ReqArg addMaxEnd "STRING")  "Threshold for end"
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
    runReaderT
        (filterFile (optAFenFile opts) (optFOutFile opts) (optNFens opts)
        (optMinMid opts) (optMaxEnd opts)) ctx

filterFile :: FilePath -> FilePath -> Maybe Int -> Int -> Int -> CtxIO ()
filterFile fi fo mn mimi maen = do
    chg <- readChanging
    let crts = crtStatus chg
        fom = fo ++ ".mid"
        foe = fo ++ ".end"
        mfi = markerEval (evalst crts) initPos
    lift $ do
        putStrLn $ "Vectorizing " ++ fi
        putStrLn $ "Mid results to  " ++ fom
        putStrLn $ "End results to  " ++ foe
        putStrLn $ "Marker: " ++ show mfi
    hm <- liftIO $ openFile fom WriteMode
    he <- liftIO $ openFile foe WriteMode
    hi <- liftIO $ openFile fi ReadMode
    loopCount $ makeMovePos crts hi hm he mn mimi maen
    liftIO $ do
        hClose hm
        hClose he
        hClose hi

loopCount :: Monad m => (Int -> m Bool) -> m ()
loopCount act = go 0
    where go k = do
              r <- act k
              when r $ go (k+1)

makeMovePos :: MyState -> Handle -> Handle -> Handle -> Maybe Int -> Int -> Int -> Int -> CtxIO Bool
makeMovePos crts hi hm he mn mimi maen k = do
    end <- case mn of
               Nothing -> lift $ hIsEOF hi
               Just n  -> if k < n then lift $ hIsEOF hi else return True
    if end
       then return False
       else do
           fen <- lift $ hGetLine hi
           let pos = posFromFen fen
               mystate = posToState pos (hash crts) (hist crts) (evalst crts)
           mfs <- runCState (searchTestPos (evalst crts)) mystate
           case fst mfs of
               Nothing       -> return ()
               Just (f0, fi) -> liftIO $ do
                   when (phase f0 >= mimi) $ do
                       hPutStrLn hm $ serialize $ midPhase f0
                       mapM_ (hPutStrLn hm . (("- " ++) . serialize . midPhase)) fi
                   when (phase f0 <= maen) $ do
                       hPutStrLn he $ serialize $ endPhase f0
                       mapM_ (hPutStrLn he . (("- " ++) . serialize . endPhase)) fi
           return True

serialize :: Feats -> String
serialize (Feats ph fts) = "Feats " ++ show ph ++ " " ++ show fts

midPhase :: Feats -> Feats
midPhase = id

endPhase :: Feats -> Feats
endPhase (Feats ph fts) = Feats (256-ph) fts

{--
serialize :: [Int] -> String
serialize is = "Feats " ++ show is

midPhase :: Feats -> [Int]
midPhase (Feats ph fts) = map (* ph) fts

endPhase :: Feats -> [Int]
endPhase (Feats ph fts) = map (* (256-ph)) fts
--}

phase :: Feats -> Int
phase (Feats ph _) = ph

-- Some utilities:
debugMes, logmes :: String -> Game ()
logmes = lift . lift . putStrLn
debugMes _ = return ()

dumpMove :: Move -> String
dumpMove m@(Move w) = show m ++ " (0x" ++ showHex w ")"

-- We analyse the current position and the positions after every legal move
-- Analyse in this context means:
-- extract the features from the position and the descendants
-- We write the features of the basic position, followed by number of nodes,
-- followed by features of every node, in binary format, to the output file
-- The optimisation procedure (done in a different programm) will minimise the function
--
-- e(x) = sum (abs (x * f0 + min (x * fi)))
--
-- minimum is done for all children of position f0, sum is done over all positions
-- x is the parameter vector, * is scalar product
-- Because we have mid & end weights, we also need to write the game phase!
-- And also take care of it when calculating the scores
searchTestPos :: EvalState -> Game (Maybe (Feats, [Feats]))
searchTestPos est = do
    mvs <- uncurry (++) <$> genMoves 1
    -- We ignore positions with more than 63 descendents, cause we want to write a fix
    -- file format in the output with max 64 vectors (f0, f1, ..., f63)
    -- Ok, sometimes there are pseudo-legal move there, for simplity we ignore this
    if length mvs > 63
       then return Nothing
       else do
           f0 <- featsEval est <$> getPos
           fi <- map (featsEval est) . catMaybes <$> mapM movPos mvs
           return $ Just (f0, fi)

movPos :: Move -> Game (Maybe MyPos)
movPos m = do
    debugMes $ "  --> movPos move: " ++ show m
    r <- doMove m False
    case r of
        Illegal -> return Nothing
        Final _ -> do	-- this would be a remis
            undoMove	-- we don't want to mess with it
            return Nothing
        _       -> do
            p <- getPos
            undoMove
            return $ Just p

{--
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
