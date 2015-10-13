{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Exception
-- import Data.Array.Unboxed
-- import Data.Foldable (foldrM)
import Data.List (intersperse)
-- import Data.Maybe
import Data.Typeable
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
import Uci.UciGlue
import Moves.Base
-- import Moves.Moves (movesInit)
import Moves.Board (posFromFen, initPos)
import Moves.History
import Search.CStateMonad (execCState)
import Eval.Eval (gamePhase)
import Eval.FileParams (makeEvalState)

data Options = Options {
        optParams   :: [String],	-- list of eval parameter assignements
        optAFenFile :: Maybe FilePath,	-- annotated fen file for analysis
        optOFile    :: Maybe FilePath	-- output file
    }

defaultOptions :: Options
defaultOptions = Options {
        optParams   = [],
        optAFenFile = Nothing,
        optOFile    = Nothing
    }

addParam :: String -> Options -> Options
addParam pa opt = opt { optParams = pa : optParams opt }

addAFile :: FilePath -> Options -> Options
addAFile fi opt = opt { optAFenFile = Just fi }

addOFile :: FilePath -> Options -> Options
addOFile fi opt = opt { optOFile = Just fi }

options :: [OptDescr (Options -> Options)]
options = [
        Option "p" ["param"]   (ReqArg addParam "STRING") "Eval/search/time parameters: name=value,...",
        Option "a" ["analyse"] (ReqArg addAFile "STRING") "Analysis file",
        Option "o" ["output"]  (ReqArg addOFile "STRING") "Output file"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: Mtg [-p name=val[,...]] -a AFILE -o OFILE"

initContext :: Options -> IO Context
initContext opts = do
    clktm <- getClockTime
    let llev = LogNever
    lchan <- newChan
    wchan <- newChan
    ichan <- newChan
    ha <- newCache 128	-- should be enough for 1 second per fen
    hi <- newHist
    let paramList = stringToParams $ concat $ intersperse "," $ optParams opts
    (parc, evs) <- makeEvalState Nothing paramList "" ""
    let chg = Chg {
            working = False,
            compThread = Nothing,
            crtStatus = posToState initPos ha hi evs,
            realPly = Nothing,
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
    case (optAFenFile opts, optOFile opts) of
        (Just aFile, Just oFile) -> runReaderT (analysingMachine aFile oFile) ctx
        _                        -> hPutStrLn stderr "Analyse file and output file must be given"

analysingMachine :: FilePath -> FilePath -> CtxIO ()
analysingMachine aFile oFile = do
    ctx <- ask
    let logFileName = progLogName ++ "-" ++ show (startSecond ctx) ++ ".log"
    startLogger logFileName
    startWriter
    startInformer
    beforeReadLoop
    fileReader aFile oFile
    -- whatever to do when ending:
    beforeProgExit

startLogger :: String -> CtxIO ()
startLogger _ = do
    ctx <- ask
    void $ liftIO $ forkIO $ catch (theLogger (logger ctx)) collectError
    ctxLog LogInfo "Logger started"

skipChan :: Chan a -> IO ()
skipChan ch = do
    _ <- readChan ch
    return ()

-- Just read the log entries and discard them
theLogger :: Chan String -> IO ()
theLogger lchan = forever $ skipChan lchan

-- The writer just writes to standard output
-- But it is necessary to write from a single thread, as this is a shared resource
startWriter :: CtxIO ()
startWriter = do
    ctx <- ask
    void $ liftIO $ forkIO
         $ theWriter (writer ctx)

-- Just discard the messages
theWriter :: Chan String -> IO ()
theWriter wchan = forever $ skipChan wchan

-- The informer is getting structured data
-- and formats it to a string which is set to the writer
-- It ignores messages which come while we are not searching
startInformer :: CtxIO ()
startInformer = do
    ctx <- ask
    void $ newThread (theInformer (inform ctx))
    return ()

-- Just discard the messages
theInformer :: Chan InfoToGui -> CtxIO ()
theInformer ichan = liftIO $ forever $ skipChan ichan

doPosition :: Pos -> [Move] -> CtxIO Int
doPosition fen mvs = do
    chg <- readChanging
    hi <- liftIO newHist
    let es = evalst $ crtStatus chg
    (mi, ns) <- newState fen mvs (hash . crtStatus $ chg) hi es
    let ph = gamePhase $ thePos ns
    modifyChanging (\c -> c { crtStatus = ns, realPly = mi, myColor = myCol })
    return ph
    where newState fpos ms c h es = foldM execMove (stateFromFen fpos c h es) ms
          execMove (mi, s) m = do
              let mj = case mi of
                           Nothing -> Nothing
                           Just i  -> Just (i+1)
              s' <- execCState (doRealMove m) s
              return (mj, s')
          fenColor = movingColor fen
          myCol = if even (length mvs) then fenColor else other fenColor

stateFromFen :: Pos -> Cache -> History -> EvalState -> (Maybe Int, MyState)
stateFromFen StartPos  c h es = (Just 1,  posToState initPos c h es)
stateFromFen (Pos fen) c h es = (Nothing, posToState (posFromFen fen) c h es)

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

-- The file reader reads an annotated analysis file
-- and analyses every fen, writing an output record
fileReader :: FilePath -> FilePath -> CtxIO ()
fileReader aFile oFile = do
    inp <- liftIO $ readFile aFile
    ho  <- liftIO $ openFile oFile WriteMode
    liftIO $ hPutStrLn ho "score;phase;ply;mtg"
    mapM_ (perFenLine ho) $ lines inp
    liftIO $ hClose ho

maxdepth :: Int
maxdepth = 20

perFenLine :: Handle -> String -> CtxIO ()
perFenLine ho fenLine = do
    let (fen, rst) = break ((==) '\t') fenLine
        ("#":"ply":scply:"maxply":smaxply:_) = words rst
        cply = read scply :: Int
        maxply = read smaxply :: Int
    liftIO $ putStrLn $ "Fen: " ++ fen
    ph <- doPosition (Pos fen) []
    ctx <- ask
    currms <- lift $ currMilli (startSecond ctx)
    modifyChanging $ \c -> c { working = True, srchStrtMs = currms,
                               prvMvInfo = Nothing,
                               crtStatus = posNewSearch (crtStatus c) }
    sc <- searchTheTree 1 maxdepth (currms + 1000) Nothing [] []	-- one second per fen -> make it an option!
    liftIO $ hPutStrLn ho $ show sc ++ ";" ++ show ph ++ ";" ++ show cply
                                  ++ ";" ++ show ((maxply - cply) `div` 2)

newThread :: CtxIO () -> CtxIO ThreadId
newThread a = do
    ctx <- ask
    liftIO $ forkIO $ runReaderT a ctx

data SearchException = SearchException String deriving (Show, Typeable)

instance Exception SearchException

-- Search with the given depth
searchTheTree :: Int -> Int -> Int -> Maybe Int -> [Move] -> [Move] -> CtxIO Int
searchTheTree depth mdepth timx lsc lpv rmvs = do
    ctx <- ask
    chg <- readChanging
    (path, sc, rmvsf, timint, stfin) <- bestMoveCont depth timx (crtStatus chg) lsc lpv rmvs
    case length path of _ -> return () -- because of lazyness!
    modifyChanging (\c -> c { crtStatus = stfin })
    currms <- lift $ currMilli (startSecond ctx)
    if timint || currms > timx || depth >= mdepth
        then return sc
        else searchTheTree (depth + 1) mdepth timx (Just sc) path rmvsf

beforeReadLoop :: CtxIO ()
beforeReadLoop = do
    ctxLog LogInfo "Time parameters:"
    tp <- asks tipars
    ctxLog LogInfo $ show tp
    chg <- readChanging
    let evst = evalst $ crtStatus chg
    ctxLog LogInfo "Eval parameters and weights:"
    ctxLog LogInfo $ show (esEParams evst)
    -- forM_ (zip3 weightNames (esDWeightsM evst) (esDWeightsE evst))
    --    $ \(n, vm, ve) -> ctxLog LogInfo $! n ++ "\t" ++ show vm ++ "\t" ++ show ve
    bm <- liftIO $ hGetBuffering stdin
    ctxLog DebugUci $ "Stdin: " ++ show bm

beforeProgExit :: CtxIO ()
beforeProgExit = return ()

-- Name of the log file
progLogName :: String
progLogName = "mtg"

-- Append error info to error file:
collectError :: SomeException -> IO ()
collectError e = handle cannot $ do
    let efname = "Barbarossa_collected_errors.txt"
    TOD tm _ <- getClockTime
    ef <- openFile efname AppendMode
    hPutStrLn ef $ show tm ++ " mtg: " ++ show e
    hClose ef
    where cannot :: IOException -> IO ()
          cannot _ = return ()
