{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Exception
import Data.Array.Unboxed
import Data.Foldable (foldrM)
import Data.List (intersperse, delete)
import Data.Maybe
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
import Moves.Moves (movesInit)
import Moves.Board (posFromFen)
import Moves.History
import Search.CStateMonad (execCState)
import Eval.Eval (weightNames)
import Eval.FileParams (makeEvalState)

-- Name, authos, version and suffix:
progName, progVersion, progVerSuff, progAuthor :: String
progName    = "Barbarossa"
progAuthor  = "Nicu Ionita"
progVersion = "0.2.0"
progVerSuff = "pl4d51k"

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
    (parc, evs) <- makeEvalState (optConfFile opts) paramList progVersion progVerSuff
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
        Nothing -> runReaderT interMachine ctx	-- the normal (interactive) mode
        Just fi -> runReaderT (analysingMachine fi) ctx	-- the analysis mode

-- The logger, writer and informer will be started only once, here,
-- so every setting about them cannot be changed later, which mainly
-- excludes logging level and log file name
interMachine :: CtxIO ()
interMachine = do
    ctx <- ask
    let logFileName = progLogName ++ "-" ++ show (startSecond ctx) ++ ".log"
    startLogger logFileName
    startWriter True
    startInformer
    beforeReadLoop
    ctxCatch theReader
        $ \e -> ctxLog LogError $ "Reader error: " ++ show e
    -- whatever to do when ending:
    beforeProgExit

analysingMachine :: FilePath -> CtxIO ()
analysingMachine fi = do
    ctx <- ask
    let logFileName = progLogName ++ "-" ++ show (startSecond ctx) ++ ".log"
    startLogger logFileName
    startWriter False
    startInformer
    beforeReadLoop
    fileReader fi
    -- whatever to do when ending:
    beforeProgExit

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

-- The writer just writes to standard output
-- But it is necessary to write from a single thread, as this is a shared resource
startWriter :: Bool -> CtxIO ()
startWriter inter = do
    ctx <- ask
    void $ liftIO $ forkIO
         $ theWriter inter (writer ctx) (logger ctx) (LogInfo >= loglev ctx) (startSecond ctx)

theWriter :: Bool -> Chan String -> Chan String -> Bool -> Integer -> IO ()
theWriter inter wchan lchan mustlog refs = forever $ do
    s <- readChan wchan
    when inter $ do	-- we write only in intercative mode
        putStrLn s
        hFlush stdout
    when mustlog $ logging lchan refs "Output" s

-- The informer is getting structured data
-- and formats it to a string which is set to the writer
-- It ignores messages which come while we are not searching
startInformer :: CtxIO ()
startInformer = do
    ctx <- ask
    void $ newThread (theInformer (inform ctx))
    return ()

theInformer :: Chan InfoToGui -> CtxIO ()
theInformer ichan = forever $ do
    s <- liftIO $ readChan ichan
    chg <- readChanging
    when (working chg) $ toGui s

toGui :: InfoToGui -> CtxIO ()
toGui s = case s of
            InfoS s'   -> answer $ infos s'
            InfoD _    -> answer $ formInfoDepth s
            InfoCM _ _ -> answer $ formInfoCM s
            _          -> answer $ formInfo s

-- The reader is executed by the main thread
-- It reads commands from the GUI and interprets them
theReader :: CtxIO ()
theReader = do
    line <- liftIO getLine
    ctxLog DebugUci $ "Input: " ++ line
    let euci = parseUciStr line
    stop <- case euci of
        Left _    -> do
            ctxLog LogWarning $ "Input: " ++ line
            ctxLog LogWarning $ "Parse: " ++ show euci
            return False
        Right uci -> interpret uci
    unless stop theReader

interpret :: UCIMess -> CtxIO Bool
interpret uci =
    case uci of
        Quit       -> do doQuit
                         let ms = 500   -- sleep 0.5 second
                         liftIO $ threadDelay $ ms * 1000
                         return True
        Uci        -> goOn doUci
        IsReady    -> goOn doIsReady
        UciNewGame -> goOn doUciNewGame
        Position p mvs -> goOn (doPosition p mvs)
        Go cmds    -> goOn (doGo cmds)
        Stop       -> goOn $ doStop True
        Ponderhit  -> goOn doPonderhit
        SetOption o -> goOn (doSetOption o)
        _          -> goOn ignore

doQuit :: CtxIO ()
doQuit = ctxLog LogInfo "Normal exit"

goOn :: CtxIO () -> CtxIO Bool
goOn action = action >> return False

doUci :: CtxIO ()
doUci = do
    evid <- asks evpid
    answer $ idName ++ " " ++ evid
    answer idAuthor
    mapM_ sendOption guiUciOptions 
    answer uciOk

doIsReady :: CtxIO ()
doIsReady = do
    when (movesInit == 0) $ return ()
    answer readyOk

doSetOption :: Option -> CtxIO ()
doSetOption opt = do
    let NameValue on ov = unifyOption opt
    chg <- readChanging
    if working chg
       then ctxLog LogWarning "GUI sent SetOption while I'm working..."
       else case on of
                "Hash" -> setOptionHash ov
                _      -> ctxLog LogWarning
                              $ "Unknown option from engine: " ++ on ++ " with value " ++ ov

unifyOption :: Option -> Option
unifyOption (Name on) = NameValue on "true"
unifyOption o         = o

setOptionHash :: String -> CtxIO ()
setOptionHash sval =
    case reads sval of
        [(val, "")] -> do
            chg <- readChanging
            let st = crtStatus chg
            ha <- liftIO $ newCache val
            modifyChanging $ \c -> c { crtStatus = st { hash = ha }}
            ctxLog LogInfo $ "Cache was set on " ++ sval ++ " MB"
        _           -> ctxLog LogError $ "GUI: wrong number of MB for option Hash: " ++ sval
    

ignore :: CtxIO ()
ignore = notImplemented "ignored"

notImplemented :: String -> CtxIO ()
notImplemented s = ctxLog LogWarning $ "not implemented: " ++ s

doUciNewGame :: CtxIO ()
doUciNewGame = notImplemented "doUciNewGame"

doPosition :: Pos -> [Move] -> CtxIO ()
doPosition fen mvs = do
    -- ctxLog DebugUci $ "Position: " ++ show fen ++ " moves " ++ show mvs
    chg <- readChanging
    if working chg
        then ctxLog LogWarning "GUI sent Position while I'm working..."
        else do
            hi <- liftIO newHist
            let es = evalst $ crtStatus chg
            ns <- newState fen mvs (hash . crtStatus $ chg) hi es
            modifyChanging (\c -> c { crtStatus = ns, myColor = myCol })
    where newState fpos ms c h es = foldM execMove (stateFromFen fpos c h es) ms
          execMove p m = execCState (doMove True m False) p
          fenColor = movingColor fen
          myCol = if even (length mvs) then fenColor else other fenColor

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

doGo :: [GoCmds] -> CtxIO ()
doGo cmds = do
    ctxLog DebugUci $ "Go: " ++ show cmds
    chg <- readChanging
    if working chg
        then ctxLog DebugUci "GUI sent Go while I'm working..."
        else if Ponder `elem` cmds
            then ctxLog DebugUci "Just ponder: ignored"
            else do
                let (tim, tpm, mtg) = getTimeParams cmds lastsc $ myColor chg
                    md = 20	-- max search depth
                    dpt = fromMaybe md (findDepth cmds)
                    lastsc = case forGui chg of
                                 Just InfoB { infoScore = sc } -> sc
                                 _ -> 0
                startWorking tim tpm mtg dpt

data Agreg = Agreg {
         agrCumErr :: !Integer,	-- accumulated error
         agrFenOk  :: !Int,	-- number of fens analysed
         agrFenNOk :: !Int	-- number of fens aborted
     } deriving Show

-- The file reader reads an annotated analysis file
-- and analyses every fen, cummulating the error
-- and reporting it
fileReader :: FilePath -> CtxIO ()
fileReader fi = do
    inp <- liftIO $ readFile fi
    let header : fens = lines inp
        "Reference" : "depth" : sdepth : _ = words header
        dpt = read sdepth
    ctxLog LogInfo $ "Start analysing from: " ++ header
    agr <- foldrM (perFenLine dpt) (Agreg 0 0 0) fens
    lift $ putStrLn $ show agr

perFenLine :: Int -> String -> Agreg -> CtxIO Agreg
perFenLine dpt fenLine agr = do
    let (refsc, fen') = break ((==) '\t') fenLine
        rsc = read refsc
        fen = tail fen'	-- it has the \t in front
    ctxLog LogInfo $ "Ref.Score " ++ refsc ++ " fen " ++ fen
    doPosition (Pos fen) []
    modifyChanging $ \c -> c { working = True }
    sc <- searchTheTree 1 dpt 0 0 0 0 Nothing [] []
    return $ aggregateError agr rsc sc

aggregateError :: Agreg -> Int -> Int -> Agreg
aggregateError agr refsc sc
    = agr { agrCumErr = agrCumErr agr + fromIntegral (dif * dif), agrFenOk = agrFenOk agr + 1 }
    where dif = sc - refsc

getTimeParams :: [GoCmds] -> Int -> Color -> (Int, Int, Int)
getTimeParams cs _ c	-- unused: lastsc
    = if tpm == 0 && tim == 0
         then (0, 0, 0)
         else (tim, tpm, mtg)
    where tpm = fromMaybe 0 $ findTInc c cs
          tim = fromMaybe 0 $ findTime c cs
          mtg = fromMaybe 0 $ findMovesToGo cs


-- These parameters should be optimised (i.e.: first made options)
remTimeFracIni, remTimeFracFin, remTimeFracDev :: Double
remTimeFracIni = 0.15	-- fraction of remaining time which we can consume at once - initial value
remTimeFracFin = 0.5	-- same at final (when remaining time is near zero)
remTimeFracDev = remTimeFracFin - remTimeFracIni

timeReserved :: Int
timeReserved   = 70	-- milliseconds reserved for move communication

-- This function calculates the normal time for the next search loop,
-- the maximum of that (whch cannot be exceeded)
-- and if we are in time troubles or not
compTime :: Int -> Int -> Int -> Int -> (Int, Int, Bool)
compTime tim tpm fixmtg lastsc
    | tim == 0 && tpm == 0 = (  0,   0,  False)
    | otherwise            = (ctm, tmx, ttroub)
    where mtg = if fixmtg > 0 then fixmtg else estimateMovesToGo lastsc
          ctn = tpm + tim `div` mtg
          (ctm, short) = if tim > 0 && tim < 2000 || tim == 0 && tpm < 700
                            then (300, True)
                            else (ctn, False)
          frtim = fromIntegral $ max 0 $ tim - ctm	-- rest time after this move
          fctm  = fromIntegral ctm :: Double
          rtimprc = fctm / max frtim fctm
          rtimfrc = remTimeFracIni + remTimeFracDev * rtimprc
          tmxt = round $ fctm + rtimfrc * frtim
          maxx = max 0 $ tim - timeReserved
          (tmx, over) = if maxx < tmxt then (maxx, True) else (tmxt, False)
          ttroub = short || over

estMvsToGo :: Array Int Int
estMvsToGo = listArray (0, 8) [30, 28, 24, 18, 12, 10, 8, 6, 3]

estimateMovesToGo :: Int -> Int
estimateMovesToGo sc = estMvsToGo ! mvidx
    where mvidx = min 8 $ abs sc `div` 100

-- Some parameters (until we have a good solution)
clearHash :: Bool
clearHash = False

newThread :: CtxIO () -> CtxIO ThreadId
newThread a = do
    ctx <- ask
    liftIO $ forkIO $ runReaderT a ctx

startWorking :: Int -> Int -> Int -> Int -> CtxIO ()
startWorking tim tpm mtg dpt = do
    ctx <- ask
    currms <- lift $ currMilli (startSecond ctx)
    ctxLog DebugUci $ "Start at " ++ show currms
        ++ " to search: " ++ show tim ++ " / " ++ show tpm ++ " / " ++ show mtg
        ++ " - maximal " ++ show dpt ++ " plys"
    modifyChanging $ \c -> c { working = True, srchStrtMs = currms,
                               prvMvInfo = Nothing,
                               crtStatus = posNewSearch (crtStatus c) }
    tid <- newThread (startSearchThread tim tpm mtg dpt)
    modifyChanging (\c -> c { compThread = Just tid })
    return ()

-- We use modifyChanging in at least 2 threads: in the reader and
-- in the search thread (here in giveBestMove)
-- This is not good, then it can lead to race conditions. We should
-- find another scheme, for example with STM
startSearchThread :: Int -> Int -> Int -> Int -> CtxIO ()
startSearchThread tim tpm mtg dpt =
    ctxCatch (void $ searchTheTree 1 dpt 0 tim tpm mtg Nothing [] [])
        $ \e -> do
            chg <- readChanging
            let mes = "searchTheTree terminated by exception: " ++ show e
            answer $ infos mes
            case forGui chg of
                Just ifg -> giveBestMove $ infoPv ifg
                Nothing  -> return ()
            ctxLog LogError mes
            lift $ collectError $ SomeException (SearchException mes)

data SearchException = SearchException String deriving (Show, Typeable)

instance Exception SearchException

ctxCatch :: CtxIO a -> (SomeException -> CtxIO a) -> CtxIO a
ctxCatch a f = do
    ctx <- ask
    liftIO $ catch (runReaderT a ctx)
            (\e -> runReaderT (f e) ctx)

internalStop :: Int -> CtxIO ()
internalStop ms = do
    let sleep = ms * 1000
    ctxLog DebugUci $ "Internal stop clock started for " ++ show ms ++ " ms"
    liftIO $ threadDelay sleep
    ctxLog DebugUci "Internal stop clock ended"
    doStop False

betterSc :: Int
betterSc = 25

-- Search with the given depth
searchTheTree :: Int -> Int -> Int -> Int -> Int -> Int -> Maybe Int -> [Move] -> [Move] -> CtxIO Int
searchTheTree tief mtief timx tim tpm mtg lsc lpv rmvs = do
    ctx <- ask
    chg <- readChanging
    ctxLog LogInfo $ "Time = " ++ show tim ++ " Timx = " ++ show timx
    (path, sc, rmvsf, stfin) <- bestMoveCont tief timx (crtStatus chg) lsc lpv rmvs
    case length path of _ -> return () -- because of lazyness!
    storeBestMove path sc	-- write back in status
    modifyChanging (\c -> c { crtStatus = stfin })
    currms <- lift $ currMilli (startSecond ctx)
    let (ms', mx, urg) = compTime tim tpm mtg sc
    ms <- if urg then return ms' else correctTime tief ms' sc path
    let strtms = srchStrtMs chg
        delta = strtms + ms - currms
        ms2 = ms `div` 2
        onlyone = ms > 0 && length rmvsf == 1 && tief >= 4	-- only in normal play
        halfover = ms > 0 && delta <= ms2  -- time is half over
        depthmax = tief >= mtief	--  or maximal depth
        mes = "Depth " ++ show tief ++ " Score " ++ show sc ++ " in ms "
                ++ show currms ++ " remaining " ++ show delta
                ++ " path " ++ show path
    ctxLog LogInfo mes
    ctxLog LogInfo $ "compTime: " ++ show ms' ++ " / " ++ show mx
    if depthmax || halfover || onlyone
        then do
            when depthmax $ ctxLog LogInfo "in searchTheTree: max depth reached"
            giveBestMove path
            return sc
        else do
            chg' <- readChanging
            if working chg'
                then if mx == 0	-- no time constraint
                        then searchTheTree (tief + 1) mtief 0             tim tpm mtg (Just sc) path rmvsf
                        else searchTheTree (tief + 1) mtief (strtms + mx) tim tpm mtg (Just sc) path rmvsf
                else do
                    ctxLog DebugUci "in searchTheTree: not working"
                    giveBestMove path -- was stopped
                    return sc

-- We assume here that we always have at least the first move of the PV (our best)
-- If not (which is a fatal error) we will get an exception (head of empty list)
-- which will be catched somewhere else and reported
correctTime :: Int -> Int -> Int -> [Move] -> CtxIO Int
correctTime draft ms sc path = do
    tp <- asks tipars
    chg <- readChanging
    (ti, func) <- case prvMvInfo chg of
        Nothing  -> do
            -- We are obviously in the first draft - just insert the first infos, use normal time
            let pmi = PrevMvInfo { pmiBestSc = sc, pmiChanged = 0, pmiBMSoFar = [bm] }
                bm = head path
                func = \c -> c { prvMvInfo = Just pmi }
            return (ms, func)
        Just (PrevMvInfo { pmiBestSc = osc, pmiChanged = ok, pmiBMSoFar = obsf }) -> do
            -- We have previous moves, update and calculate time factor
            let pmi = PrevMvInfo { pmiBestSc = sc, pmiChanged = k, pmiBMSoFar = bsf }
                bm = head path
                (k, bsf, cha) = if bm == head obsf	-- same best move?
                                   then (ok, obsf, False)
                                   else (ok + 1, bm : delete bm obsf, True)
                func = \c -> c { prvMvInfo = Just pmi }
                ms'  = timeFactor tp cha draft ms osc sc k bsf
            ctxLog LogInfo $ "timeFactor: " ++ show cha
                                       ++ " / " ++ show draft
                                       ++ " / " ++ show ms
                                       ++ " / " ++ show osc
                                       ++ " / " ++ show sc
                                       ++ " / " ++ show k
                                       ++ " / " ++ show bsf
            return (ms', func)
    modifyChanging func
    return ti

-- Concept is: have an initial factor and 3 auxiliary: one for draft (when last best move
-- changed), one for score change and one for number of changes and different best moves so far
-- Add all together and limit by a max factor
-- Then multiply the given time by that factor
timeFactor :: TimeParams -> Bool -> Int -> Int -> Int -> Int -> Int -> [Move] -> Int
timeFactor tp cha draft tim osc sc chgs mvs = round $ fromIntegral tim * min (tpMaxFact tp) finf
    where finf = tpIniFact tp + drf + scf + chf
          drf  = if cha then tpDrScale tp * fromIntegral (draft * draft)	-- draft factor
                        else 0
          scf  = let scdiff = osc - sc
                 in tpScScale tp * fromIntegral (scdiff * scdiff * draft)	-- score change factor
          chf  = tpChScale tp * fromIntegral (chgs * length mvs * draft)	-- change factor

storeBestMove :: [Move] -> Int -> CtxIO ()
storeBestMove mvs sc = do
    let s = InfoB { infoPv = mvs, infoScore = sc }
    modifyChanging (\c -> c { forGui = Just s })

giveBestMove :: [Move] -> CtxIO ()
giveBestMove mvs = do
    -- ctxLog "Info" $ "The moves: " ++ show mvs
    modifyChanging $ \c -> c {
        working = False, compThread = Nothing, forGui = Nothing }
    if null mvs
        then answer $ infos "empty pv"
        else answer $ bestMove (head mvs) Nothing

beforeReadLoop :: CtxIO ()
beforeReadLoop = do
    ctxLog LogInfo "Time parameters:"
    tp <- asks tipars
    ctxLog LogInfo $ show tp
    chg <- readChanging
    let evst = evalst $ crtStatus chg
    ctxLog LogInfo "Eval parameters and weights:"
    ctxLog LogInfo $ show (esEParams evst)
    forM_ (zip3 weightNames (esDWeightsM evst) (esDWeightsE evst))
       $ \(n, vm, ve) -> ctxLog LogInfo $! n ++ "\t" ++ show vm ++ "\t" ++ show ve
    bm <- liftIO $ hGetBuffering stdin
    ctxLog DebugUci $ "Stdin: " ++ show bm

beforeProgExit :: CtxIO ()
beforeProgExit = return ()

doStop :: Bool -> CtxIO ()
doStop extern = do
    chg <- readChanging
    modifyChanging (\c -> c { working = False, compThread = Nothing })
    case compThread chg of
        Just tid -> do
            -- when extern $ liftIO $ threadDelay 500000  -- warte 0.5 Sec.
            when extern $ liftIO $ threadDelay 100000  -- warte 0.1 Sec.
            liftIO $ killThread tid
            case forGui chg of
                Just ifg -> giveBestMove $ infoPv ifg
                Nothing  -> return ()
        _ -> return ()

doPonderhit :: CtxIO ()
doPonderhit = notImplemented "doPonderhit"

-- Helper: Answers the GUI with a string
answer :: String -> CtxIO ()
answer s = do
    ctx <- ask
    liftIO $ writeChan (writer ctx) s

-- Name of the log file
progLogName :: String
progLogName = "barbarossa" ++ '-' : progVersion
                 ++ if null progVerSuff then ""
                                        else '-' : progVerSuff

-- These are the possible answers from engine to GUI:
idName, idAuthor, uciOk, readyOk :: String
idName = "id name " ++ progName ++ ' ' : progVersion
             ++ if null progVerSuff then "" else ' ' : progVerSuff
idAuthor = "id author " ++ progAuthor
uciOk = "uciok"
readyOk = "readyok"

bestMove :: Move -> Maybe Move -> String
bestMove m mp = s
    where s = "bestmove " ++ toString m ++ sp
          sp = maybe "" (\v -> " ponder " ++ toString v) mp

-- Info answers:
-- sel.depth nicht implementiert
formInfo :: InfoToGui -> String
formInfo itg = "info"
    -- ++ " score cp " ++ show isc
    ++ formScore isc
    ++ " depth " ++ show (infoDepth itg)
    -- ++ " seldepth " ++ show idp
    ++ " time " ++ show (infoTime itg)
    ++ " nodes " ++ show (infoNodes itg)
    ++ nps'
    ++ " pv" ++ concatMap (\m -> ' ' : toString m) (infoPv itg)
    where nps' = case infoTime itg of
                     0 -> ""
                     x -> " nps " ++ show (infoNodes itg `div` x * 1000)
          isc = infoScore itg

formInfoB :: InfoToGui -> String
formInfoB itg = "info"
    -- ++ " score cp " ++ show isc
    ++ formScore isc
    ++ " pv" ++ concatMap (\m -> ' ' : toString m) (infoPv itg)
    where isc = infoScore itg

formScore :: Int -> String
formScore i
    | i >= mateScore - 255    = " score mate " ++ show ((mateScore - i + 1) `div` 2)
    | i <= (-mateScore) + 255 = " score mate " ++ show ((-mateScore - i) `div` 2)
    | otherwise               = " score cp " ++ show i

-- sel.depth nicht implementiert
formInfo2 :: InfoToGui -> String
formInfo2 itg = "info"
    ++ " depth " ++ show (infoDepth itg)
    ++ " time " ++ show (infoTime itg)
    ++ " nodes " ++ show (infoNodes itg)
    ++ nps'
    -- ++ " pv" ++ concatMap (\m -> ' ' : toString m) (infoPv itg)
    where nps' = case infoTime itg of
                     0 -> ""
                     x -> " nps " ++ show (infoNodes itg * 1000 `div` x)

formInfoNps :: InfoToGui -> Maybe String
formInfoNps itg
    = case infoTime itg of
          0 -> Nothing
          x -> Just $ "info nps " ++ show (infoNodes itg `div` x * 1000)

formInfoDepth :: InfoToGui -> String
formInfoDepth itg
    = "info depth " ++ show (infoDepth itg)
      --  ++ " seldepth " ++ show (infoDepth itg)

formInfoCM :: InfoToGui -> String
formInfoCM itg
    = "info currmove " ++ toString (infoMove itg)
        ++ " currmovenumber " ++ show (infoCurMove itg)

depth :: Int -> Int -> String
depth d _ = "info depth " ++ show d

inodes :: Int -> String
inodes n = "info nodes " ++ show n

pv :: Int -> [Move] -> String
pv t mvs = "info time " ++ show t ++ " pv"
    ++ concatMap (\m -> ' ' : toString m) mvs

nps :: Int -> String
nps n = "info nps " ++ show n

infos :: String -> String
infos s = "info string " ++ s

-- These are the supported Uci options
data UciGUIOptionType = UGOTRange String String
                      | UGOTList [String]
                      | UGOTNone

guiUciOptions :: [(String, String, String, UciGUIOptionType)]
guiUciOptions = [
        ("Hash", "spin", "16", UGOTRange "16" "1024")	-- hash size in MB
    ]

sendOption :: (String, String, String, UciGUIOptionType) -> CtxIO ()
sendOption odesc = do
    let str = describeOption odesc
    answer str

describeOption :: (String, String, String, UciGUIOptionType) -> String
describeOption (oname, otype, odef, ovals)
    = "option name " ++ oname ++ " type " ++ otype
        ++ " default " ++ odef ++ makeOptionVals ovals

makeOptionVals :: UciGUIOptionType -> String
makeOptionVals (UGOTRange mi ma) = " min " ++ mi ++ " max " ++ ma
makeOptionVals (UGOTList li) = foldl (\a x -> a ++ " var " ++ x) "" li
makeOptionVals UGOTNone = ""

-- Append error info to error file:
collectError :: SomeException -> IO ()
collectError e = handle cannot $ do
    let efname = "Barbarossa_collected_errors.txt"
    TOD tm _ <- getClockTime
    ef <- openFile efname AppendMode
    hPutStrLn ef $ show tm ++ " " ++ idName ++ ": " ++ show e
    hClose ef
    where cannot :: IOException -> IO ()
          cannot _ = return ()
