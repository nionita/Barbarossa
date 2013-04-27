{-# LANGUAGE PatternGuards #-}
module Main where
import Control.Monad
import Control.Monad.Reader
import Data.Array.Unboxed
import Data.Maybe
import Control.Concurrent
import qualified Control.Exception as CE
import System.Environment (getArgs)
import System.IO
import System.Time

import Struct.Struct
import Struct.Status
import Struct.Context
import Config.ConfigClass
import Hash.TransTab
import Uci.UCI
import Uci.UciGlue
import Moves.Base
import Moves.Moves (movesInit)
import Moves.Board (posFromFen)
import Moves.History
import Search.SearchMonad (execSearch)
import Eval.Eval (paramNames)
import Eval.FileParams (makeEvalState)

-- Name, authos, version and suffix:
progName, progVersion, progVerSuff, progAuthor :: String
progName    = "Barbarossa"
progAuthor  = "Nicu Ionita"
progVersion = "0.01"
progVerSuff = ""

data Options = Options {
        optConfFile :: Maybe String,	-- config file
        optParams   :: [String],	-- list of eval parameter assignements
        optLogging  :: LogLevel		-- logging level
    }

defaultOptions :: Options
defaultOptions = Options {
        optConfFile = Nothing,
        optParams   = [],
        optLogging  = LogError
    }

setConfFile :: String -> Options -> Options
setConfFile cf opt = opt { optConfFile = Just sc }

addParam :: String -> Options -> Options
addParam pa opt = opt { optParams = pa : optParams opt }

setLogging :: Int -> Options -> Options
setLogging lev opt = opt { optLogging = llev }
    where llev = case lev of
                   0 -> DebugSearch
                   1 -> DebugUci
                   2 -> LogInfo
                   3 -> LogWarning
                   4 -> LogError
                   _ -> if llev < 0 then DebugSearch else LogNever

options :: [OptDescr (Options -> Options)]
options = [
        Option ['c'] ["config"]  (ReqArg setConfFile "STRING") "Configuration file",
        Option ['l'] ["loglev"] (ReqArg setLogging "INT")     "Logging level: 0=debug, 5=never"
        Option ['p'] ["param"]   (ReqArg addParam "STRING")    "Eval parameters: name=value,..."
    ]

theOptions :: IO (Options, [String])
theOptions argv = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName ++ " [-c CONF] [-l LEV] [-p name=val[,...]]"

initContext :: Options -> IO Context
initContext opts = do
    clktm <- getClockTime
    let llev = getIParamDefault cfg "logLevel" 0
    mlchan <- newChan
    wchan  <- newChan
    ichan <- newChan
    ha <- newCache cfg
    hi <- newHist
    (parc, evs) <- makeEvalState (optConfFile opts) progVersion progVerSuff
    let chg = Chg {
            config = cf,
            working = False,
            compThread = Nothing,
            crtStatus = posToState initPos ha hi evs,
            forGui = Nothing,
            srchStrtMs = 0,
            myColor = White
         }
    ctxVar <- newMVar chg
    let context = Ctx {
            logger = mlchan,
            writer = wchan,
            inform = ichan,
            strttm = clktm,
            change = ctxVar,
            loglev = llev,
            evpid  = parc
         }
    return context

main :: IO ()
main = do
    opts <- theOptions
    ctx <- initContext opts
    runReaderT startTheMachine ctx

startTheMachine :: CtxIO ()
startTheMachine = do
    ctx <- ask
    let TOD crts _  = strttm ctx
        logFileName = progLogName ++ show crtt ++ ".log"
    startLogger logFileName
    startWriter
    startInformer
    beforeReadLoop
    ctxCatch theReader
        $ \e -> ctxLog LogError $ "Reader error: " ++ show e
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
    _ <- liftIO $ forkIO $ CE.catch (theLogger (logger ctx) (LoggerFile file)) collectError
    ctxLog LogInfo "Logger started"

theLogger :: Chan String -> LoggerState -> IO ()
theLogger lchan lst = do
    s <- readChan lchan
    case lst of
        LoggerError -> theLogger lchan lst
        LoggerFile  -> flip CE.catch collectError $ do
            h <- openFile file AppendMode
            hPutStrLn h s
            hFlush h
            theLogger lchan (LoggerHandle h)
        LoggerHandle h -> do
            hPutStrLn h s
            hFlush h
            theLogger lchan lst

-- The writer just writes to standard output
-- But it is necessary to write from a single thread, as this is a shared resource
startWriter :: CtxIO ()
startWriter = do
    ctx <- ask
    _ <- liftIO $ forkIO $ theWriter (writer ctx) (logger ctx)
    return ()

theWriter :: Chan String -> Chan String -> Bool -> IO ()
theWriter wchan lchan log = forever $ do
    s <- readChan wchan
    hPutStrLn stdout s
    hFlush stdout
    when log $ logging lchan $ "Output: " ++ s

-- The informer is getting structured data
-- and formats it to a string which is set to the writer
-- It ignores messages which come while we are not searching
startInformer :: CtxIO ()
startInformer = do
    ctx <- ask
    _ <- newThread (theInformer (inform ctx))
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
    let euci = parseUciStr line
    stop <- case euci of
        Left _    -> do
            ctxLog DebugUci $ "Input: " ++ line
            ctxLog DebugUci $ "Parse: " ++ show euci
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
        _          -> goOn ignore

doQuit :: CtxIO ()
doQuit = ctxLog LogInfo "Normal exit"

goOn :: CtxIO () -> CtxIO Bool
goOn action = action >> return False

doUci :: CtxIO ()
doUci = do
    evid <- asks evpid
    answer (idName ++ " " ++ evid) >> answer idAuthor >> answer uciOk

doIsReady :: CtxIO ()
doIsReady = when (movesInit == 0) $ answer readyOk

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
        then ctxLog DebugUci "GUI sent Position while I'm working..."
        else do
            hi <- liftIO newHist
            let es = evalst $ crtStatus chg
            ns <- newState fen mvs (hash . crtStatus $ chg) hi es
            -- ns <- newState fen mvs
            modifyChanging (\c -> c { crtStatus = ns, myColor = myCol })
    where newState fpos ms c h es = foldM execMove (stateFromFen fpos c h es) ms
          -- execMove p m = execStateT (doMove True m False) p
          execMove p m = execSearch (doMove True m False) p
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
                md <- getIParamDef "maxDepth" 20
                let (tim, tpm, mtg) = getTimeParams cmds lastsc $ myColor chg
                    dpt = fromMaybe md (findDepth cmds)
                    lastsc = case forGui chg of
                                 Just InfoB { infoScore = sc } -> sc
                                 _ -> 0
                startWorking tim tpm mtg dpt

getTimeParams :: [GoCmds] -> Int -> Color -> (Int, Int, Int)
getTimeParams cs _ c	-- unused: lastsc
    = if tpm == 0 && tim == 0
         then (0, 0, 0)
         else (tim, tpm, mtg)
    where tpm = fromMaybe 0 $ findTInc c cs
          tim = fromMaybe 0 $ findTime c cs
          mtg = fromMaybe 0 $ findMovesToGo cs

timeReserved :: Int
timeReserved   = 20	-- milliseconds reserved for move communication

remTimeFracIni, remTimeFracFin, remTimeFracDev :: Double
remTimeFracIni = 0.01	-- fraction of remaining time which we can consume at once - initial value
remTimeFracFin = 0.5	-- same at final (when remaining time is near zero)
remTimeFracDev = remTimeFracFin - remTimeFracIni

compTime :: Int -> Int -> Int -> Int -> (Int, Int)
compTime tim tpm fixmtg lastsc
    = if tpm == 0 && tim == 0 then (0, 0) else (ctm, tmx)
    where ctn = tpm + tim `div` mtg
          ctm = if tim > 0 && tim < 8000 || tim == 0 && tpm < 1500 then 200 else ctn
          mtg = if fixmtg > 0 then fixmtg else estimateMovesToGo lastsc
          frtim = fromIntegral $ max 0 $ tim - ctm	-- rest time after this move
          fctm  = fromIntegral ctm :: Double
          rtimprc = fctm / max frtim fctm
          rtimfrc = remTimeFracIni + remTimeFracDev * rtimprc
          tmxt = round $ fctm + rtimfrc * frtim
          tmx  = min (tim - timeReserved) tmxt

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
    currms <- lift currMilli
    ctxLog DebugUci $ "Start at " ++ show currms
        ++ " to search: " ++ show tim ++ " / " ++ show tpm ++ " / " ++ show mtg
        ++ " - maximal " ++ show dpt ++ " plys"
    modifyChanging $ \c -> c { working = True, srchStrtMs = currms,
                               crtStatus = posNewSearch (crtStatus c) }
    tid <- newThread (startSearchThread tim tpm mtg dpt)
    modifyChanging (\c -> c { compThread = Just tid })
    return ()

-- We use modifyChanging in at least 2 threads: in the reader and
-- in the search thread (here in giveBestMove)
-- This is not good, then it can lead to race conditions. We should
-- find another scheme, for example with STM
startSearchThread :: Int -> Int -> Int -> Int -> CtxIO ()
startSearchThread tim tpm mtg dpt = do
    fd <- getIParamDef "firstDepth" 1
    ctxCatch (searchTheTree fd dpt 0 tim tpm mtg Nothing [] [])
        $ \e -> do
            chg <- readChanging
            let mes = "searchTheTree terminated by exception: " ++ show e
            answer $ infos mes
            case forGui chg of
                Just ifg -> giveBestMove $ infoPv ifg
                Nothing  -> return ()
            ctx <- ask
            case logger ctx of
                Just _  -> ctxLog LogError mes
                Nothing -> return ()
            lift $ collectError mes
            -- Why? liftIO $ threadDelay $ 50*1000 -- give time to send the ans

ctxCatch :: CtxIO a -> (CE.SomeException -> CtxIO a) -> CtxIO a
ctxCatch a f = do
    ctx <- ask
    liftIO $ CE.catch (runReaderT a ctx)
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
searchTheTree :: Int -> Int -> Int -> Int -> Int -> Int -> Maybe Int -> [Move] -> [Move] -> CtxIO ()
searchTheTree tief mtief timx tim tpm mtg lsc lpv rmvs = do
    chg <- readChanging
    ctxLog LogInfo $ "Time = " ++ show tim ++ " Timx = " ++ show timx
    (path, sc, rmvsf, stfin) <- bestMoveCont tief timx (crtStatus chg) lsc lpv rmvs
    case length path of _ -> return () -- because of lazyness!
    storeBestMove path sc	-- write back in status
    modifyChanging (\c -> c { crtStatus = stfin })
    currms <- lift currMilli
    let (ms', mx) = compTime tim tpm mtg sc
        ms  = if sc > betterSc
                 then ms' * 4 `div` 5
                 else if sc < -betterSc
                      then ms' * 6 `div` 5
                      else ms'
        strtms = srchStrtMs chg
        delta = strtms + ms - currms
        ms2 = ms `div` 2
        onlyone = ms > 0 && length rmvsf == 1 && tief >= 4	-- only in normal play
        timeover = ms > 0 && delta <= ms2  -- time is half over
        depthmax = tief >= mtief	--  or maximal depth
        mes = "Depth " ++ show tief ++ " Score " ++ show sc ++ " in ms "
                ++ show currms ++ " remaining " ++ show delta
                ++ " path " ++ show path
    -- answer $ infos $ "currms = " ++ show currms
    -- answer $ infos $ "ms     = " ++ show ms
    -- answer $ infos $ "mx     = " ++ show mx
    -- answer $ infos $ "cr+mx  = " ++ show (currms + mx)
    ctxLog LogInfo mes
    ctxLog LogInfo $ "compTime: " ++ show ms' ++ " / " ++ show mx
    -- if ms > 0 && (delta <= 0 || tief >= mtief)  -- time is over or maximal depth
    if depthmax || timeover || onlyone
        then do
            -- answer $ infos $ "End of search"
            -- answer $ infos $ "depthmax = " ++ show depthmax
            -- answer $ infos $ "timeover = " ++ show timeover
            -- answer $ infos $ "onlyone = " ++ show onlyone
            when depthmax $ ctxLog LogInfo "in searchTheTree: max depth reached"
            giveBestMove path
        else do
            chg' <- readChanging
            if working chg'
                then searchTheTree (tief + 1) mtief (currms + mx) tim tpm mtg (Just sc) path rmvsf
                else do
                    ctxLog DebugUci "in searchTheTree: not working"
                    giveBestMove path -- was stopped

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
    chg <- readChanging
    let evst = evalst $ crtStatus chg
    ctxLog LogInfo "Initial eval parameters:"
    forM_ (zip paramNames (esDParams evst)) $ \(n, v) -> ctxLog LogInfo $! n ++ "\t" ++ show v

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
progLogName = "abulafia" ++ "-" ++ progVersion
                 ++ if null progVerSuff then ""
                                        else "-" ++ progVerSuff

-- These are the possible answers from engine to GUI:
idName, idAuthor, uciOk, readyOk :: String
idName = "id name " ++ progName ++ " " ++ progVersion
             ++ if null progVerSuff then "" else " " ++ progVerSuff
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

-- Append error info to error file:
collectError :: CE.SomeException -> IO ()
collectError e = CE.catch (do
    let efname = "Abulafia_collected_errors.txt"
    tm <- currentSecs
    ef <- openFile efname AppendMode
    hPutStrLn ef $ show tm ++ " " ++ idName ++ ": " ++ show e
    hClose ef
    ) $ \_ -> return ()
