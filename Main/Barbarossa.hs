{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Exception
import Data.Array.Unboxed
import Data.Foldable (foldrM)
import Data.List (intersperse)
import Data.Maybe
import Data.Time.Clock (UTCTime)
import Data.Typeable
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO
import System.Random

import Struct.Struct
import Struct.Status
import Struct.Context
import Struct.Config
import Hash.TransTab
import Uci.UCI
import Uci.UciGlue
import Moves.Base
import Moves.Moves (movesInit)
import Moves.Board (posFromFen, initPos)
import Moves.History
import Search.CStateMonad (execCState)
import Search.AlbetaTypes
import Eval.FileParams (makeEvalState)

-- Name, author, version and suffix:
progName, progVersion, progVerSuff, progAuthor :: String
progName    = "Barbarossa"
progAuthor  = "Nicu Ionita"
progVersion = "0.7.0"
progVerSuff = "pvp5c"

data Options = Options {
        optConfFile :: Maybe String,	-- config file
        optParams   :: [String],	-- list of eval parameter assignements
        optLogging  :: LogLevel,	-- logging level
        optAFenFile :: Maybe FilePath	-- annotated fen file for self analysis
    }

logOptDefault :: LogLevel
logOptDefault = if length progVerSuff == 0 then LogNever else LogInfo

defaultOptions :: Options
defaultOptions = Options {
        optConfFile = Nothing,
        optParams   = [],
        optLogging  = logOptDefault,
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
        Option "c" ["config"]  (ReqArg setConfFile "STRING") "Configuration file",
        Option "l" ["loglev"]  (ReqArg setLogging "STRING")  "Logging level from 0 (debug) to 5 (never)",
        Option "p" ["param"]   (ReqArg addParam "STRING")    "Eval/search/time parameters: name=value,...",
        Option "a" ["analyse"] (ReqArg addAFile "STRING")    "Analysis file"
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
    clktm <- getMyTime
    let llev = optLogging opts
    lchan <- newChan
    wchan  <- newChan
    ha <- newCache 1	-- it will take the minimum number of entries
    hi <- newHist
    let paramList
            | null $ optParams opts = []
            | otherwise             = stringToParams $ concat $ intersperse "," $ optParams opts
    (parc, evs) <- makeEvalState (optConfFile opts) paramList progVersion progVerSuff
    let chg = Chg {
            working = False,
            compThread = Nothing,
            crtStatus = posToState initPos ha hi evs,
            realPly = Nothing,
            forGui = Nothing,
            srchStrtMs = 0,
            myColor = White,
            totBmCh = 0, lastChDr = 0, lmvScore = Nothing
         }
    ctxVar <- newMVar chg
    let context = Ctx {
            logger = lchan,
            writer = wchan,
            strttm = clktm,
            change = ctxVar,
            loglev = llev,
            evpid  = parc
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
         $ theWriter inter (writer ctx) (logger ctx) (LogInfo >= loglev ctx) (strttm ctx)

theWriter :: Bool -> Chan String -> Chan String -> Bool -> UTCTime -> IO ()
theWriter inter wchan lchan mustlog refs = forever $ do
    s <- readChan wchan
    when inter $ do	-- we write only in intercative mode
        putStrLn s
        hFlush stdout
    when mustlog $ logging lchan refs "Output" s

-- The reader is executed by the main thread
-- It reads commands from the GUI and interprets them
theReader :: CtxIO ()
theReader = do
    line <- liftIO getLine
    ctxLog DebugUci $ "Input: " ++ line
    let euci = parseUciStr line
    stop <- case euci of
        Left erm  -> do
            ctxLog LogWarning $ "Input: " ++ line
            ctxLog LogWarning $ "Parse: " ++ show erm
            answer $ infos $ "Parse: " ++ show erm
            return False
        Right uci -> interpret uci
    unless stop theReader

interpret :: UCIMess -> CtxIO Bool
interpret uci =
    case uci of
        Quit           -> doQuit
        Uci            -> goOn doUci
        IsReady        -> goOn doIsReady
        UciNewGame     -> goOn doUciNewGame
        Position p mvs -> goOn $ doPosition p mvs
        Go cmds        -> goOn $ doGo cmds
        Stop           -> goOn doStop
        Ponderhit      -> goOn doPonderhit
        SetOption o    -> goOn $ doSetOption o
        _              -> goOn ignore

doQuit :: CtxIO Bool
doQuit = do
    ctxLog LogInfo "Normal exit"
    let ms = 500   -- sleep 0.5 second to let the channels time to process
    liftIO $ threadDelay $ ms * 1000
    return True

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
doUciNewGame = modifyChanging $ \c -> c { totBmCh = 0, lastChDr = 0, lmvScore = Nothing }

doPosition :: Pos -> [Move] -> CtxIO ()
doPosition fen mvs = do
    -- ctxLog DebugUci $ "Position: " ++ show fen ++ " moves " ++ show mvs
    chg <- readChanging
    if working chg
        then ctxLog LogWarning "GUI sent Position while I'm working..."
        else do
            hi <- liftIO newHist
            let es = evalst $ crtStatus chg
            (mi, ns) <- newState fen mvs (hash . crtStatus $ chg) hi es
            -- reset the last move score when we begin new game:
            let lsc = case mi of
                          Just x  -> if x == 1 || x == 2 then Nothing else lmvScore chg
                          Nothing -> lmvScore chg
            modifyChanging $ \c -> c { crtStatus = ns, realPly = mi, myColor = myCol, lmvScore = lsc }
    where newState fpos ms c h es = foldM execMove (stateFromFen fpos c h es) ms
          execMove (mi, s) m = do
              let mj = ((+) 1) <$> mi	-- increment real ply
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

doGo :: [GoCmds] -> CtxIO ()
doGo cmds = do
    ctxLog DebugUci $ "Go: " ++ show cmds
    chg <- readChanging
    if working chg
        then ctxLog DebugUci "GUI sent Go while I'm working..."
        else if Ponder `elem` cmds
            then ctxLog DebugUci "Just ponder: ignored"
            else do
                let (tim, tpm, mtg) = getUCITime cmds $ myColor chg
                    rept = countRepetitions $ crtStatus chg
                    md   = 20	-- max search depth
                    dpt  = fromMaybe md (findDepth cmds)
                startWorking tim tpm mtg dpt rept

data Agreg = Agreg {
         agrCumErr :: !Integer,	-- accumulated error
         agrFenOk  :: !Int	-- number of fens analysed
         -- agrFenNOk :: !Int	-- number of fens aborted
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
    agr <- foldrM (perFenLine dpt) (Agreg 0 0) fens
    lift $ putStrLn $ show agr

perFenLine :: Int -> String -> Agreg -> CtxIO Agreg
perFenLine dpt fenLine agr = do
    let (refsc, fen') = break ((==) '\t') fenLine
        rsc = read refsc
        fen = tail fen'	-- it has the \t in front
    ctxLog LogInfo $ "Ref.Score " ++ refsc ++ " fen " ++ fen
    doPosition (Pos fen) []
    modifyChanging $ \c -> c { working = True }
    sc <- searchTheTree 1 dpt 0 0 0 0 0 0 Nothing [] []
    return $ aggregateError agr rsc sc

aggregateError :: Agreg -> Int -> Int -> Agreg
aggregateError agr refsc sc
    = agr { agrCumErr = agrCumErr agr + fromIntegral (dif * dif), agrFenOk = agrFenOk agr + 1 }
    where dif = sc - refsc

getUCITime :: [GoCmds] -> Color -> (Int, Int, Int)
getUCITime cs c
    | tpm == 0 && tim == 0 = (0, 0, 0)
    | otherwise            = (tim, tpm, mtg)
    where tpm = fromMaybe 0 $ findTInc c cs
          tim = fromMaybe 0 $ findTime c cs
          mtg = fromMaybe 0 $ findMovesToGo cs

-- These parameters should be optimised (i.e.: first made options)
remTimeFracIni, remTimeFracFin, remTimeFracDev :: Double
remTimeFracIni = 0.15	-- fraction of remaining time which we can consume at once - initial value
remTimeFracFin = 0.5	-- same at final (when remaining time is near zero)
remTimeFracDev = remTimeFracFin - remTimeFracIni

timeReserved :: Int
timeReserved = 100	-- milliseconds reserved for move communication

extendScoreMargin :: Int
extendScoreMargin = 24

-- This function calculates the normal time for the next search loop,
-- the maximum of that (which cannot be exceeded)
-- and if we are in time troubles or not
compTime :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
compTime tim tpm fixmtg cursc rept
    | tim == 0 && tpm == 0 = (  0,   0)
    | otherwise            = (ctm, tmx)
    where mtg = if fixmtg > 0 then fixmtg else estimateMovesToGo cursc
          mtr | rept >= 2 = 3	-- consider repetitions
              | mtg == 0  = 1
              | otherwise = mtg
          ctn = tpm + tim `div` mtr
          ctm = if tim > 0 && tim < 2000 || tim == 0 && tpm < 700 then 300 else ctn
          frtim = fromIntegral $ max 0 $ tim - ctm	-- rest time after this move
          fctm  = fromIntegral ctm :: Double
          rtimprc = fctm / max frtim fctm
          rtimfrc = remTimeFracIni + remTimeFracDev * rtimprc
          tmxt = round $ fctm + rtimfrc * frtim
          tmx  = min tmxt $ max 0 $ tim - timeReserved

estMvsToGo :: Array Int Int
estMvsToGo = listArray (0, 8) [50, 36, 24, 15, 10, 7, 5, 3, 2]

estimateMovesToGo :: Int -> Int
estimateMovesToGo sc = estMvsToGo ! mvidx
    where mvidx = min 8 $ abs sc `div` 100

newThread :: CtxIO () -> CtxIO ThreadId
newThread a = do
    ctx <- ask
    liftIO $ forkIO $ runReaderT a ctx

startWorking :: Int -> Int -> Int -> Int -> Int -> CtxIO ()
startWorking tim tpm mtg dpt rept = do
    ctx <- ask
    currms <- lift $ currMilli (strttm ctx)
    ctxLog DebugUci $ "Start at " ++ show currms
        ++ " to search: " ++ show tim ++ " / " ++ show tpm ++ " / " ++ show mtg
        ++ " - maximal " ++ show dpt ++ " plys"
    modifyChanging $ \c -> c { working = True, srchStrtMs = currms, totBmCh = 0,
                               lastChDr = 0, crtStatus = posNewSearch (crtStatus c) }
    tid <- newThread (startSearchThread tim tpm mtg dpt rept)
    modifyChanging (\c -> c { compThread = Just tid })
    return ()

-- We use modifyChanging in at least 2 threads: in the reader and
-- in the search thread (here in giveBestMove)
-- This is not good, then it can lead to race conditions. We should
-- find another scheme, for example with STM
startSearchThread :: Int -> Int -> Int -> Int -> Int -> CtxIO ()
startSearchThread tim tpm mtg dpt rept =
    ctxCatch (void $ searchTheTree 1 dpt 0 0 tim tpm mtg rept Nothing [] [])
        $ \e -> do
            chg <- readChanging
            let mes = "searchTheTree terminated by exception: " ++ show e
            answer $ infos mes
            case forGui chg of
                Just ms -> giveBestMove ms
                Nothing -> return ()
            ctxLog LogError mes
            lift $ collectError $ SomeException (SearchException mes)

data SearchException = SearchException String deriving (Show, Typeable)

instance Exception SearchException

ctxCatch :: CtxIO a -> (SomeException -> CtxIO a) -> CtxIO a
ctxCatch a f = do
    ctx <- ask
    liftIO $ catch (runReaderT a ctx)
            (\e -> runReaderT (f e) ctx)

-- Search with the given depth
searchTheTree :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Maybe Int -> [Move] -> [Move] -> CtxIO Int
searchTheTree draft mdraft timx1 timx tim tpm mtg rept lsc lpv rmvs = do
    ctxLog LogInfo $ "searchTheTree starts draft " ++ show draft
    ctx <- ask
    chg <- readChanging
    ctxLog LogInfo $ "Time = " ++ show tim ++ " Timx1 = " ++ show timx1 ++ " Timx = " ++ show timx
    (path, sc, rmvsf, timint, stfin, ch) <- bestMoveCont draft timx1 timx (crtStatus chg) lsc lpv rmvs
    let totch = totBmCh chg + ch
        ldCh | ch > 0    = draft
             | otherwise = lastChDr chg
    when (ch > 0) $
        ctxLog LogInfo $ "Changes in draft " ++ show draft ++ ": " ++ show ch ++ " / " ++ show totch
    modifyChanging $
        \c -> c { crtStatus = stfin, totBmCh = totch, lastChDr = ldCh, forGui = Just path }
    currms <- lift $ currMilli (strttm ctx)
    let (ms, mx) = compTime tim tpm mtg sc rept
        exte = maybe False id $ do
                  los <- lsc
                  gls <- lmvScore chg
                  return $ sc < los - extendScoreMargin
                        || sc < gls - extendScoreMargin
        reds = case lmvScore chg of
                   Just osc -> timeProlongation osc sc
                   _        -> 1
        redp  = reduceBegin $ realPly chg
        start = srchStrtMs chg
        used  = currms - start
        over  = mx > 0 && used >= mx
        onlyone = ms > 0 && length rmvsf == 1 && draft >= 4	-- only in normal play
        draftmax = draft >= mdraft	--  or maximal draft
        mes = "Draft " ++ show draft ++ " Score " ++ show sc ++ " path " ++ show path
                  ++ " ms " ++ show ms ++ " used " ++ show used
    ctxLog LogInfo mes
    ctxLog LogInfo $ "Time factors (reds/redp): " ++ show reds ++ " / " ++ show redp
    (justStop, mxr) <- if mx > 0
                          then stopByChance (reds * redp) exte ms used mx draft ch totch ldCh
                          else return (False, 0)
    ctxLog LogInfo $ "compTime (ms/mx/mxr): " ++ show ms ++ " / " ++ show mx ++ " / " ++ show mxr
    if draftmax || timint || over || onlyone || justStop
       then do
           ctxLog LogInfo $ "searchTheTree terminated in first if: "
               ++ show draftmax ++ "/"
               ++ show timint ++ "/"
               ++ show over ++ "/"
               ++ show onlyone ++ "/"
               ++ show justStop
           -- Store last score for this move
           modifyChanging $ \c -> c { lmvScore = Just sc }
           giveBestMove path
           return sc
       else do
           ctxLog LogInfo $ "searchTheTree finishes draft " ++ show draft
           chg' <- readChanging
           if working chg'
               then if mx == 0	-- no time constraint (take original maximum)
                       then searchTheTree (draft+1) mdraft 0 0 tim tpm mtg rept (Just sc) path rmvsf
                       else do
                           let mxt  = start + mxr
                               mxt1 = start + mxr `div` firstMoveTimeReductionFactor
                           searchTheTree (draft+1) mdraft mxt1 mxt tim tpm mtg rept (Just sc) path rmvsf
               else do
                   ctxLog DebugUci "in searchTheTree: not working"
                   giveBestMove path -- was stopped
                   return sc
    where firstMoveTimeReductionFactor = 3

-- The time management changes like this:
-- We calculate the normal and maximum time to use for this move, as before
-- we correct the time per move with some factor (ply in game, score drop)
-- we calculate what would be the max draft we could reach at this rate
-- if we reached the max draft - stop the search
-- else we calculate a probability that next draft will change the best move based on:
-- - last draft
-- - number of changes in last draft
-- - total number of changes in this search
-- - last draft with changes
-- Than based on this probability we limit the interrupt time for next search (in case it starts)
-- if max draft is greater than next draft we start next search
-- if max draft = next draft we decide probabilistically if we start the next draft or not
stopByChance :: Double -> Bool -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> CtxIO (Bool, Int)
stopByChance red extend ms used mx draft ch totch ldCh
    | extend      = return (False, mx)
    | used >= msr = return (True, mx)
    | otherwise   = do
    let dmax = maxDepthAtThisRate draft used msr
    ctxLog LogInfo $ "stopByChance: draft = " ++ show draft ++ " dmax = " ++ show dmax
    if dmax < draft
       then return (True, mx)
       else do
          -- We need the probability to limit the interrupt time at least:
          let imbalance = 1
              p = imbalance * probChange (fromIntegral draft) (fromIntegral ch)
                                         (fromIntegral totch) (fromIntegral ldCh)
              mxf = fromIntegral mx
              mxr = min mx $ round $ red * (mxf + mxf * p) / 2
          if dmax > draft + 1
             then return (False, mxr)	-- we have more than 1 draft to go
             else do
                 ctxLog LogInfo $ "stopByChance: d/ch/tch/ldch = "
                     ++ show draft ++ " / " ++ show ch ++ " / " ++ show totch ++ " / " ++ show ldCh
                 ctxLog LogInfo $ "stopByChance: p = " ++ show p
                 r <- liftIO $ getStdRandom (randomR (0::Double, 1))
                 if r < p then return (False, mxr) else return (True, mxr)
    where msr = round $ red * fromIntegral ms

maxDepthAtThisRate :: Int -> Int -> Int -> Int
maxDepthAtThisRate d used ms
    | d == 1    = floor dmax1
    | otherwise = floor dmax
    where branchingFactor = 1.5 :: Double
          logs  = 1 / log branchingFactor
          bfd   = branchingFactor ** fromIntegral d
          msf   = fromIntegral ms
          usedf = fromIntegral (used+1)
          dmax  = (log (bfd * msf - msf + usedf) - log usedf) * logs
          dmax1 = (log (msf + usedf) - log usedf) * logs

-- This is just a prediction using logistic regression on the 4 parameters
-- The parameters were found outside and have reached 68,7% prediction rate
-- 600k samples, class_weight balanced, scaled, L2, C=100
probChange :: Double -> Double -> Double -> Double -> Double
probChange d c ct dc = 1 / (1 + exp (-z))
    where w0 = 1.701
          w1 = -4.977 / 19
          w2 = 3.269 / 6
          w3 =  0.723 / 18
          w4 = 2.729 / 19
          z  = w0 + w1 * d + w2 * c + w3 * ct + w4 * dc

reduceBegin :: Maybe Int -> Double
reduceBegin mi | Just i <- mi = reduce i
               | otherwise    = 1
    where reduce i | i < 6     = 0.5
                   | i < 10    = fromIntegral i / 10
                   | otherwise = 1

timeProlongation :: Int -> Int -> Double
timeProlongation osc sc
    | sc >= osc - tpMargin = 1
    | otherwise            = 1 + log ((oscf - scf) / fm)
    where oscf = fromIntegral osc
          scf  = fromIntegral sc
          tpMargin = 8
          fm = fromIntegral tpMargin

giveBestMove :: [Move] -> CtxIO ()
giveBestMove mvs = do
    -- ctxLog "Info" $ "The moves: " ++ show mvs
    modifyChanging $ \c -> c { working = False, compThread = Nothing, forGui = Nothing }
    if null mvs
        then answer $ infos "empty pv"
        else answer $ bestMove (head mvs) Nothing
    cng <- readChanging
    let mst = mstats $ crtStatus cng
    ctxLog LogInfo $ "Search statistics:"
    mapM_ (ctxLog LogInfo) $ formatStats mst

beforeReadLoop :: CtxIO ()
beforeReadLoop = do
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

doStop :: CtxIO ()
doStop = do
    chg <- readChanging
    modifyChanging $ \c -> c { working = False, compThread = Nothing }
    case compThread chg of
        Just tid -> do
            liftIO $ killThread tid
            case forGui chg of
                Just ms -> giveBestMove ms
                Nothing  -> return ()
        _ -> return ()

doPonderhit :: CtxIO ()
doPonderhit = notImplemented "doPonderhit"

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
    tm <- getMyTime
    ef <- openFile efname AppendMode
    hPutStrLn ef $ formatMyTime tm ++ " " ++ idName ++ ": " ++ show e
    hClose ef
    where cannot :: IOException -> IO ()
          cannot _ = return ()
