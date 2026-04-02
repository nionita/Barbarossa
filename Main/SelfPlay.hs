{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where
import Control.Monad.Reader
import Control.Monad (when, void, forever, foldM)
import Control.Concurrent
import Control.Exception
import Data.Char (isSpace)
import Data.List (intersperse)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Foreign hiding (void)
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, utcToLocalTime)
import Text.Printf (printf)
import Text.Read (readMaybe)
-- import System.Time

import SelfPlay.SPRT
    ( PairWdl(..)
    , PentaScore(..)
    , SprtConfig(..)
    , SprtResult(..)
    , emptyPenta
    , addPentaScore
    , classifyPair
    , mkSprtState
    , sprtResult
    , sprtPentaLLR
    )
import Struct.Struct
import Struct.Status
import Struct.Context
import Struct.Config
import Hash.TransTab
import Search.AlbetaTypes (DoResult(..), SStats(..), ssts0)
import Moves.Core
import Moves.Internal.Base
import Moves.Notation
import Moves.History
import Search.CStateMonad (CState, runCState, execCState)
import Eval.FileParams (makeEvalState)
import Eval.Core (initEvalState)
-- import Eval.Eval	-- not yet needed
import Uci.UciGlue

debug :: Bool
debug = False

data Options = Options {
        optPlayer1  :: Maybe String,	-- player 1 config file
        optPlayer2  :: Maybe String,	-- player 2 config file
        optBaseCurrent :: Bool,	-- player 2 uses compiled-in current weights
        optConfFile :: Maybe String,	-- config file
        optParams   :: [String],	-- list of eval parameter assignements
        -- optNThreads :: Int,		-- number of threads - not used for self play now
        optDepth    :: Int,		-- search depth for self play
        optLogLev   :: LogLevel,		-- log level: 0 (debug) to 5 (never)
        optNodes    :: Maybe Int,	-- search nodes per move for self play
        optNodeMargin :: Int,	-- percentual safety margin for nodes passed to search
        optNSkip    :: Maybe Int,	-- number of fens to skip (Nothing = none)
        optNFens    :: Maybe Int,	-- number of fens (Nothing = all)
        optMatch    :: Maybe String,	-- match between configs in the given directory
        optSprtDir  :: Maybe String,	-- SPRT match between configs in the given directory
        optPerfTest :: Bool,		-- perf test on input fen file
        optPerft    :: Bool,		-- perft on a single FEN
        optBuildInfo :: Bool,		-- print build flavor and opposite build command
        optSprtAlpha :: Maybe Double,	-- SPRT alpha
        optSprtBeta  :: Maybe Double,	-- SPRT beta
        optSprtElo0  :: Maybe Double,	-- SPRT elo0
        optSprtElo1  :: Maybe Double,
        -- SPRT elo1
        optSprtSaveMinutes :: Maybe Int,	-- SPRT save interval in minutes
        optSprtLoad :: Maybe FilePath,	-- explicit SPRT save file to load
        optStatsEvery :: Int,
        -- print match stats every Nth input fen
        optFenPrintEvery :: Int,
        -- print every Nth input fen
        optFen      :: Maybe String,	-- single fen for perft
        optAFenFile :: FilePath,	-- fen file with start positions
        optFOutFile :: FilePath		-- output file for filter option
    }

defaultOptions :: Options
defaultOptions = Options {
        optPlayer1  = Nothing,
        optPlayer2  = Nothing,
        optBaseCurrent = False,
        optConfFile = Nothing,
        optParams   = [],
        -- optNThreads = 1,
        optDepth    = 1,
        optLogLev   = LogNever,		-- default: never log
        optNodes    = Nothing,
        optNodeMargin = 50,
        optNSkip    = Nothing,
        optNFens    = Nothing,
        optMatch    = Nothing,
        optSprtDir  = Nothing,
        optPerfTest = False,
        optPerft    = False,
        optBuildInfo = False,
        optSprtAlpha = Nothing,
        optSprtBeta  = Nothing,
        optSprtElo0  = Nothing,
        optSprtElo1  = Nothing,
        optSprtSaveMinutes = Nothing,
        optSprtLoad = Nothing,
        optStatsEvery = 10,
        optFenPrintEvery = 10,
        optFen      = Nothing,
        optAFenFile = "alle.epd",
        optFOutFile = "vect.txt"
    }
setPlayer1 :: String -> Options -> Options
setPlayer1 cf opt = opt { optPlayer1 = Just cf }

setPlayer2 :: String -> Options -> Options
setPlayer2 cf opt = opt { optPlayer2 = Just cf }

setBaseCurrent :: Options -> Options
setBaseCurrent opt = opt { optBaseCurrent = True }

setConfFile :: String -> Options -> Options
setConfFile cf opt = opt { optConfFile = Just cf }

addParam :: String -> Options -> Options
addParam pa opt = opt { optParams = pa : optParams opt }

-- addNThrds :: String -> Options -> Options
-- addNThrds ns opt = opt { optNThreads = read ns }

addDepth :: String -> Options -> Options
addDepth ns opt = opt { optDepth = read ns }

-- When we use nodes, set the depth high enough to ignore it
-- If we want to limit also the depth: set depth after nodes
addNodes :: String -> Options -> Options
addNodes ns opt = opt { optNodes = Just $ read ns, optDepth = 40 }

addNodeMargin :: String -> Options -> Options
addNodeMargin ns opt = opt { optNodeMargin = read ns }

addNSkip :: String -> Options -> Options
addNSkip ns opt = opt { optNSkip = Just $ read ns }

addNFens :: String -> Options -> Options
addNFens ns opt = opt { optNFens = Just $ read ns }

addMatch :: String -> Options -> Options
addMatch ns opt = opt { optMatch = Just ns }

addSprtDir :: String -> Options -> Options
addSprtDir ns opt = opt { optSprtDir = Just ns }

setPerfTest :: Options -> Options
setPerfTest opt = opt { optPerfTest = True }

setPerft :: Options -> Options
setPerft opt = opt { optPerft = True }

setBuildInfo :: Options -> Options
setBuildInfo opt = opt { optBuildInfo = True }

addSprtAlpha :: String -> Options -> Options
addSprtAlpha ns opt = opt { optSprtAlpha = Just $ read ns }

addSprtBeta :: String -> Options -> Options
addSprtBeta ns opt = opt { optSprtBeta = Just $ read ns }

addSprtElo0 :: String -> Options -> Options
addSprtElo0 ns opt = opt { optSprtElo0 = Just $ read ns }

addSprtElo1 :: String -> Options -> Options
addSprtElo1 ns opt = opt { optSprtElo1 = Just $ read ns }

addSprtSaveMinutes :: String -> Options -> Options
addSprtSaveMinutes ns opt = opt { optSprtSaveMinutes = Just $ read ns }

addSprtLoad :: FilePath -> Options -> Options
addSprtLoad fp opt = opt { optSprtLoad = Just fp }

addStatsEvery :: String -> Options -> Options
addStatsEvery ns opt = opt { optStatsEvery = read ns }

addIFile :: FilePath -> Options -> Options
addIFile fi opt = opt { optAFenFile = fi }

addFen :: String -> Options -> Options
addFen fen opt = opt { optFen = Just fen }

addOFile :: FilePath -> Options -> Options
addOFile fi opt = opt { optFOutFile = fi }

setLogLev :: String -> Options -> Options
setLogLev lv opt = opt { optLogLev = llev }
    where llev = case levi of
                     0 -> DebugSearch
                     1 -> DebugUci
                     2 -> LogInfo
                     3 -> LogWarning
                     4 -> LogError
                     _ -> if levi < 0 then DebugSearch else LogNever
          levi = read lv :: Int

options :: [OptDescr (Options -> Options)]
options = [
        Option "a" ["player1"] (ReqArg setPlayer1 "STRING") "Configuration file for player 1",
        Option "b" ["player2"] (ReqArg setPlayer2 "STRING") "Configuration file for player 2",
        Option "" ["base-current"] (NoArg setBaseCurrent) "Use current compiled weights as player 2 baseline",
        Option "c" ["config"]  (ReqArg setConfFile "STRING") "Configuration file",
        Option "p" ["param"]   (ReqArg addParam "STRING") "Eval/search/time params: name=value,...",
        Option "m" ["match"]   (ReqArg addMatch "STRING") "Match between 2 configs in the given directory",
        Option "" ["sprt"]     (ReqArg addSprtDir "STRING") "SPRT match between 2 configs in the given directory",
        Option "P" ["perf"]    (NoArg setPerfTest) "Performance test on input FEN file",
        Option "" ["perft"]    (NoArg setPerft) "Perft on a single FEN",
        Option "B" ["build-info"] (NoArg setBuildInfo) "Show build flavor and opposite build command",
        Option "" ["sprt-alpha"] (ReqArg addSprtAlpha "DOUBLE") "SPRT alpha (default 0.05 when SPRT is enabled)",
        Option "" ["sprt-beta"]  (ReqArg addSprtBeta  "DOUBLE") "SPRT beta (default 0.05 when SPRT is enabled)",
        Option "" ["sprt-elo0"]  (ReqArg addSprtElo0  "DOUBLE") "SPRT H0 elo bound (default 0.0 when SPRT is enabled)",
        Option "" ["sprt-elo1"]  (ReqArg addSprtElo1  "DOUBLE") "SPRT H1 elo bound (default 5.0 when SPRT is enabled)",
        Option "" ["sprt-save-minutes"] (ReqArg addSprtSaveMinutes "INT") "Save SPRT state every N minutes at the next stats print (default 15)",
        Option "" ["sprt-load"] (ReqArg addSprtLoad "FILE") "Resume SPRT from a specific .sav file",
        Option "" ["stats-every"] (ReqArg addStatsEvery "INT") "Print match stats every Nth played pair (default 10)",
        Option "i" ["input"]   (ReqArg addIFile "STRING") "Input (fen) file",
        Option "" ["fen"]      (ReqArg addFen "STRING") "Input FEN for perft",
        Option "o" ["output"]  (ReqArg addOFile "STRING") "Output file",
        Option "d" ["depth"]   (ReqArg addDepth "STRING") "Search depth",
        Option "n" ["nodes"]   (ReqArg addNodes "STRING") "Search nodes budget per move",
        Option "M" ["node-margin"] (ReqArg addNodeMargin "INT") "Safety margin percent for node budget (default 50)",
        -- Option "t" ["threads"] (ReqArg addNThrds "STRING") "Number of threads",
        Option "s" ["skip"]    (ReqArg addNSkip "STRING")  "Number of fens to skip",
        Option "f" ["fens"]    (ReqArg addNFens "STRING")  "Number of fens to play",
        Option "l" ["log"]     (ReqArg setLogLev "STRING") "Log leven from 0 to 5 (debug to never)"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName
              ++ " [-c CONF] [(-m DIR | --sprt DIR) [-a CFILE1] (-b CFILE2 | --base-current)] [-P] [--perft --fen FEN] [-B] [-i FENFILE [-s SKIP][-f FENS]] [-o OUTFILE] [-d DEPTH]"
          idName = "SelfPlay"

saveFileName :: FilePath
saveFileName = "selfplay.sav"

data SprtSave = SprtSave {
        savePlayer1        :: !FilePath,
        savePlayer2        :: !(Maybe FilePath),
        saveBaseCurrent    :: !Bool,
        saveInputFile      :: !FilePath,
        saveOrigSkip       :: !Int,
        saveOrigPairs      :: !Int,
        saveNextPairIndex  :: !Int,
        saveRemainingPairs :: !Int,
        saveDepth          :: !Int,
        saveNodes          :: !(Maybe Int),
        saveNodeMargin     :: !Int,
        saveStatsEvery     :: !Int,
        saveSprtCfg        :: !SprtConfig,
        saveSprtMinutes    :: !Int,
        savePairsDone      :: !Int,
        savePairsTried     :: !Int,
        saveWdl            :: !GameScore,
        savePenta          :: !PentaScore,
        saveLastLLR        :: !(Maybe Double),
        saveVerdict        :: !SprtResult,
        saveElapsedSeconds :: !Double
    }

canonicalSavePath :: FilePath -> FilePath
canonicalSavePath dir = dir </> saveFileName

discoverSprtSaveForOptions :: Options -> IO (Maybe FilePath)
discoverSprtSaveForOptions opts =
    case optSprtDir opts of
        Nothing -> return Nothing
        Just dir -> discoverSprtSaveInDir dir opts

discoverSprtSaveInDir :: FilePath -> Options -> IO (Maybe FilePath)
discoverSprtSaveInDir dir opts =
    case optSprtLoad opts of
        Just fp -> do
            let path = resolveSavePath dir fp
            ex <- doesFileExist path
            if ex
               then return $ Just path
               else ioError $ userError $ "SPRT save file not found: " ++ path
        Nothing -> do
            ex <- doesDirectoryExist dir
            if not ex
               then return Nothing
               else do
                   files <- listDirectory dir
                   let savs = [dir </> f | f <- files, takeExtension f == ".sav"]
                   case savs of
                       []  -> return Nothing
                       [f] -> return $ Just f
                       _   -> ioError $ userError $ "More than one .sav file found in " ++ dir

resolveSavePath :: FilePath -> FilePath -> FilePath
resolveSavePath dir fp
    | isAbsolute fp = fp
    | otherwise = dir </> fp

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

readSaveFile :: FilePath -> IO SprtSave
readSaveFile fileName = do
    ls <- readAllLinesStrict fileName
    let useful = filter (not . null) $ map trim $ filter (not . isComment) ls
        isComment s = case dropWhile isSpace s of
            '-':'-':_ -> True
            _         -> False
        parseLine ln = case break (== '=') ln of
            (k, '=':v) -> Right (trim k, trim v)
            _          -> Left $ "Invalid save file line: " ++ ln
    entries <- case traverse parseLine useful of
        Left err -> ioError $ userError err
        Right xs -> return xs
    mp <- foldM insertUnique M.empty entries
    SprtSave
        <$> reqRead "player1" mp
        <*> reqRead "player2" mp
        <*> reqRead "baseCurrent" mp
        <*> reqRead "inputFile" mp
        <*> reqRead "origSkip" mp
        <*> reqRead "origPairs" mp
        <*> reqRead "nextPairIndex" mp
        <*> reqRead "remainingPairs" mp
        <*> reqRead "depth" mp
        <*> reqRead "nodes" mp
        <*> reqRead "nodeMargin" mp
        <*> reqRead "statsEvery" mp
        <*> reqRead "sprtCfg" mp
        <*> reqRead "sprtMinutes" mp
        <*> reqRead "pairsDone" mp
        <*> reqRead "pairsTried" mp
        <*> ((\w d l -> GameScore w d l) <$> reqRead "wdlW" mp <*> reqRead "wdlD" mp <*> reqRead "wdlL" mp)
        <*> ((\ww wd wl dd ld ll -> PentaScore ww wd wl dd ld ll)
                <$> reqRead "pWW" mp <*> reqRead "pWD" mp <*> reqRead "pWL" mp
                <*> reqRead "pDD" mp <*> reqRead "pLD" mp <*> reqRead "pLL" mp)
        <*> reqRead "lastLLR" mp
        <*> reqRead "verdict" mp
        <*> reqRead "elapsedSeconds" mp
    where
      insertUnique mp (k, v)
          | M.member k mp = ioError $ userError $ "Duplicate save file field: " ++ k
          | otherwise = return $ M.insert k v mp
      reqRead :: Read a => String -> M.Map String String -> IO a
      reqRead key mp = case M.lookup key mp of
          Nothing -> ioError $ userError $ "Missing save file field: " ++ key
          Just v -> case readMaybe v of
              Just a  -> return a
              Nothing -> ioError $ userError $ "Invalid save file field: " ++ key ++ "=" ++ v

writeSaveFile :: FilePath -> SprtSave -> IO ()
writeSaveFile fileName save = do
    let tmp = fileName ++ ".tmp"
        GameScore w d l = saveWdl save
        PentaScore ww wd wl dd ld ll = savePenta save
        ls =
            [ "-- SelfPlay SPRT save file"
            , "player1 = " ++ show (savePlayer1 save)
            , "player2 = " ++ show (savePlayer2 save)
            , "baseCurrent = " ++ show (saveBaseCurrent save)
            , "inputFile = " ++ show (saveInputFile save)
            , "origSkip = " ++ show (saveOrigSkip save)
            , "origPairs = " ++ show (saveOrigPairs save)
            , "nextPairIndex = " ++ show (saveNextPairIndex save)
            , "remainingPairs = " ++ show (saveRemainingPairs save)
            , "depth = " ++ show (saveDepth save)
            , "nodes = " ++ show (saveNodes save)
            , "nodeMargin = " ++ show (saveNodeMargin save)
            , "statsEvery = " ++ show (saveStatsEvery save)
            , "sprtCfg = " ++ show (saveSprtCfg save)
            , "sprtMinutes = " ++ show (saveSprtMinutes save)
            , "pairsDone = " ++ show (savePairsDone save)
            , "pairsTried = " ++ show (savePairsTried save)
            , "wdlW = " ++ show w
            , "wdlD = " ++ show d
            , "wdlL = " ++ show l
            , "pWW = " ++ show ww
            , "pWD = " ++ show wd
            , "pWL = " ++ show wl
            , "pDD = " ++ show dd
            , "pLD = " ++ show ld
            , "pLL = " ++ show ll
            , "lastLLR = " ++ show (saveLastLLR save)
            , "verdict = " ++ show (saveVerdict save)
            , "elapsedSeconds = " ++ show (saveElapsedSeconds save)
            ]
    writeFile tmp $ unlines ls
    ex <- doesFileExist fileName
    when ex $ removeFile fileName
    renameFile tmp fileName

saveToOptions :: Options -> SprtSave -> Options
saveToOptions opts save = opts {
        optPlayer1 = Just $ savePlayer1 save,
        optPlayer2 = savePlayer2 save,
        optBaseCurrent = saveBaseCurrent save,
        optDepth = saveDepth save,
        optNodes = saveNodes save,
        optNodeMargin = saveNodeMargin save,
        optNSkip = Just $ saveOrigSkip save,
        optNFens = Just $ resumeTotalPairs opts save,
        optSprtAlpha = Just $ sprtAlpha $ saveSprtCfg save,
        optSprtBeta = Just $ sprtBeta $ saveSprtCfg save,
        optSprtElo0 = Just $ sprtElo0 $ saveSprtCfg save,
        optSprtElo1 = Just $ sprtElo1 $ saveSprtCfg save,
        optSprtSaveMinutes = Just $ saveSprtMinutes save,
        optStatsEvery = saveStatsEvery save,
        optAFenFile = saveInputFile save
    }

resumeTotalPairs :: Options -> SprtSave -> Int
resumeTotalPairs opts save =
    case optNFens opts of
        Just n | canExtendSavedSprt save && n > saveOrigPairs save -> n
        _ -> saveOrigPairs save

canExtendSavedSprt :: SprtSave -> Bool
canExtendSavedSprt save = saveVerdict save == SprtContinue && saveRemainingPairs save == 0

validateResumeConflicts :: Options -> SprtSave -> IO ()
validateResumeConflicts opts save = do
    chkMaybe "player1" (optPlayer1 opts) Nothing (Just $ savePlayer1 save)
    chkMaybe "player2" (optPlayer2 opts) Nothing (savePlayer2 save)
    chkBool "base-current" (optBaseCurrent opts) False (saveBaseCurrent save)
    chkVal "input" (optAFenFile opts) (optAFenFile defaultOptions) (saveInputFile save)
    chkMaybe "skip" (optNSkip opts) Nothing (Just $ saveOrigSkip save)
    chkFens
    chkVal "depth" (optDepth opts) (optDepth defaultOptions) (saveDepth save)
    chkMaybe "nodes" (optNodes opts) Nothing (saveNodes save)
    chkVal "node-margin" (optNodeMargin opts) (optNodeMargin defaultOptions) (saveNodeMargin save)
    chkVal "stats-every" (optStatsEvery opts) (optStatsEvery defaultOptions) (saveStatsEvery save)
    chkMaybe "sprt-alpha" (optSprtAlpha opts) Nothing (Just $ sprtAlpha $ saveSprtCfg save)
    chkMaybe "sprt-beta" (optSprtBeta opts) Nothing (Just $ sprtBeta $ saveSprtCfg save)
    chkMaybe "sprt-elo0" (optSprtElo0 opts) Nothing (Just $ sprtElo0 $ saveSprtCfg save)
    chkMaybe "sprt-elo1" (optSprtElo1 opts) Nothing (Just $ sprtElo1 $ saveSprtCfg save)
    chkMaybe "sprt-save-minutes" (optSprtSaveMinutes opts) Nothing (Just $ saveSprtMinutes save)
    where
      chkVal name cur def saved =
          when (cur /= def && cur /= saved) $
              ioError $ userError $ "--" ++ name ++ " conflicts with the SPRT save file"
      chkBool name cur def saved =
          when (cur /= def && cur /= saved) $
              ioError $ userError $ "--" ++ name ++ " conflicts with the SPRT save file"
      chkMaybe name cur def saved =
          when (cur /= def && cur /= saved) $
              ioError $ userError $ "--" ++ name ++ " conflicts with the SPRT save file"
      chkFens = case optNFens opts of
          Nothing -> return ()
          Just n
              | n == saveOrigPairs save -> return ()
              | canExtendSavedSprt save && n > saveOrigPairs save -> return ()
              | otherwise -> ioError $ userError "--fens conflicts with the SPRT save file"

matchDir :: Options -> Maybe String
matchDir opts = case (optMatch opts, optSprtDir opts) of
    (Just _, Just _) -> Nothing
    (mdir, Nothing)  -> mdir
    (Nothing, sdir)  -> sdir

sprtEnabled :: Options -> Bool
sprtEnabled opts = isJust (optSprtDir opts) || any isJust
    [optSprtAlpha opts, optSprtBeta opts, optSprtElo0 opts, optSprtElo1 opts]

getSprtSaveMinutes :: Options -> Int
getSprtSaveMinutes opts = fromMaybe 15 (optSprtSaveMinutes opts)

getSprtConfig :: Options -> SprtConfig
getSprtConfig opts = SprtConfig {
        sprtAlpha = fromMaybe 0.05 (optSprtAlpha opts),
        sprtBeta  = fromMaybe 0.05 (optSprtBeta opts),
        sprtElo0  = fromMaybe 0.0  (optSprtElo0 opts),
        sprtElo1  = fromMaybe 5.0  (optSprtElo1 opts)
    }

validateOptions :: Options -> IO ()
validateOptions opts = do
    mresume <- discoverSprtSaveForOptions opts
    let cfg = getSprtConfig opts
        needsPlayers = isJust (matchDir opts) && not (isJust mresume && isJust (optSprtDir opts))
    if | optMatch opts /= Nothing && optSprtDir opts /= Nothing
           -> ioError $ userError "--match cannot be combined with --sprt"
       | optPerfTest opts && isJust (matchDir opts)
           -> ioError $ userError "--perf cannot be combined with --match/--sprt"
       | optPerfTest opts && optPerft opts
           -> ioError $ userError "--perf cannot be combined with --perft"
       | optPerfTest opts && optFOutFile opts /= optFOutFile defaultOptions
           -> ioError $ userError "--perf cannot be combined with --output"
       | optPerfTest opts && (optPlayer1 opts /= Nothing || optPlayer2 opts /= Nothing)
           -> ioError $ userError "--perf cannot be combined with --player1/--player2"
       | optPerfTest opts && optBaseCurrent opts
           -> ioError $ userError "--perf cannot be combined with --base-current"
       | optBaseCurrent opts && optPlayer2 opts /= Nothing
           -> ioError $ userError "--base-current cannot be combined with --player2"
       | optSprtLoad opts /= Nothing && optSprtDir opts == Nothing
           -> ioError $ userError "--sprt-load requires --sprt"
       | optMatch opts /= Nothing && optSprtSaveMinutes opts /= Nothing
           -> ioError $ userError "--sprt-save-minutes requires --sprt"
       | optMatch opts /= Nothing && optSprtLoad opts /= Nothing
           -> ioError $ userError "--sprt-load requires --sprt"
       | needsPlayers && optPlayer1 opts == Nothing
           -> ioError $ userError "--match requires --player1"
       | needsPlayers && not (optBaseCurrent opts) && optPlayer2 opts == Nothing
           -> ioError $ userError "--match requires either --player2 or --base-current"
       | optPerft opts && optMatch opts /= Nothing
           -> ioError $ userError "--perft cannot be combined with --match"
       | optPerft opts && optPerfTest opts
           -> ioError $ userError "--perft cannot be combined with --perf"
       | optPerft opts && optAFenFile opts /= optAFenFile defaultOptions
           -> ioError $ userError "--perft cannot be combined with --input"
       | optPerft opts && optFOutFile opts /= optFOutFile defaultOptions
           -> ioError $ userError "--perft cannot be combined with --output"
       | optPerft opts && (optPlayer1 opts /= Nothing || optPlayer2 opts /= Nothing)
           -> ioError $ userError "--perft cannot be combined with --player1/--player2"
       | optPerft opts && optNodes opts /= Nothing
           -> ioError $ userError "--perft cannot be combined with --nodes"
       | optPerft opts && optNodeMargin opts /= optNodeMargin defaultOptions
           -> ioError $ userError "--perft cannot be combined with --node-margin"
       | optPerft opts && optNSkip opts /= Nothing
           -> ioError $ userError "--perft cannot be combined with --skip"
       | optPerft opts && optNFens opts /= Nothing
           -> ioError $ userError "--perft cannot be combined with --fens"
       | optPerft opts && optFen opts == Nothing
           -> ioError $ userError "--perft requires --fen"
       | optPerft opts && optDepth opts < 1
           -> ioError $ userError "--perft requires --depth >= 1"
       | optNodeMargin opts < 0 || optNodeMargin opts > 99
           -> ioError $ userError "--node-margin must be in [0,99]"
       | optStatsEvery opts <= 0
           -> ioError $ userError "--stats-every must be > 0"
       | isJust (optSprtDir opts) && getSprtSaveMinutes opts <= 0
           -> ioError $ userError "--sprt-save-minutes must be > 0"
       | sprtEnabled opts && not (isJust (matchDir opts))
           -> ioError $ userError "SPRT options require --match or --sprt"
       | sprtEnabled opts && (sprtAlpha cfg <= 0 || sprtAlpha cfg >= 1)
           -> ioError $ userError "SPRT alpha must be between 0 and 1"
       | sprtEnabled opts && (sprtBeta cfg <= 0 || sprtBeta cfg >= 1)
           -> ioError $ userError "SPRT beta must be between 0 and 1"
       | sprtEnabled opts && sprtAlpha cfg + sprtBeta cfg >= 1
           -> ioError $ userError "SPRT requires alpha + beta < 1"
       | sprtEnabled opts && sprtElo0 cfg >= sprtElo1 cfg
           -> ioError $ userError "SPRT requires elo0 < elo1"
       | optFenPrintEvery opts <= 0
           -> ioError $ userError "--print-fen-every must be > 0"
       | otherwise -> return ()

buildFlavor, otherBuildCmd :: String
#ifdef REPRO_HIST
buildFlavor = "reproducible history (REPRO_HIST enabled)"
otherBuildCmd = "stack build Barbarossa:exe:SelfPlay"
#else
buildFlavor = "default history (randomized small init)"
otherBuildCmd = "stack build --flag Barbarossa:reproselfplay Barbarossa:exe:SelfPlay"
#endif

showBuildInfo :: IO ()
showBuildInfo = do
    putStrLn $ "SelfPlay build flavor: " ++ buildFlavor
    putStrLn $ "To build the other flavor use:"
    putStrLn $ "  " ++ otherBuildCmd

initContext :: Options -> IO Context
initContext opts = do
    clktm <- getMyTime
    lchan <- newChan
    wchan <- newChan
    ha <- newCache 1	-- it will take the minimum number of entries
    hi <- newHist
    let paramList
            | null $ optParams opts = []
            | otherwise             = stringToParams $ concat $ intersperse "," $ optParams opts
    (parc, evs) <- makeEvalState (optConfFile opts) paramList "progver" "progsuf"
    let chg = Chg {
            searchToken = 0,
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
            loglev = if debug then DebugSearch else optLogLev opts,
            evpid  = parc
         }
    return context

main :: IO ()
main = do
    (opts, _) <- theOptions
    if optBuildInfo opts
       then showBuildInfo
       else do
           validateOptions opts
           ctx <- initContext opts
           runReaderT (startWriter False) ctx
           if optPerft opts
              then runReaderT (perftCommand opts) ctx
              else if optPerfTest opts
                      then runReaderT (perfTestFile opts) ctx
                      else case matchDir opts of
                        Nothing  -> runReaderT (filterFile opts) ctx
                        Just dir -> do
                            GameScore w d l <- runReaderT (matchFile opts dir) ctx
                            putStrLn $ "End result: (" ++ show w ++ "," ++ show d ++ "," ++ show l ++ ")"

filterFile :: Options -> CtxIO ()
filterFile opts = do
    ctx <- ask
    let logFileName = "selfplay-" ++ show (startSecond ctx) ++ ".log"
    startLogger logFileName
    lift $ do
        putStrLn $ "Vectorizing " ++ optAFenFile opts
        putStrLn $ "Play depth  " ++ show (optDepth opts)
        putStrLn $ "Results to  " ++ optFOutFile opts
        -- putStrLn $ "Marker: " ++ show markerEval
    hi <- liftIO $ openFile (optAFenFile opts) ReadMode
    ho <- liftIO $ openFile (optFOutFile opts) WriteMode
    case optNSkip opts of
        Just m  -> loopCount (skipLines hi m) ()
        Nothing -> return ()
    -- loopCount (oracleAndFeats (optDepth opts) hi ho (optNFens opts)) ()
    loopCount (balancedPos hi ho (optNFens opts)) ()
    liftIO $ do
        hClose ho
        hClose hi

data PerfAcc = PerfAcc {
        perfValid   :: !Int,
        perfInvalid :: !Int,
        perfNodes   :: !Int
    }

perftCommand :: Options -> CtxIO ()
perftCommand opts = do
    ctx <- ask
    let logFileName = "selfplay-" ++ show (startSecond ctx) ++ ".log"
        fen = fromMaybe "" (optFen opts)
    startLogger logFileName
    liftIO $ do
        putStrLn $ "Perft from FEN  " ++ fen
        putStrLn $ "Max depth       " ++ show (optDepth opts)
    posRes <- liftIO $ try (evaluate (posFromFen fen)) :: CtxIO (Either SomeException MyPos)
    case posRes of
        Left e -> liftIO $ ioError $ userError $ "Invalid FEN for --perft: " ++ show e
        Right pos -> do
            chg <- readChanging
            let crts = crtStatus chg
                sini = posToState pos (hash crts) (hist crts) (evalst crts)
            mapM_ (printPerftDepth sini) [1 .. optDepth opts]

printPerftDepth :: MyState -> Int -> CtxIO ()
printPerftDepth sini depth = do
    acc <- perftDivide sini depth
    liftIO $ do
        putStrLn $ "Depth " ++ show depth
        mapM_ (\(mv, nodes) -> putStrLn $ show mv ++ " " ++ show nodes) (perftMoves acc)
        putStrLn $ "Legal moves: " ++ show (perftLegalMoves acc)
        putStrLn $ "Nodes:       " ++ show (perftNodes acc)

data PerftDivide = PerftDivide {
        perftMoves      :: ![(Move, Integer)],
        perftLegalMoves :: !Int,
        perftNodes      :: !Integer
    }

perftDivide :: MyState -> Int -> CtxIO PerftDivide
perftDivide sini depth = do
    (moves, _) <- runCState (uncurry (++) <$> genMoves depth) sini
    foldM step (PerftDivide [] 0 0) moves
    where step acc mv = do
              (res, s1) <- runCState (doMove mv) sini
              case res of
                  Illegal -> return acc
                  _ -> do
                      (nodes, _s2) <- runCState (perftCount (depth - 1) <* undoMove) s1
                      let moveNodes = if depth == 1 then 1 else nodes
                      return acc {
                              perftMoves = perftMoves acc ++ [(mv, moveNodes)],
                              perftLegalMoves = perftLegalMoves acc + 1,
                              perftNodes = perftNodes acc + moveNodes
                          }

perftCount :: Int -> CState MyState CtxIO Integer
perftCount 0 = return 1
perftCount depth = do
    moves <- uncurry (++) <$> genMoves depth
    go moves 0
    where go [] !acc = return acc
          go (mv:mvs) !acc = do
              res <- doMove mv
              case res of
                  Illegal -> go mvs acc
                  _ -> do
                      nodes <- perftCount (depth - 1)
                      undoMove
                      go mvs (acc + nodes)

perfTestFile :: Options -> CtxIO ()
perfTestFile opts = do
    ctx <- ask
    let logFileName = "selfplay-" ++ show (startSecond ctx) ++ ".log"
    startLogger logFileName
    lift $ do
        putStrLn $ "Performance test from " ++ optAFenFile opts
        putStrLn $ "Search depth  " ++ show (optDepth opts)
    hi <- liftIO $ openFile (optAFenFile opts) ReadMode
    startMs <- liftIO $ currMilli (strttm ctx)
    acc <- loopCount (perfLoop hi (optDepth opts)) (PerfAcc 0 0 0)
    endMs <- liftIO $ currMilli (strttm ctx)
    let elapsedMs = max 1 (endMs - startMs)
        nps = perfNodes acc * 1000 `div` elapsedMs
    liftIO $ do
        putStrLn $ "Positions searched: " ++ show (perfValid acc)
        putStrLn $ "Positions skipped:  " ++ show (perfInvalid acc)
        putStrLn $ "Total nodes:        " ++ show (perfNodes acc)
        putStrLn $ "Total time (ms):    " ++ show elapsedMs
        putStrLn $ "Nodes/second:       " ++ show nps
        hClose hi

perfLoop :: Handle -> Int -> Int -> PerfAcc -> CtxIO (Bool, PerfAcc)
perfLoop hi depth _k acc = do
    end <- lift $ hIsEOF hi
    if end
       then return (False, acc)
       else do
           fen <- lift $ hGetLine hi
           if all isSpace fen
              then return (True, acc { perfInvalid = perfInvalid acc + 1 })
              else do
                  posRes <- liftIO $ try (evaluate (posFromFen fen)) :: CtxIO (Either SomeException MyPos)
                  case posRes of
                      Left _ -> return (True, acc { perfInvalid = perfInvalid acc + 1 })
                      Right pos -> do
                          chg <- readChanging
                          let crts = crtStatus chg
                              sini = posToState pos (hash crts) (hist crts) (evalst crts)
                          modifyChanging $ \c -> c { crtStatus = sini }
                          (_, _, nodes) <- iterativeDeepening depth Nothing
                          let acc' = PerfAcc {
                                  perfValid = perfValid acc + 1,
                                  perfInvalid = perfInvalid acc,
                                  perfNodes = perfNodes acc + nodes
                              }
                          return (True, acc')

matchFile :: Options -> String -> CtxIO GameScore
matchFile opts dir = do
    liftIO $ setCurrentDirectory dir
    ctx <- ask
    let logFileName = "selfplay-" ++ show (startSecond ctx) ++ ".log"
    startLogger logFileName
    msavePath <- liftIO $ discoverSprtSaveInDir "." opts
    msave <- liftIO $ traverse readSaveFile msavePath
    liftIO $ maybe (return ()) (validateResumeConflicts opts) msave
    let runOpts = maybe opts (saveToOptions opts) msave
    liftIO $ do
        putStrLn $ "Playing games from " ++ optAFenFile runOpts
        putStrLn $ "Play depth  " ++ show (optDepth runOpts)
        putStrLn $ "Play nodes  " ++ show (optNodes runOpts)
        putStrLn $ "Node margin " ++ show (optNodeMargin runOpts) ++ "%"
    case optPlayer1 runOpts of
        Nothing -> do
            liftIO $ putStrLn "For a match we need a challenger config as player 1"
            return (GameScore 0 0 0)
        Just id1 -> do
            ctxLog LogWarning $ "Players from directory " ++ dir
            ctxLog LogWarning $ "Player 1 " ++ id1
            fens <- getFens (optAFenFile runOpts) (fromMaybe 0 (optNSkip runOpts)) (fromMaybe 1 (optNFens runOpts))
            (id2, eval1, eval2) <- liftIO $ do
                (_, eval1) <- makeEvalState (Just id1) [] "progver" "progsuf"
                if optBaseCurrent runOpts
                   then return ("<compiled>", eval1, initEvalState [])
                   else case optPlayer2 runOpts of
                            Just cf -> do
                                (_, eval2) <- makeEvalState (Just cf) [] "progver" "progsuf"
                                return (cf, eval1, eval2)
                            Nothing -> error "matchFile: missing baseline selector"
            ctxLog LogWarning $ "Player 2 " ++ id2
            when debug $ do
                ctxLog LogInfo $ "Player 1 config: " ++ show eval1
                ctxLog LogInfo $ "Player 2 config: " ++ show eval2
            let msprt = if sprtEnabled runOpts then Just $ mkSprtState (getSprtConfig runOpts) else Nothing
            when (isJust msprt) $ liftIO $ do
                let scfg = getSprtConfig runOpts
                putStrLn $ "SPRT enabled: alpha=" ++ show (sprtAlpha scfg)
                    ++ " beta=" ++ show (sprtBeta scfg)
                    ++ " elo0=" ++ show (sprtElo0 scfg)
                    ++ " elo1=" ++ show (sprtElo1 scfg)
            when (isJust msave) $ liftIO $ putStrLn $ "Resuming SPRT from " ++ fromMaybe "" msavePath
            liftIO $ hFlush stdout
            let startLine = fromMaybe 0 (optNSkip runOpts) + 1
                initialAcc = maybe emptyMatchAcc saveToMatchAcc msave
                nextPairIndex = maybe 0 saveNextPairIndex msave
                remainingPairs = case msave of
                    Nothing -> length fens
                    Just save
                        | resumeTotalPairs opts save > saveOrigPairs save
                            -> max 0 (length fens - nextPairIndex)
                        | otherwise -> saveRemainingPairs save
                remainingFens = take remainingPairs $ drop nextPairIndex $ zip [startLine..] fens
                savePath = if isJust msprt then Just saveFileName else Nothing
            now <- liftIO getCurrentTime
            let matchInfo = MatchInfo {
                    matchMaxPairs = length fens,
                    matchStartTime = maybe now (\sv -> addUTCTime (negate $ realToFrac $ saveElapsedSeconds sv) now) msave,
                    matchOptions = runOpts,
                    matchSavePath = savePath
                }
            acc <- playMatchPairs (optDepth runOpts) (optNodes runOpts) (optNodeMargin runOpts) (optStatsEvery runOpts)
                                 matchInfo initialAcc (id1, eval1) (id2, eval2) msprt remainingFens
            liftIO $ printMatchSummary matchInfo msprt acc
            when (isJust msprt) $ liftIO $ finalizeSprtSave matchInfo acc
            return (matWdl acc)

readAllLinesStrict :: FilePath -> IO [String]
readAllLinesStrict fileName = withFile fileName ReadMode $ \hi -> go hi []
    where
      go h acc = do
          eof <- hIsEOF h
          if eof
             then return $ reverse acc
             else do
                 ln <- hGetLine h
                 ln `seq` go h (ln : acc)

-- Read a chunk of fens strictly into memory, then cycle deterministically.
getFens :: MonadIO m => String -> Int -> Int -> m [String]
getFens fileName skip count = liftIO $ do
    allFens <- readAllLinesStrict fileName
    if null allFens
       then ioError $ userError $ "Empty fen input file: " ++ fileName
       else do
           let total = length allFens
               start = if skip >= total then 0 else max 0 skip
               rotated = drop start allFens ++ take start allFens
           return $ take count $ cycle rotated
-- If the fen file has an index, use it to skip to the wanted line
-- The index must be created with binary IndexTxt from Bartest
skipWithIndex :: MonadIO m => String -> Handle -> Int -> m Bool
skipWithIndex fn h k = liftIO $ do
    let fni = replaceExtension fn "idx"
    fniex <- doesFileExist fni
    if fniex
       then do
           withBinaryFile fni ReadMode $ \ih ->
               allocaBytes 4 $ \ptr -> do
                   when debug $ putStrLn $ "Skip to index entry " ++ show k
                   hSeek ih AbsoluteSeek (fromIntegral k * 4)
                   rb <- hGetBuf ih ptr 4
                   if rb < 4
                      then error "Unexpected EOF in index file"
                      else do
                          wo <- peek ptr :: IO Word32
                          when debug $ putStrLn $ "Skip to file byte " ++ show wo
                          hSeek h AbsoluteSeek (fromIntegral wo)
                          return True
       else return False

loopCount :: Monad m => (Int -> a -> m (Bool, a)) -> a -> m a
loopCount act = go 1
    where go !k a = do
              (r, b) <- act k a
              if r then go (k+1) b else return b

skipLines :: MonadIO m => Handle -> Int -> Int -> () -> m (Bool, ())
skipLines hi m k () = do
    end <- if k <= m then liftIO $ hIsEOF hi else return True
    if end
       then return (False, ())
       else do
           _ <- liftIO $ hGetLine hi
           when (k `mod` 100000 == 0) $ liftIO $ do
               putStrLn $ "Positions skipped: " ++ show k
               hFlush stdout
           return (True, ())

-- We want positions which are not very imbalanced (after 1 ply search)
balancedPos :: Handle -> Handle -> Maybe Int -> Int -> () -> CtxIO (Bool, ())
balancedPos hi ho mn k () = do
    end <- case mn of
               Nothing -> lift $ hIsEOF hi
               Just n  -> if k <= n then lift $ hIsEOF hi else return True
    if end
       then return (False, ())
       else do
           fen <- lift $ hGetLine hi
           when debug $ lift $ do
               putStrLn $ "Fen: " ++ fen
               hFlush stdout
           when (k `mod` 100000 == 0) $ do
               ctx <- ask
               currms <- lift $ currMilli (strttm ctx)
               lift $ do
                   putStrLn $ "Positions completed: " ++ show k ++ " ("
                       ++ show (k `div` currms) ++ " positions per ms)"
                   hFlush stdout
           let pos = posFromFen fen
           chg <- readChanging
           let crts = crtStatus chg
               sini = posToState pos (hash crts) (hist crts) (evalst crts)
           modifyChanging $ \c -> c { crtStatus = sini }
           (sc, path, _) <- iterativeDeepening 1 Nothing
           when (not (null path) && abs sc <= 150) $ lift $ hPutStrLn ho fen
           return (True, ())

oracleAndFeats :: Int -> Handle -> Handle -> Maybe Int -> Int -> () -> CtxIO (Bool, ())
oracleAndFeats depth hi _ho mn k () = do	-- not functional yet
    end <- case mn of
               Nothing -> lift $ hIsEOF hi
               Just n  -> if k <= n then lift $ hIsEOF hi else return True
    if end
       then return (False, ())
       else do
           fen <- lift $ hGetLine hi
           when debug $ lift $ do
               putStrLn $ "Fen: " ++ fen
               hFlush stdout
           when (k `mod` 10 == 0) $ do
               ctx <- ask
               currms <- lift $ currMilli (strttm ctx)
               lift $ do
                   putStrLn $ "Positions completed: " ++ show k ++ " ("
                       ++ show (currms `div` k) ++ " ms per position)"
                   hFlush stdout
           let pos = posFromFen fen
           msc <- autoPlayToEnd depth pos
           when debug $ lift $ do
               putStrLn $ "Rez of auto play: " ++ show msc
               hFlush stdout
-- This part can't work now as we do not have featsEval
{-
           case msc of
               Just sc' -> do
                    let (ph, fts) = featsEval pos
                        sc | moving pos == White =  sc'	-- score autoPlayToEnd is from White p.o.v.
                           | otherwise           = -sc'
                    lift $ hPutStrLn ho $ show ph ++ " " ++ show sc ++ " " ++ show fts
               Nothing -> return ()
-}
           return (True, ())

data MatchTerm = MatchTermSprt SprtResult Double | MatchTermMaxPairs

data MatchInfo = MatchInfo {
        matchMaxPairs  :: !Int,
        matchStartTime :: !UTCTime,
        matchOptions   :: !Options,
        matchSavePath  :: !(Maybe FilePath)
    }

data MatchAcc = MatchAcc {
        matWdl       :: !GameScore,
        matPenta     :: !PentaScore,
        matPairsDone :: !Int,
        matPairsTried :: !Int,
        matLastLLR   :: !(Maybe Double),
        matLastSaveElapsed :: !(Maybe Double),
        matTerm      :: MatchTerm
    }

data PairResult = PairIncomplete | PairCompleted !GameScore !PentaScore

emptyMatchAcc :: MatchAcc
emptyMatchAcc = MatchAcc (GameScore 0 0 0) emptyPenta 0 0 Nothing Nothing MatchTermMaxPairs

saveToMatchAcc :: SprtSave -> MatchAcc
saveToMatchAcc save = MatchAcc {
        matWdl = saveWdl save,
        matPenta = savePenta save,
        matPairsDone = savePairsDone save,
        matPairsTried = savePairsTried save,
        matLastLLR = saveLastLLR save,
        matLastSaveElapsed = Just $ saveElapsedSeconds save,
        matTerm = MatchTermMaxPairs
    }

removeCanonicalSaveIfExists :: IO ()
removeCanonicalSaveIfExists = do
    ex <- doesFileExist saveFileName
    when ex $ removeFile saveFileName

finalizeSprtSave :: MatchInfo -> MatchAcc -> IO ()
finalizeSprtSave matchInfo acc = do
    now <- getCurrentTime
    case matchSavePath matchInfo of
        Nothing -> return ()
        Just path -> case finalSprtVerdict matchInfo acc of
            SprtContinue -> writeSaveFile path $ matchToSave matchInfo acc (realToFrac $ diffUTCTime now (matchStartTime matchInfo))
            _            -> removeCanonicalSaveIfExists

pairWdl :: String -> GameResult -> Maybe PairWdl
pairWdl player (GameWin plwin _) = Just $ if player == plwin then PairWin else PairLoss
pairWdl _ (GameRemis _)          = Just PairDraw
pairWdl _ _                      = Nothing

playOnePair
    :: Int
    -> Maybe Int
    -> Int
    -> (String, EvalState)
    -> (String, EvalState)
    -> String
    -> CtxIO PairResult
playOnePair depth maybeNodes nodeMarginPc (id1, eval1) (id2, eval2) fen = do
    let pos = posFromFen fen
    gr1 <- playGame depth maybeNodes nodeMarginPc pos (id1, eval1) (id2, eval2)
    gr2 <- playGame depth maybeNodes nodeMarginPc pos (id2, eval2) (id1, eval1)
    case (pairWdl id1 gr1, pairWdl id1 gr2) of
        (Just r1, Just r2) -> do
            let wdl1  = scoreGameResult id1 gr1
                wdl2  = scoreGameResult id1 gr2
                wdlp  = addGameScores wdl1 wdl2
                penta = classifyPair r1 r2
            return $ PairCompleted wdlp penta
        _ -> return PairIncomplete

playMatchPairs
    :: Int
    -> Maybe Int
    -> Int
    -> Int
    -> MatchInfo
    -> MatchAcc
    -> (String, EvalState)
    -> (String, EvalState)
    -> Maybe SprtState
    -> [(Int, String)]
    -> CtxIO MatchAcc
playMatchPairs depth maybeNodes nodeMarginPc printEvery matchInfo initAcc (id1, eval1) (id2, eval2) msprt fens =
    go initAcc fens
    where
      go acc [] = return acc { matTerm = MatchTermMaxPairs }
      go acc ((lineNo, fen):rest) = do
          pres <- playOnePair depth maybeNodes nodeMarginPc (id1, eval1) (id2, eval2) fen
          let accTried = acc { matPairsTried = matPairsTried acc + 1 }
          case pres of
              PairIncomplete -> do
                  accPrinted <- if shouldPrintMatchStatus printEvery accTried
                      then liftIO $ printMatchProgress matchInfo msprt lineNo fen accTried
                      else return accTried
                  go accPrinted rest
              PairCompleted wdlp pp -> do
                  let penta' = addPentaScore (matPenta accTried) pp
                      wdl' = addGameScores (matWdl accTried) wdlp
                      done' = matPairsDone accTried + 1
                      llrM = fmap (`sprtPentaLLR` penta') msprt
                      acc0 = accTried { matWdl = wdl', matPenta = penta', matPairsDone = done', matLastLLR = llrM }
                      acc1 = case (msprt, llrM) of
                          (Just ss, Just llr) -> case sprtResult ss llr of
                              SprtContinue -> acc0
                              res          -> acc0 { matTerm = MatchTermSprt res llr }
                          _ -> acc0
                  accPrinted <- if shouldPrintMatchStatus printEvery acc1
                      then liftIO $ printMatchProgress matchInfo msprt lineNo fen acc1
                      else return acc1
                  case matTerm accPrinted of
                      MatchTermMaxPairs -> go accPrinted rest
                      MatchTermSprt _ _ -> return accPrinted

printMatchSummary :: MatchInfo -> Maybe SprtState -> MatchAcc -> IO ()
printMatchSummary matchInfo msprt acc = do
    now <- getCurrentTime
    tz <- getCurrentTimeZone
    putStrLn "=============="
    mapM_ putStrLn $ matchStatusLines True matchInfo tz now msprt acc
    case matTerm acc of
        MatchTermMaxPairs -> putStrLn "Termination: max pairs reached"
        MatchTermSprt SprtH0 _ -> putStrLn "Termination: SPRT accepted H0"
        MatchTermSprt SprtH1 _ -> putStrLn "Termination: SPRT accepted H1"
        MatchTermSprt SprtContinue _ -> putStrLn "Termination: max pairs reached"

shouldPrintMatchStatus :: Int -> MatchAcc -> Bool
shouldPrintMatchStatus printEvery acc = matPairsTried acc > 0 && matPairsTried acc `mod` printEvery == 0

printMatchProgress :: MatchInfo -> Maybe SprtState -> Int -> String -> MatchAcc -> IO MatchAcc
printMatchProgress matchInfo msprt lineNo fen acc = do
    now <- getCurrentTime
    tz <- getCurrentTimeZone
    putStrLn $ show lineNo ++ ": " ++ fen
    mapM_ putStrLn $ matchStatusLines False matchInfo tz now msprt acc
    (accSaved, msavedAt) <- maybeSaveSprt matchInfo tz now acc
    maybe (return ()) (\savedAt -> putStrLn $ "SPRT status saved: " ++ savedAt) msavedAt
    hFlush stdout
    return accSaved

matchStatusLines :: Bool -> MatchInfo -> TimeZone -> UTCTime -> Maybe SprtState -> MatchAcc -> [String]
matchStatusLines isFinal matchInfo tz now msprt acc =
    [ "Games: " ++ show gamesDone ++ " / " ++ show gamesTotal
        ++ ", Games / sec: " ++ showGamesPerSec gamesDone elapsed
        ++ ", ETA: " ++ etaText matchInfo tz now acc elapsed
    , "WDL: " ++ showGameScore (matWdl acc)
    ] ++ sprtStatusLines isFinal msprt acc
    where
      gamesDone = playedGames acc
      gamesTotal = 2 * matchMaxPairs matchInfo
      elapsed = diffUTCTime now (matchStartTime matchInfo)

playedGames :: MatchAcc -> Int
playedGames acc = 2 * matPairsTried acc

showGameScore :: GameScore -> String
showGameScore (GameScore w d l) = "(" ++ show w ++ "," ++ show d ++ "," ++ show l ++ ")"

showPentaScore :: PentaScore -> String
showPentaScore (PentaScore ww wd wl dd ld ll) =
    "(" ++ show ww ++ "," ++ show wd ++ "," ++ show wl ++ "," ++ show dd ++ "," ++ show ld ++ "," ++ show ll ++ ")"

sprtStatusLines :: Bool -> Maybe SprtState -> MatchAcc -> [String]
sprtStatusLines _ Nothing _ = []
sprtStatusLines isFinal (Just ss) acc =
    [ "Completed pairs: " ++ show (matPairsDone acc) ++ " / " ++ show (matPairsTried acc)
    , "Penta (WW,WD,WL,DD,LD,LL): " ++ showPentaScore (matPenta acc)
    , "LLR: " ++ maybe "n/a" show (matLastLLR acc)
    , "Bounds: [" ++ show (sprtLowerBound ss) ++ ", " ++ show (sprtUpperBound ss) ++ "]"
    ] ++ sprtResultLine isFinal (matchSprtResult ss acc)

sprtResultLine :: Bool -> SprtResult -> [String]
sprtResultLine False SprtContinue = []
sprtResultLine _ res = ["SPRT: " ++ sprtStatusText res]

matchSprtResult :: SprtState -> MatchAcc -> SprtResult
matchSprtResult ss acc = maybe SprtContinue (sprtResult ss) (matLastLLR acc)

finalSprtVerdict :: MatchInfo -> MatchAcc -> SprtResult
finalSprtVerdict matchInfo acc = case matchSavePath matchInfo of
    Nothing -> SprtContinue
    Just _ -> case matLastLLR acc of
        Nothing -> SprtContinue
        Just llr -> sprtResult (mkSprtState $ getSprtConfig $ matchOptions matchInfo) llr

sprtStatusText :: SprtResult -> String
sprtStatusText SprtContinue = "continue"
sprtStatusText SprtH0 = "accepted H0"
sprtStatusText SprtH1 = "accepted H1"

showGamesPerSec :: Int -> NominalDiffTime -> String
showGamesPerSec games elapsed
    | elapsed <= 0 = "n/a"
    | otherwise = printf "%.1f" (fromIntegral games / realToFrac elapsed :: Double)

etaText :: MatchInfo -> TimeZone -> UTCTime -> MatchAcc -> NominalDiffTime -> String
etaText matchInfo tz now acc elapsed
    | gamesDone <= 0 = "n/a"
    | gamesDone >= gamesTotal = formatEta tz now
    | elapsed <= 0 = "n/a"
    | otherwise = formatEta tz $ addUTCTime remainingSeconds now
    where
      gamesDone = playedGames acc
      gamesTotal = 2 * matchMaxPairs matchInfo
      gamesPerSec = fromIntegral gamesDone / realToFrac elapsed :: Double
      remainingGames = gamesTotal - gamesDone
      remainingSeconds = realToFrac (fromIntegral remainingGames / gamesPerSec :: Double)

formatEta :: TimeZone -> UTCTime -> String
formatEta tz utc = formatTime defaultTimeLocale "%d.%m.%Y %H:%M" (utcToLocalTime tz utc)

maybeSaveSprt :: MatchInfo -> TimeZone -> UTCTime -> MatchAcc -> IO (MatchAcc, Maybe String)
maybeSaveSprt matchInfo tz now acc =
    case matchSavePath matchInfo of
        Nothing -> return (acc, Nothing)
        Just path -> do
            let elapsed = realToFrac (diffUTCTime now (matchStartTime matchInfo)) :: Double
                interval = fromIntegral (getSprtSaveMinutes $ matchOptions matchInfo) * 60.0
                due = maybe (elapsed >= interval) (\lastSave -> elapsed - lastSave >= interval) (matLastSaveElapsed acc)
            if not due
               then return (acc, Nothing)
               else do
                   writeSaveFile path $ matchToSave matchInfo acc elapsed
                   return (acc { matLastSaveElapsed = Just elapsed }, Just $ formatEta tz now)

matchToSave :: MatchInfo -> MatchAcc -> Double -> SprtSave
matchToSave matchInfo acc elapsed = SprtSave {
        savePlayer1 = fromMaybe (error "matchToSave: missing player1") $ optPlayer1 opts,
        savePlayer2 = optPlayer2 opts,
        saveBaseCurrent = optBaseCurrent opts,
        saveInputFile = optAFenFile opts,
        saveOrigSkip = fromMaybe 0 $ optNSkip opts,
        saveOrigPairs = matchMaxPairs matchInfo,
        saveNextPairIndex = matPairsTried acc,
        saveRemainingPairs = max 0 (matchMaxPairs matchInfo - matPairsTried acc),
        saveDepth = optDepth opts,
        saveNodes = optNodes opts,
        saveNodeMargin = optNodeMargin opts,
        saveStatsEvery = optStatsEvery opts,
        saveSprtCfg = getSprtConfig opts,
        saveSprtMinutes = getSprtSaveMinutes opts,
        savePairsDone = matPairsDone acc,
        savePairsTried = matPairsTried acc,
        saveWdl = matWdl acc,
        savePenta = matPenta acc,
        saveLastLLR = matLastLLR acc,
        saveVerdict = finalSprtVerdict matchInfo acc,
        saveElapsedSeconds = elapsed
    }
    where opts = matchOptions matchInfo

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

-- Here the logger has a problem: at the end it does not flush the logfile
-- This is important only for debug (for normal operation there will be no log)
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

startWriter :: Bool -> CtxIO ()
startWriter inter = do
    ctx <- ask
    void $ liftIO $ forkIO
         $ theWriter inter (writer ctx) (logger ctx) (LogInfo >= loglev ctx) (strttm ctx)

theWriter :: Bool -> Chan String -> Chan String -> Bool -> UTCTime -> IO ()
theWriter inter wchan lchan mustlog refs = forever $ do
    s <- readChan wchan
    when inter $ do
        putStrLn s
        hFlush stdout
    when mustlog $ logging lchan refs "Output" s

newThread :: CtxIO () -> CtxIO ThreadId
newThread a = do
    ctx <- ask
    liftIO $ forkIO $ runReaderT a ctx

-- We play the position to the end using fixed depth for now
-- which means, this function can be used only to optimize eval weights but not
-- time or search parameters
-- The result score is from White p.o.v.:
--  0: remis
-- +1: white wins
-- -1: black wins
autoPlayToEnd :: Int -> MyPos -> CtxIO (Maybe Int)
autoPlayToEnd d pos = do
    chg <- readChanging
    let crts = crtStatus chg
        sini = posToState pos (hash crts) (hist crts) (evalst crts)
    modifyChanging $ \c -> c { crtStatus = sini }
    go (0::Int)
    where go i = do
              -- Search to depth:
              (sc, path, _) <- iterativeDeepening d Nothing
              case path of
                 []  -> do
                     when (i>0) $ ctxLog LogError $ "Empty path when playing"
                     return Nothing	-- should not happen when i > 0
                 m:_ -> do
                     let j = i+1
                     ctxLog LogInfo $ "Real move " ++ show j ++ ": " ++ show m
                     chg   <- readChanging
                     sfin' <- execCState (doRealMove m) (crtStatus chg)
                     case stack sfin' of
                         []  -> do
                             ctxLog LogError $ "Empty path when playing"
                             return Nothing
                         p:_ -> if sc == 19999	-- mate in 1
                                   then if tacticalPos p
                                           then do
                                               let r = if moving p == White then -1 else 1
                                               ctxLog LogInfo $ "Mate (" ++ show r ++ ")"
                                               return $ Just r
                                           else do
                                               ctxLog LogError $ "Mate announced, not in check!"
                                               return Nothing
                                   else if remis50Moves p
                                           then do
                                               ctxLog LogInfo $ "Remis 50 moves"
                                               return $ Just 0
                                           else do
                                               hi <- liftIO newHist
                                               let sfin = sfin' { hist = hi }
                                               modifyChanging $ \s -> s { crtStatus = sfin }
                                               go j

-- Status kept for each "player" during a game
data Player = Player {
        plName  :: String,
        plChg   :: Changing,
        plNodes :: Int
    }

-- We may play with or without a search nodes budget, the we must be able to
-- make some operations with ints & maybe ints
aloNodes :: Maybe Int -> Int -> Maybe Int
aloNodes Nothing   _  = Nothing
aloNodes (Just n1) _  = Just n1

subNodes :: Maybe Int -> Int -> Int
subNodes Nothing   _  = 0
subNodes (Just _)  _  = 0

stopNodes :: Maybe Int -> Int -> Bool
stopNodes Nothing _    = False
stopNodes (Just n1) n2 = n2 >= n1

nodesForSearch :: Int -> Maybe Int -> Maybe Int
nodesForSearch _ Nothing = Nothing
nodesForSearch _ (Just n) | n <= 0 = Just n
nodesForSearch pc (Just n) = Just $ max 1 $ n - n * pc `div` 100

-- Play the given position to the end using node budget or fixed depth with 2 configurations
-- It can be used only to optimize eval weights but not time or search parameters
-- The result contains the winner (if any) and a reason for termination
playGame :: Int -> Maybe Int -> Int -> MyPos -> (String, EvalState) -> (String, EvalState) -> CtxIO GameResult
playGame d maybeNodes nodeMarginPc pos (ide1, eval1) (ide2, eval2) = do
    ctxLog LogWarning "--------------------------"
    ctxLog LogWarning $ "Setup new game between " ++ ide1 ++ " and " ++ ide2
    ctx <- ask
    chg <- readChanging
    (hash1, hash2, hist1, hist2) <- liftIO $ do
        (,,,) <$> newCache 1 <*> newCache 1 <*> newHist <*> newHist
    let state1 = posToState pos hash1 hist1 eval1
        state2 = posToState pos hash2 hist2 eval2
        color1 = moving pos
        color2 = other color1
        -- Maybe set realPly? (what is this good for if not for time management?)
        chg1 = chg { working = False, compThread = Nothing,
                     crtStatus = state1, forGui = Nothing, srchStrtMs = 0,
                     myColor = color1, totBmCh = 0, lastChDr = 0, lmvScore = Nothing }
        chg2 = chg { working = False, compThread = Nothing,
                     crtStatus = state2, forGui = Nothing, srchStrtMs = 0,
                     myColor = color2, totBmCh = 0, lastChDr = 0, lmvScore = Nothing }
        player1 = Player { plName = ide1, plChg = chg1, plNodes = 0 }
        player2 = Player { plName = ide2, plChg = chg2, plNodes = 0 }
    ctxLog LogWarning $ "Color for " ++ ide1 ++ ": " ++ show color1
    ctxLog LogWarning $ "Starting position: " ++ posToFen pos
    let cleanup = do
            runReaderT (modifyChanging $ const chg) ctx
            freeCache hash1
            freeCache hash2
    liftIO $ finally (runReaderT (go (0::Int) player1 player2) ctx) cleanup
    where go i player1 player2 = do
              start  <- asks strttm
              currms <- lift $ currMilli start
              let j = i + 1
              -- Prepare for chg1 to search:
              modifyChanging $ const (plChg player1) { forGui = Nothing, srchStrtMs = currms,
                                            totBmCh = 0, lastChDr = 0 }
              case (stack . crtStatus . plChg $ player1) of
                  []   -> return $ GameAborted "Empty stack in crtStatus"
                  p0:_ -> do
                      let mbNodes = aloNodes maybeNodes (plNodes player1)
                          searchNodes = nodesForSearch nodeMarginPc mbNodes
                          curfen = posToFen p0
                      ctxLog LogInfo $ "Real ply " ++ show j ++ " engine " ++ plName player1
                          ++ " (nodes budget: " ++ show mbNodes ++ ", search budget: " ++ show searchNodes ++ ")"
                      ctxLog LogInfo $ "Current fen: " ++ curfen
                      -- Search to depth or node budget:
                      (sc, path, nodes) <- iterativeDeepening d searchNodes
                      ctxLog LogInfo $ "Real ply " ++ show j ++ " returns " ++ show sc ++ " / " ++ show path
                          ++ " / " ++ show nodes
                      case path of
                        [] -> if sc == 0 && nodes == 0	-- can't make move: mated or stale mated
                                 then if tacticalPos p0
                                         then do
                                             ctxLog LogWarning $ "Mated (" ++ plName player2 ++ " wins)"
                                             ctxLog LogWarning $ "Fen: " ++ curfen
                                             ctxLog LogWarning $ "Rezult: ply " ++ show j ++ " sc " ++ show sc
                                                  ++ " path " ++ show path ++ " nodes " ++ show nodes
                                             return $ GameWin (plName player2) "Mate"
                                         else do
                                             ctxLog LogWarning $ "Remis: patt"
                                             ctxLog LogWarning $ "Fen: " ++ curfen
                                             ctxLog LogWarning $ "Rezult: ply " ++ show j ++ " sc " ++ show sc
                                                  ++ " path " ++ show path ++ " nodes " ++ show nodes
                                             return $ GameRemis "Remis (patt)"
                                 else do
                                     ctxLog LogError $ "Aborted: unexpected empty path when playing"
                                     ctxLog LogError $ "Fen: " ++ curfen
                                     ctxLog LogError $ "Rezult: ply " ++ show j ++ " sc " ++ show sc
                                          ++ " path " ++ show path ++ " nodes " ++ show nodes
                                     return $ GameAborted "Empty path when playing"
                        m:_ -> do
                             ctxLog LogInfo $ "Real move " ++ show j
                                  ++ " from " ++ plName player1 ++ ": " ++ show m
                             chg1f <- readChanging
                             s1fin <- execCState (doRealMove m) (crtStatus chg1f)
                             s2ini <- execCState (doRealMove m) (crtStatus (plChg player2))
                             case stack s1fin of
                               []  -> return $ GameAborted "Empty stack in s1fin"
                               p:_ -> do
                                 -- Using length path here could be a problem
                                 -- in case of a TT cut which is on path
                                 if | sc == mateScore && length path == 1	-- mate in 1
                                      -> if tacticalPos p
                                            then do
                                                ctxLog LogWarning $ "Mate (" ++ plName player1 ++ " wins)"
                                                ctxLog LogWarning $ "Fen: " ++ curfen
                                                ctxLog LogWarning $ "Rezult: ply " ++ show j ++ " sc " ++ show sc
                                                     ++ " path " ++ show path ++ " nodes " ++ show nodes
                                                return $ GameWin (plName player1) "Mate"
                                            else do
                                                ctxLog LogError $ "Aborted: mate announced, but not in check!"
                                                ctxLog LogError $ "Fen: " ++ curfen
                                                ctxLog LogError $ "Rezult: ply " ++ show j ++ " sc " ++ show sc
                                                     ++ " path " ++ show path ++ " nodes " ++ show nodes
                                                return $ GameAborted "Mate announced, but not in check"
                                    | remis50Moves p -> do
                                         ctxLog LogWarning $ "Remis: 50 moves"
                                         return $ GameRemis "Remis 50 moves rule"
                                    | remis3Repetitions p $ stack s1fin -> do
                                         ctxLog LogWarning $ "Remis: 3 repetitions"
                                         return $ GameRemis "Remis 3 repetitions rule"
                                    | noMatingMaterial p -> do
                                         ctxLog LogWarning $ "Remis: no mating material"
                                         ctxLog LogWarning $ "Fen: " ++ curfen
                                         ctxLog LogWarning $ "Rezult: ply " ++ show j ++ " sc " ++ show sc
                                              ++ " path " ++ show path ++ " nodes " ++ show nodes
                                         return $ GameRemis "Remis no mating material"
                                    -- Under 1200 cp we consider resignation
                                    | sc < -1200 -> do
                                         ctxLog LogWarning $ "Resign (" ++ plName player2 ++ " wins)"
                                         ctxLog LogWarning $ "Fen: " ++ curfen
                                         ctxLog LogWarning $ "Rezult: ply " ++ show j ++ " sc " ++ show sc
                                              ++ " path " ++ show path ++ " nodes " ++ show nodes
                                         return $ GameWin (plName player2) "Resignation"
                                    | otherwise -> do
                                         hi <- liftIO newHist
                                         let state2 = s2ini { hist = hi, mstats = ssts0 }
                                             chg1n  = chg1f { crtStatus = s1fin }
                                             chg2n  = (plChg player2) { crtStatus = state2 }
                                         go j (player2 { plChg = chg2n })
                                              (player1 { plChg = chg1n, plNodes = subNodes mbNodes nodes})

iterativeDeepening :: Int -> Maybe Int -> CtxIO (Int, [Move], Int)
iterativeDeepening depth maybeMaxNodes = do
    --when debug $ lift $ do
    --    putStrLn $ "In iter deep: " ++ show depth
    --    hFlush stdout
    chg <- readChanging
    go 1 (crtStatus chg) Nothing [] []
    where go d sini lsc lpv rmvs = do
              --when debug $ lift $ do
              --    putStrLn $ "In iter deep go: " ++ show d
              --    hFlush stdout
              (path, sc, rmvsf, _timint, sfin, _) <- bestMoveCont True maybeMaxNodes d 0 0 sini lsc lpv rmvs
              let nodes = fromIntegral $ sNodes $ mstats sfin
              -- We don't want to search less than depth 2, because depth 1 delivers
              -- erroneus moves by currently not updating the best score
              if d > 1 && (null path || d >= depth || stopNodes maybeMaxNodes nodes)
                 then return (sc, path, nodes)
                 else go (d+1) sfin (Just sc) path rmvsf

-- Append error info to error file:
collectError :: SomeException -> IO ()
collectError e = handle cannot $ do
    let efname = "Barbarossa_collected_errors.txt"
    tm <- getMyTime
    ef <- openFile efname AppendMode
    hPutStrLn ef $ formatMyTime tm ++ " selfplay: " ++ show e
    hClose ef
    where cannot :: IOException -> IO ()
          cannot _ = return ()

data GameResult = GameAborted String
                | GameWin String String
                | GameRemis String
                deriving Show

data GameScore = GameScore !Int !Int !Int

scoreGameResult :: String -> GameResult -> GameScore
scoreGameResult player (GameWin plwin _)
    | player == plwin = GameScore 1 0 0
    | otherwise       = GameScore 0 0 1
scoreGameResult _ (GameRemis _) = GameScore 0 1 0
scoreGameResult _ _             = GameScore 0 0 0

addGameScores :: GameScore -> GameScore -> GameScore
addGameScores (GameScore w1 d1 l1) (GameScore w2 d2 l2) = GameScore (w1+w2) (d1+d2) (l1+l2)

remis3Repetitions :: MyPos -> [MyPos] -> Bool
remis3Repetitions p ps
    | _:_:_:_ <- filter (== zobkey p)
        $ map zobkey $ takeWhile isReversible ps = True
    | otherwise                                  = False

noMatingMaterial :: MyPos -> Bool
noMatingMaterial p
    | occup p == kings p .|. knights p = True	-- one side should have only king
    | occup p == kings p .|. bishops p
        && popCount (occup p) == 3     = True
    | otherwise                        = False
