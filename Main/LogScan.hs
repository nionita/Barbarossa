module Main (main) where

import System.Console.GetOpt
import System.Environment (getArgs)
import Tune.LogScan
    ( FindKind(..)
    , LogFormat(..)
    , processLogContentWithFormat
    , processReplayLogContent
    )

data Options = Options
    { optInPath      :: FilePath
    , optOutPath     :: FilePath
    , optTrackCount  :: Int
    , optLowLimit    :: Int
    , optHighLimit   :: Int
    , optMinDepth    :: Int
    , optLogFormat   :: LogFormat
    , optFindFen     :: Maybe String
    , optFindKind    :: FindKind
    , optPgnOutPath  :: Maybe FilePath
    , optMatchDepth  :: Int
    }

defaultOptions :: Options
defaultOptions = Options
    { optInPath = ""
    , optOutPath = ""
    , optTrackCount = 5
    , optLowLimit = 100
    , optHighLimit = 450
    , optMinDepth = 6
    , optLogFormat = NewLogFormat
    , optFindFen = Nothing
    , optFindKind = MatchBoth
    , optPgnOutPath = Nothing
    , optMatchDepth = 6
    }

addInPath :: FilePath -> Options -> Options
addInPath fi opt = opt { optInPath = fi }

addOutPath :: FilePath -> Options -> Options
addOutPath fi opt = opt { optOutPath = fi }

setTrackCount :: String -> Options -> Options
setTrackCount ba opt = opt { optTrackCount = read ba }

setLowLimit :: String -> Options -> Options
setLowLimit ba opt = opt { optLowLimit = read ba }

setHighLimit :: String -> Options -> Options
setHighLimit ba opt = opt { optHighLimit = read ba }

setMinDepth :: String -> Options -> Options
setMinDepth ba opt = opt { optMinDepth = read ba }

setLogFormat :: String -> Options -> Options
setLogFormat formatName opt =
    opt { optLogFormat = parseLogFormat formatName }
  where
    parseLogFormat "new" = NewLogFormat
    parseLogFormat "old" = OldLogFormat
    parseLogFormat otherFormat = error $ "Invalid --log-format value: " ++ otherFormat

setFindFen :: String -> Options -> Options
setFindFen fen opt = opt { optFindFen = Just fen }

setPgnOutPath :: FilePath -> Options -> Options
setPgnOutPath fi opt = opt { optPgnOutPath = Just fi }

setMatchDepth :: String -> Options -> Options
setMatchDepth ba opt = opt { optMatchDepth = read ba }

setFindKind :: String -> Options -> Options
setFindKind kind opt =
    opt { optFindKind = parseFindKind kind }
  where
    parseFindKind "orig" = MatchOrig
    parseFindKind "eval" = MatchEval
    parseFindKind "both" = MatchBoth
    parseFindKind otherKind = error $ "Invalid --find-kind value: " ++ otherKind

options :: [OptDescr (Options -> Options)]
options =
    [ Option "i" ["input"] (ReqArg addInPath "STRING") "Input log file"
    , Option "o" ["output"] (ReqArg addOutPath "STRING") "CSV output file"
    , Option "t" ["track"] (ReqArg setTrackCount "INT") "Track count for game result"
    , Option "H" ["high"] (ReqArg setHighLimit "INT") "High limit for unclear games"
    , Option "L" ["low"] (ReqArg setLowLimit "INT") "Low limit for unclear games"
    , Option "d" ["min-depth"] (ReqArg setMinDepth "INT") "Minimum Origin depth to emit"
    , Option "" ["log-format"] (ReqArg setLogFormat "new|old") "Log format for CSV extraction (default: new)"
    , Option "" ["find-fen"] (ReqArg setFindFen "FEN") "Target FEN for replay mode"
    , Option "" ["find-kind"] (ReqArg setFindKind "orig|eval|both") "Match target against original or evaluated Origin FEN"
    , Option "" ["pgn-out"] (ReqArg setPgnOutPath "PATH") "PGN output file for replay mode"
    , Option "" ["match-depth"] (ReqArg setMatchDepth "INT") "Minimum Origin depth used when locating the target FEN"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> pure (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo usageHeader options))

usageHeader :: String
usageHeader =
    "Usage: LogScan -i PATH (-o PATH [-t TRACK -L LOW -H HIGH -d MIN_DEPTH --log-format new|old] | "
        ++ "--find-fen FEN --pgn-out PATH [--find-kind orig|eval|both] [--match-depth INT])"

processLogFile :: FilePath -> FilePath -> Int -> Int -> Int -> Int -> LogFormat -> IO ()
processLogFile inputFile outputFile trackCount lowLimit highLimit minDepth logFormat = do
    content <- readFile inputFile
    writeFile outputFile $
        processLogContentWithFormat logFormat minDepth trackCount lowLimit highLimit content

processReplayFile :: FilePath -> FilePath -> FindKind -> Int -> String -> IO ()
processReplayFile inputFile outputFile findKind matchDepth targetFen = do
    content <- readFile inputFile
    case processReplayLogContent findKind matchDepth targetFen content of
        Left err -> ioError (userError err)
        Right pgn -> writeFile outputFile pgn

requireValue :: String -> String -> IO String
requireValue description value
    | null value = ioError (userError ("Missing " ++ description ++ "\n" ++ usageInfo usageHeader options))
    | otherwise = pure value

requireMaybeValue :: String -> Maybe String -> IO String
requireMaybeValue description maybeValue =
    case maybeValue of
        Just value -> pure value
        Nothing -> ioError (userError ("Missing " ++ description ++ "\n" ++ usageInfo usageHeader options))

main :: IO ()
main = do
    (opts, _) <- theOptions
    inputPath <- requireValue "input path (-i/--input)" (optInPath opts)
    case optFindFen opts of
        Nothing -> do
            outputPath <- requireValue "CSV output path (-o/--output)" (optOutPath opts)
            processLogFile inputPath outputPath
                (optTrackCount opts) (optLowLimit opts) (optHighLimit opts) (optMinDepth opts) (optLogFormat opts)
        Just targetFen -> do
            pgnOutPath <- requireMaybeValue "PGN output path (--pgn-out)" (optPgnOutPath opts)
            processReplayFile inputPath pgnOutPath (optFindKind opts) (optMatchDepth opts) targetFen
