-- Chess Engine Log Processor
-- Extracts training data from chess engine log files
-- Author: Generated for chess training data preparation

module Main (main) where

import LogScan.Core (processLogContent)
import System.Console.GetOpt
import System.Environment (getArgs)

data Options = Options {
        optInPath     :: FilePath,  -- input file
        optOutPath    :: FilePath,  -- output file (CSV)
        optTrackCount :: Int,       -- number of scores to track for game result
        optLowLimit   :: Int,       -- lower margin for unclear games
        optHighLimit  :: Int,       -- higher margin for unclear games
        optMinDepth   :: Int        -- minimum Origin depth to emit
    }

defaultOptions :: Options
defaultOptions = Options {
        optInPath     = "",
        optOutPath    = "",
        optTrackCount = 5,
        optLowLimit   = 100,
        optHighLimit  = 450,
        optMinDepth   = 6
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

options :: [OptDescr (Options -> Options)]
options = [
        Option "i" ["input"]  (ReqArg addInPath   "STRING") "Input file",
        Option "o" ["output"] (ReqArg addOutPath  "STRING") "Output file",
        Option "t" ["track"]  (ReqArg setTrackCount  "INT") "Track count for game result",
        Option "H" ["high"]   (ReqArg setHighLimit   "INT") "High limit for unclear games",
        Option "L" ["low"]    (ReqArg setLowLimit    "INT") "Low limit for unclear games",
        Option "d" ["min-depth"] (ReqArg setMinDepth "INT") "Minimum Origin depth to emit"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: LogScan -i PATH -o PATH -t TRACK -L LOW -H HIGH -d MIN_DEPTH"

-- MAIN PROCESSING FUNCTION --

-- | Main function to process a log file and generate training data
-- Arguments:
--   inputFile: path to the input log file
--   outputFile: path to the output CSV file
--   trackCount: how many last positions to examine (typically 4-5)
--   lowLimit: max score magnitude for draw (typically 100)
--   highLimit: min score magnitude for decided game (typically 400)
--   minDepth: minimum Origin depth required for output rows
--
-- The function reads the input file, processes each line, groups positions
-- into games, determines game results, and writes CSV output
processLogFile :: FilePath -> FilePath -> Int -> Int -> Int -> Int -> IO ()
processLogFile inputFile outputFile trackCount lowLimit highLimit minDepth = do
    content <- readFile inputFile
    writeFile outputFile $
        processLogContent minDepth trackCount lowLimit highLimit content

main :: IO ()
main = do
    (opts, _) <- theOptions
    processLogFile (optInPath opts) (optOutPath opts)
                   (optTrackCount opts) (optLowLimit opts) (optHighLimit opts)
                   (optMinDepth opts)

{-
This will:
- Read from "input.log"
- Track the last 5 positions for game result determination
- Consider scores in [-100, 100] as draws
- Consider scores outside [-400, 400] as decided games
- Write training data to "output.csv"
-}
