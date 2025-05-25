{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE MultiWayIf #-}

module Main where
import Control.Monad (when)
import Data.List (isSuffixOf)
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import Data.Time.Clock

-- import Data.Vector.Unboxed (toList)

import Struct.Status (EvalState(..))
-- import Struct.Struct (get50Moves)
import Struct.Struct
-- import Struct.Context
-- import Struct.Config
-- import Hash.TransTab
-- import Search.AlbetaTypes
-- import Moves.Base
import Moves.Fen
import Eval.FileParams (makeEvalState)
import Eval.Eval
import Tune.FenFiles

debug :: Bool
debug = False

data Options = Options {
        optCsvPath :: FilePath,	-- CSV file or directoy with files to vectorize (fen, score, rezult)
        optOutDir  :: FilePath,	-- output directory for training files
        optConFile :: Maybe FilePath,	-- model file to load
        optGener   :: Bool,    	-- generate NNUE features
        optTeste   :: Bool    	-- test NNUE
    }

defaultOptions :: Options
defaultOptions = Options {
        optCsvPath = "",
        optOutDir  = "",
        optConFile = Nothing,
        optGener   = False,
        optTeste   = False
    }

addIFile :: FilePath -> Options -> Options
addIFile fi opt = opt { optCsvPath = fi }

addOFile :: FilePath -> Options -> Options
addOFile fi opt = opt { optOutDir = fi }

addConf :: FilePath -> Options -> Options
addConf fi opt = opt { optConFile = Just fi }

addGener :: Options -> Options
addGener opt = opt { optGener = True }

addTeste :: Options -> Options
addTeste opt = opt { optTeste = True }

options :: [OptDescr (Options -> Options)]
options = [
        Option "i" ["input"]  (ReqArg addIFile "STRING") "Input file or directory",
        Option "o" ["output"] (ReqArg addOFile "STRING") "Output directory",
        Option "c" ["config"] (ReqArg addConf  "STRING") "Load model file",
        Option "g" ["gen"]    (NoArg addGener) "Generate NNUE features",
        Option "t" ["test"]   (NoArg addTeste) "Test NNUE function"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName ++ " -g -i PATH -o PATH | -t -i PATH [-c PATH]]"
          idName = "TuneSGD"

main :: IO ()
main = do
    (opts, _) <- theOptions
    if optTeste opts
        then testLoss (optCsvPath opts)
        else if optGener opts
                then filterFile (optCsvPath opts) (optOutDir opts)
                else putStrLn $ "No useful option, should be one of -t or -g"

-- Read a CSV file with fen, score, rezult and write in the output directory a filtered fen file,
-- The file name is called like the input file (without .csv extension) with suffix -filt.csv
filterFile :: FilePath -> FilePath -> IO ()
filterFile inFileName outFileDir = do
    fex <- doesFileExist inFileName
    if not fex
        then putStrLn $ "File " ++ inFileName ++ " does not exist"
        else do
            let outFileName = makeFileName inFileName outFileDir "-filt.csv"
            putStrLn $ inFileName ++ " --> " ++ outFileName
            hi <- openFile inFileName  ReadMode
            ho <- openFile outFileName WriteMode
            wr <- loopCount (filterQuietPos hi ho Nothing) 0
            putStrLn $ show wr ++ " records written"
            hClose hi
            hClose ho

-- Make a new file name from the old name (without directory), the new directory and a suffix
-- Ex: "/input_dir/input_file.csv" with dir "/output" and suffix "_feat.txt"
-- will rezult in "/output/input_file_feat.txt"
makeFileName :: FilePath -> FilePath -> String -> FilePath
makeFileName inName outDir suffix = joinPath [ outDir, fnn ]
    where fne     = takeFileName inName
          (fn, _) = splitExtension fne
          fnn     = fn ++ suffix

-- Filter out non quiet positions as well as the ones with too high quiet moves counter
-- (50 move rule) from the input file (as these will be too drawish)
-- The fen is the first part of the line, before the first ","
filterQuietPos :: Handle -> Handle -> Maybe Int -> Int -> Int -> IO (Bool, Int)
filterQuietPos hi ho mn k i = do
    end <- case mn of
               Nothing -> hIsEOF hi
               Just n  -> if k <= n then hIsEOF hi else return True
    if end
       then return (False, i)
       else do
           line <- hGetLine hi
           when (k `mod` 100000 == 0) $ do
               putStrLn $ "Positions completed: " ++ show k
               hFlush stdout
           when debug $ do
               putStrLn $ "Line: " ++ line
               hFlush stdout
           let (fen, _) = break ((==) ',') line
           when debug $ do
               putStrLn $ "Fen: " ++ fen
               hFlush stdout
           let pos = posFromFen fen
           if (get50Moves pos <= 90 && prettyQuiet pos)
               then do
                   hPutStrLn ho line
                   return (True, i+1)
               else return (True, i)

-- Fake functions until we have the correct ones
prettyQuiet :: MyPos -> Bool
prettyQuiet _ = True

get50Moves :: MyPos -> Int
get50Moves _ = 10

-- Read a file of FENs and accumulate the loss
-- This function is called via loopCount, with status: remaining files, number of fens
-- and accumulated loss
evalFile :: Loss -> EvalState -> Int -> ([FilePath], Int, Double) -> IO (Bool, ([FilePath], Int, Double))
evalFile loss es k (files, !r, !acc)
    | null files = return (False, ([], r, acc))
    | otherwise  = do
        let inFileName = head files
            rest       = tail files
        fex <- doesFileExist inFileName
        if not fex
            then do
                putStrLn $ "File " ++ show k ++ ": " ++ inFileName ++ " does not exist"
                return (True, (rest, r, acc))
            else do
                putStrLn $ "Eval " ++ show k ++ ": " ++ inFileName
                hi <- openFile inFileName ReadMode
                (r', acc') <- loopCount (evalPos loss es hi) (0, 0)
                putStrLn $ "Records: " ++ show r' ++ ", loss = " ++ show acc'
                hClose hi
                return (True, (rest, r+r', acc+acc'))

-- Evalutes one fen (line) of the file, calculates the loss and accumulates it
-- Status is number of total records and accumulated loss
evalPos :: Loss -> EvalState -> Handle -> Int -> (Int, Double) -> IO (Bool, (Int, Double))
evalPos loss es hi _ (!r, !acc) = do
    end <- hIsEOF hi
    if end
       then return (False, (r, acc))
       else do
           line <- hGetLine hi
           when (r `mod` 100000 == 0) $ do
               putStrLn $ "Positions completed: " ++ show r
               hFlush stdout
           when debug $ do
               putStrLn $ "Line: " ++ line
               hFlush stdout
           -- The lines we expect contain: fen ',' score ',' rez
           let (fen, rest)  = break ((==) ',') line
               (sco, rest1) = break ((==) ',') $ tail rest
               -- Target score in centipawns
               tgsco = read sco
               -- Rezult is:
               rez   = read (tail rest1)
               pos   = posFromFen fen
               score = posEval pos es
               thisLoss = loss tgsco rez (fromIntegral score)
           when debug $ do
               putStrLn $ "Fen: " ++ fen
               putStrLn $ "- tgsco: " ++ show tgsco
               putStrLn $ "- rezul: " ++ show rez
               putStrLn $ "- score: " ++ show score
               putStrLn $ "- loss:  " ++ show thisLoss
               hFlush stdout
           return (True, (r + 1, acc + thisLoss))

type Pred = FilePath -> Bool

maybeFilterFilePaths :: Maybe Pred -> [FilePath] -> [FilePath]
maybeFilterFilePaths maybePred
    | Just filt <- maybePred = filter filt
    | otherwise              = id

getFileList :: FilePath -> Maybe Pred -> IO [FilePath]
getFileList filePath maybePred = do
    isDir <- doesDirectoryExist filePath
    fileList <- if isDir
                   then listDirectory filePath
                   else do
                       isFile <- doesFileExist filePath
                       if isFile then return [takeFileName filePath] else return []
    let fl = maybeFilterFilePaths maybePred $ map ((filePath </>)) fileList
    putStrLn $ "All files: " ++ show fl
    return fl

-- Loss function type: depends on target score, target result and actual score
type Loss = Double -> Double -> Double -> Double

lossPerScore :: Loss
lossPerScore tgsc _ sc = x * x
    where wdl_target = scoreSigmoidScale * sigmoid tgsc
          wdl_model  = scoreSigmoidScale * sigmoid sc
          x = wdl_target - wdl_model
          scoreSigmoidScale = 600

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

filteredCsv :: Pred
filteredCsv f = isCsv && isFilt
    where isCsv = takeExtension f == ".csv"
          isFilt = "-filt" `isSuffixOf` (fst $ splitExtension f)

-- Evaluate a loss function with the given eval state for all CSV file in the given list
evaluateLoss :: Loss -> EvalState -> [FilePath] -> IO Double
evaluateLoss loss es files = do
    putStrLn "Begin full eval"
    (_, r, acc) <- loopCount (evalFile loss es) (files, 0, 0)
    let meanLoss = acc / fromIntegral r
    putStrLn $ "Full eval: " ++ show acc ++ " / " ++ show r ++ " = " ++ show meanLoss
    hFlush stdout
    return meanLoss

testLoss :: FilePath -> IO ()
testLoss inPath = do
    files   <- getFileList inPath (Just filteredCsv)
    (_, es) <- makeEvalState Nothing [] "" ""
    stime   <- getCurrentTime
    ml      <- evaluateLoss lossPerScore es files
    putStrLn $ "Rezult: " ++ show ml
    etime   <- getCurrentTime
    putStrLn $ "Time: " ++ show (diffUTCTime etime stime)
    return ()
