{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Main where
import Control.Monad.Reader
import Control.Monad (when)
-- import Control.Concurrent
-- import Control.Exception
-- import Data.Foldable (foldlM)
import Data.List (intersperse)
-- import Data.Maybe (fromMaybe)
import Foreign hiding (void)
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
-- import System.Time

import Data.Vector.Unboxed (toList)

-- import Struct.Struct
-- import Struct.Status
-- import Struct.Context
-- import Struct.Config
-- import Hash.TransTab
-- import Search.AlbetaTypes
-- import Moves.Base
import Moves.Fen
-- import Moves.Notation
-- import Moves.History
-- import Search.CStateMonad (execCState)
-- import Eval.FileParams (makeEvalState)
import Eval.Eval
-- import Uci.UciGlue

debug :: Bool
debug = False

data Options = Options {
        -- optNThreads :: Int,	-- number of threads - not used for self play now
        optCsvPath :: FilePath,	-- CSV file or directoy with files to vectorize (fen, score, rezult)
        optOutDir  :: FilePath	-- output directory for training files
    }

defaultOptions :: Options
defaultOptions = Options {
        optCsvPath = "",
        optOutDir = ""
    }

addIFile :: FilePath -> Options -> Options
addIFile fi opt = opt { optCsvPath = fi }

addOFile :: FilePath -> Options -> Options
addOFile fi opt = opt { optOutDir = fi }

options :: [OptDescr (Options -> Options)]
options = [
        Option "i" ["input"]  (ReqArg addIFile "STRING") "Input file or directory",
        Option "o" ["output"] (ReqArg addOFile "STRING") "Output directory"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName ++ " -i PATH -o PATH"
          idName = "FensToTrain"

main :: IO ()
main = do
    (opts, _) <- theOptions
    filterFile (optCsvPath opts) (optOutDir opts)

-- Read a CSV file with fen, score, rezult and write in the output directory 2 files,
-- one with the features and one with the training target:
-- Features line: features for one fen as a list of integers separated by comma
-- Target line: score, rezult for one fen (2 integers, commy separated)
-- - score is in centipawns from the point of view of the moving part
-- - rezult is 0, 1 or 2 for lost, draw or win from point of view of the moving part
-- The feature file name is called like the input file (without .csv extension) with prefix -feat.txt
-- The target file name is called like the input file (without .csv extension) with prefix -targ.txt
filterFile :: FilePath -> FilePath -> IO ()
filterFile inFileName outFileDir = do
    fex <- doesFileExist inFileName
    if not fex
        then putStrLn $ "File " ++ inFileName ++ " does not exist"
        else do
            let outFeat = makeFileName inFileName outFileDir "-feat.txt"
                outTarg = makeFileName inFileName outFileDir "-targ.txt"
            putStrLn $ inFileName ++ " --> " ++ outFeat ++ " & " ++ outTarg
            hi  <- openFile inFileName ReadMode
            hof <- openFile outFeat WriteMode
            hot <- openFile outTarg WriteMode
            loopCount (featurePos hi hof hot Nothing) ()
            hClose hof
            hClose hot
            hClose hi

-- Make a new file name from the old name (without directory), the new directory and a suffix
-- Ex: "/input_dir/input_file.csv" with dir "/output" and suffix "_feat.txt"
-- will rezult in "/output/input_file_feat.txt"
makeFileName :: FilePath -> FilePath -> String -> FilePath
makeFileName inName outDir suffix = joinPath [ outDir, fnn ]
    where fne     = takeFileName inName
          (fn, _) = splitExtension fne
          fnn     = fn ++ suffix

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

-- Eval position and write features & target
featurePos :: Handle -> Handle -> Handle -> Maybe Int -> Int -> () -> IO (Bool, ())
featurePos hi hof hot mn k () = do
    end <- case mn of
               Nothing -> hIsEOF hi
               Just n  -> if k <= n then hIsEOF hi else return True
    if end
       then return (False, ())
       else do
           line <- hGetLine hi
           when (k `mod` 100000 == 0) $ do
               putStrLn $ "Positions completed: " ++ show k
               hFlush stdout
           when debug $ do
               putStrLn $ "Line: " ++ line
               hFlush stdout
           let (fen, rest) = break ((==) ',') line
           when debug $ do
               putStrLn $ "Fen: " ++ fen
               hFlush stdout
           let pos = posFromFen fen
               targ = tail rest
               idxs = posToIndexes pos
           hPutStrLn hof $ concat $ intersperse "," $ map show idxs
           hPutStrLn hot targ
           return (True, ())
