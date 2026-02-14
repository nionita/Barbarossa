{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

module Main where
import Control.Monad (when, forM_)
import Data.List (isSuffixOf, sort)
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.Random
import Data.Time.Clock
import Debug.Trace (traceShow)

import qualified Data.Vector.Unboxed as U

import Struct.Status (EvalState(..))
import Struct.Struct
import Struct.ParamsPost (optSpaceNames, optSpaceInit)
import Moves.Fen
import Eval.Eval
import Tune.FenFiles
import Tune.EvalAccum
import Tune.Utils

debug :: Bool
debug = False

data Options = Options {
        optCsvPath   :: FilePath,	-- CSV file or directory with files to vectorize (fen, score, result)
        optOutPath   :: FilePath,	-- output directory for training files
        optConFile   :: Maybe FilePath,	-- model file to load
        optShuffle   :: Bool,    	-- shuffle training files
        optGener     :: Bool,    	-- generate NNUE features
        optTest      :: Bool,    	-- calculate test loss only
        optDebug     :: Bool,    	-- debug
        optType      :: Int,    	-- input file type: 0, 1 or 2 for S, R or SR (fen is always there, first)
        optTrain     :: Int,    	-- number of train batches - if > 0
        optOptim     :: Int,    	-- random search until so many improment fails
        optRosen     :: Int,    	-- minimize Rosenbrock for internal test - so many steps
        optTolerate  :: Int,	-- tolerate this percentage of bad scores in every batch
        optBatchSz   :: Int,	-- batch size
        optValidBa   :: Int,	-- validate every so many batches
        optLossFun   :: Int,	-- loss function code
        optFixIdx    :: Int,	-- this weight will not be modified
        optX         :: Double,    	-- start x for Rosenbrock
        optY         :: Double,    	-- start x for Rosenbrock
        optLR        :: Double,	-- learning rate
        optMaxChange :: Double,	-- max change per parameter
        optLossLuft  :: Double,	-- loss luft (domain of 0 loss) in centipawns
        optSigScale  :: Double,	-- sigmoid scale
        optWeiScale  :: Double,	-- sigmoid scale
        optScoreWei  :: Double	-- score weight for score/result loss
    }

defaultOptions :: Options
defaultOptions = Options {
        optCsvPath   = "",
        optOutPath   = "",
        optConFile   = Nothing,
        optShuffle   = False,
        optGener     = False,
        optTest      = False,
        optDebug     = False,
        optType      = 2,
        optTrain     = 0,
        optOptim     = 0,
        optRosen     = 0,
        optTolerate  = 0,
        optBatchSz   = 3072,
        optValidBa   = 0,
        optLossFun   = 1,	-- default lossScoreSigWeight
        optFixIdx    = 0,	-- default: material middle game not modified
        optX         = 0,
        optY         = 0,
        optLR        = 0.1,
        optMaxChange = 0.01,
        optLossLuft  = 25,
        optSigScale  = 1 / 595,	-- best factor on ccrl3200 train set (all)
        optWeiScale  = 1 / 200,
        optScoreWei  = 0.5
    }

addIFile :: FilePath -> Options -> Options
addIFile fi opt = opt { optCsvPath = fi }

addOFile :: FilePath -> Options -> Options
addOFile fi opt = opt { optOutPath = fi }

addConf :: FilePath -> Options -> Options
addConf fi opt = opt { optConFile = Just fi }

addGener :: Options -> Options
addGener opt = opt { optGener = True }

setShuffle :: Options -> Options
setShuffle opt = opt { optShuffle = True }

setScore :: Options -> Options
setScore opt = opt { optType = 0 }

setResult :: Options -> Options
setResult opt = opt { optType = 1 }

addTrain :: String -> Options -> Options
addTrain ba opt = opt { optTrain = read ba }

addOptim :: String -> Options -> Options
addOptim ba opt = opt { optOptim = read ba }

addTest :: Options -> Options
addTest opt = opt { optTest = True }

addDebug :: Options -> Options
addDebug opt = opt { optDebug = True }

addRosen :: String -> Options -> Options
addRosen ba opt = opt { optRosen = read ba }

addX :: String -> Options -> Options
addX ba opt = opt { optX = read ba }

addY :: String -> Options -> Options
addY ba opt = opt { optY = read ba }

addBatchSz :: String -> Options -> Options
addBatchSz ba opt = opt { optBatchSz = read ba }

addLR :: String -> Options -> Options
addLR lr opt = opt { optLR = read lr }

addValidBa :: String -> Options -> Options
addValidBa ba opt = opt { optValidBa = read ba }

addLossFun :: String -> Options -> Options
addLossFun ba opt = opt { optLossFun = read ba }

setFixIdx :: String -> Options -> Options
setFixIdx ba opt = opt { optFixIdx = read ba }

addMaxChg :: String -> Options -> Options
addMaxChg ba opt = opt { optMaxChange = read ba }

setLossLuft :: String -> Options -> Options
setLossLuft ba opt = opt { optLossLuft = read ba }

setSigScale :: String -> Options -> Options
setSigScale ba opt = opt { optSigScale = 1 / read ba }

setWeiScale :: String -> Options -> Options
setWeiScale ba opt = opt { optWeiScale = 1 / read ba }

setScoreWei :: String -> Options -> Options
setScoreWei ba opt = opt { optScoreWei = read ba }

setTolerate :: String -> Options -> Options
setTolerate ba opt = opt { optTolerate = read ba }

options :: [OptDescr (Options -> Options)]
options = [
        Option "a" ["tolerate"] (ReqArg setTolerate "FLOAT") "Tolerate (ignore) this percent of bad scores",
        Option "b" ["bsize"]    (ReqArg addBatchSz "INT")    "Batch size (10240)",
        Option "c" ["config"]   (ReqArg addConf    "STRING") "Load model file",
        Option "d" ["debug"]    (NoArg  addDebug)            "Debug functionality",
        Option "e" ["test"]     (NoArg  addTest)             "Test evaluation",
        Option "f" ["luft"]     (ReqArg setLossLuft "FLOAT") "Loss luft in centipawns (25)",
        Option "g" ["gen"]      (NoArg  addGener)            "Generate NNUE features",
        Option "i" ["input"]    (ReqArg addIFile   "STRING") "Input file or directory",
        Option "j" ["wscale"]   (ReqArg setWeiScale "FLOAT") "Weight scale in centipawns (200)",
        Option "k" ["scale"]    (ReqArg setSigScale "FLOAT") "Sigmoid scale in centipawns (595)",
        Option "l" ["lr"]       (ReqArg addLR      "FLOAT")  "Learning rate (0.1)",
        Option "m" ["max"]      (ReqArg addMaxChg  "FLOAT")  "Max param change (0.01)",
        Option "o" ["output"]   (ReqArg addOFile   "STRING") "Output directory",
        Option "q" ["frasc"]    (ReqArg setScoreWei "FLOAT") "Score weight for score/result loss (0.5)",
        Option "r" ["rosen"]    (ReqArg addRosen "INT")      "Minimize Rosenbrock so many steps",
        Option "s" ["shuffle"]  (NoArg  setShuffle)          "Shuffle training files",
        Option "t" ["train"]    (ReqArg addTrain   "INT")    "Train so many batches",
        Option "v" ["validate"] (ReqArg addValidBa "INT")    "Validate every so many batches (1000)",
        Option "x" ["xi"]       (ReqArg addX "INT")          "Start X for Rosenbrock",
        Option "y" ["yi"]       (ReqArg addY "INT")          "Start Y for Rosenbrock",
        Option "L" ["loss"]     (ReqArg addLossFun "INT")    "Loss function: 0 - 3 (0)",
        Option "F" ["fix"]      (ReqArg setFixIdx "INT")     "Fix this index 0 - dimensions (0)",
        Option "R" ["result"]   (NoArg  setResult)           "Input file format: fen,result",
        Option "S" ["score"]    (NoArg  setScore)            "Input file format: fen,score",
        Option "O" ["optim"]    (ReqArg addOptim   "INT")    "Optimize by random search"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName ++ " -g -i PATH -o PATH | -t -i PATH -b BATCHES"
          idName = "TuneSGD"

main :: IO ()
main = do
    (opts, _) <- theOptions
    if optTrain opts > 0 || optOptim opts > 0
        then train opts
        else if optGener opts
                then filterFile (optCsvPath opts) (optOutPath opts)
                else if optTest opts
                        then testLoss opts
                        -- then checkStep opts
                        else if optRosen opts > 0
                                then minRosenbrock (optX opts) (optY opts) (optLR opts) (optRosen opts)
                                else putStrLn $ "No useful option, should be one of -t, -e or -g"

-- Read a CSV file with fen, score, result and write in the output directory a filtered fen file,
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
            -- wr <- loopCount (filterQuietPos hi ho Nothing) 0
            wr <- loopCount (filterMidGamePos hi ho Nothing) 0
            putStrLn $ show wr ++ " records written"
            hClose hi
            hClose ho

-- Make a new file name from the old name (without directory), the new directory and a suffix
-- Ex: "/input_dir/input_file.csv" with dir "/output" and suffix "_feat.txt"
-- will result in "/output/input_file_feat.txt"
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

-- Filter out positions with too small game phase from the input file
-- The fen is the first part of the line, before the first ","
filterMidGamePos :: Handle -> Handle -> Maybe Int -> Int -> Int -> IO (Bool, Int)
filterMidGamePos hi ho mn k i = do
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
           if gamePhase pos >= 120
               then do
                   hPutStrLn ho line
                   return (True, i+1)
               else return (True, i)

-- Read a file of FENs and accumulate the loss
-- This function is called via loopCount, with status: remaining files, number of fens
-- and accumulated loss
evalFile :: EvalAccum a => Loss -> Sampler -> [EvalState] -> Int -> ([FilePath], a)
         -> IO (Bool, ([FilePath], a))
evalFile _    _       _   _ ([], a)               = return (False, ([], a))
evalFile loss sampler ess k (inFileName:rest, !a) = do
    fex <- doesFileExist inFileName
    if not fex
        then do
            putStrLn $ "File " ++ show k ++ ": " ++ inFileName ++ " does not exist"
            return (True, (rest, a))
        else do
            putStrLn $ "Eval " ++ show k ++ ": " ++ inFileName
            hFlush stdout
            hi <- openFile inFileName ReadMode
            a' <- loopCount (evalPos loss sampler ess hi) a
            -- putStrLn $ "Records: " ++ show r' ++ ", loss = " ++ show acc'
            hFlush stdout
            hClose hi
            return (True, (rest, a'))
            -- For test: return after the first file
            -- return (False, (rest, a'))

-- Evaluates one fen (line) of the file, calculates the loss and accumulates it
-- Status is number of total records and accumulated loss
evalPos :: EvalAccum a => Loss -> Sampler -> [EvalState] -> Handle -> Int -> a -> IO (Bool, a)
evalPos loss sampler ess hi _ a = do
    end <- hIsEOF hi
    if end
       then return (False, a)
       else do
           line <- hGetLine hi
           -- when (r `mod` 100000 == 0) $ do
           --     putStrLn $ "Positions completed: " ++ show r
           --     hFlush stdout
           when debug $ do
               putStrLn $ "Line: " ++ line
               hFlush stdout
           -- The lines we expect contain: fen ',' score ',' rez or (when dual) fen ',' rez
           case sampler line of
               Nothing -> return (True, a)
               Just (pos, sco, rez) -> do
                   let scores = map (posExactEval pos) ess
                       losses = map (loss sco rez . fromIntegral) scores
                   when debug $ do
                       putStrLn $ "Line: " ++ line
                       putStrLn $ "- sco:   " ++ show sco
                       putStrLn $ "- rez:   " ++ show rez
                       putStrLn $ "- score: " ++ show scores
                       putStrLn $ "- loss:  " ++ show losses
                       hFlush stdout
                   let !a' = eaAccum pos sco rez scores losses a
                   if eaStop a' then return (False, a') else return (True, a')
                   -- return (False, a')

type Pred = FilePath -> Bool

maybeFilterFilePaths :: Maybe Pred -> [FilePath] -> [FilePath]
maybeFilterFilePaths maybePred
    | Just filt <- maybePred = filter filt
    | otherwise              = id

getFileList :: FilePath -> Bool -> Maybe Pred -> IO [FilePath]
getFileList filePath emptyok maybePred = do
    -- putStrLn $ "Get File List: " ++ filePath
    isDir <- doesDirectoryExist filePath
    fileList <- if isDir
                   then listDirectory filePath
                   else do
                       isFile <- doesFileExist filePath
                       if isFile
                          then return [takeFileName filePath]
                          else if emptyok
                                  then return []
                                  else fail $ "Directory of file " ++ filePath ++ " not found"
    let fl = maybeFilterFilePaths maybePred $ map ((filePath </>)) fileList
    -- putStrLn $ "All files: " ++ show fl
    when (null fl && not emptyok) $ do
        putStrLn $ "No matching file found in " ++ filePath
        fail $ "Files: " ++ show fileList
    return fl

-- Loss function type: depends on target score, target result and actual score
type Loss = Double -> Double -> Double -> Double

-- theLoss :: Loss
-- theLoss = lossScoreRez
-- theLoss = lossRez
-- theLoss = lossSF

-- A dummy loss for the Step method
lossDist :: Loss
lossDist tgsc _ sc = abs (tgsc - sc)

-- Score/result sigmoid loss model, ignore too high scores
lossScoreRez :: Double -> Double -> Double -> Loss
lossScoreRez maxScore scale frasc tgsc rez sc
    | abs tgsc <= maxScore = x * x
    | otherwise            = 0
    where wdl_tgsc   = sigmoid (scale * tgsc)
          wdl_target = frasc * wdl_tgsc + (1 - frasc) * rez / 2
          wdl_model  = sigmoid (scale * sc)
          x = wdl_target - wdl_model

-- Score/sigmoid loss model, no result
-- The target score sigmoid decides the weight of the loss,
-- which itself is squared error
lossScoreWeight :: Double -> Loss
lossScoreWeight scale tgsc _ sc = x * x * weight
    where wdl_tgsc = sigmoid (scale * tgsc)
          -- The weight of the error is: 1 for target wdl of 0.5 and 0 for those of 0 or 1
          -- corresponding to 1 for target scores around 0 (draw) and 0 for target scores
          -- which are near wone or near lost
          weight = 1 - 2 * abs (wdl_tgsc - 0.5)
          x = tgsc - sc

-- Score/sigmoid loss model, no result
-- The target score sigmoid decides the weight of the loss,
-- which itself is the squared error of sigmoids
-- We have 2 scales, one for the score itself and one for the weight
lossScoreSigWeight :: Double -> Double -> Loss
lossScoreSigWeight sscale wscale tgsc _ sc = x * x * weight
    where wdl_tgsc  = sigmoid (sscale * tgsc)
          wdl_model = sigmoid (sscale * sc)
          x = wdl_tgsc - wdl_model
          -- The weight of the error is: 1 for target wdl of 0.5 and 0 for those of 0 or 1
          -- corresponding to 1 for target scores around 0 (draw) and 0 for target scores
          -- which are near wone or near lost
          ws = sigmoid (wscale * tgsc)
          weight = 4 * ws * (1 - ws)

-- A loss function which counts how many times the model score is outside of a given range of
-- the target score - less is better
lossScoreOutside :: Double -> Double -> Loss
lossScoreOutside luft scale tgsc _ sc
    | sdist <= luft = 0
    | otherwise     = 2 * sigmoid (scale * (sdist - luft)) - 1
    where sdist = abs (sc - tgsc)

-- Exp linear loss: x * e ^ (-x)
lossExpLinear :: Double -> Loss
lossExpLinear scale tgsc _ sc = sdist * exp (-sdist)
    where sdist = scale * abs (sc - tgsc)

-- Rezult sigmoid loss model
-- It expects result to be 0, 1 or 2 (loss, draw, win)
lossRez :: Double -> Loss
lossRez scale _ rez sc = x * x
    where wdl_target = rez / 2
          wdl_model  = sigmoid (scale * sc)
          x = wdl_target - wdl_model

-- Loss model like Stockfish (see https://github.com/official-stockfish/nnue-pytorch/blob/master/model.py)
lossSF :: Double -> Double -> Loss
lossSF sscale wscale tgsc _ sc = exp (2.5 * log (abs (pf - qf)))
    where -- in_scaling  = 340
          -- out_scaling = 380
          pf = scoreToRezult tgsc wscale
          qf = scoreToRezult sc   sscale

scoreToRezult :: Double -> Double -> Double
scoreToRezult sc scaling = 0.5 * (1 + sigmoid pp - sigmoid pm)
    where offset = 270
          pp = ( sc - offset) / scaling
          pm = (-sc - offset) / scaling

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (-x))

filteredCsv :: Pred
filteredCsv f = isCsv && isFilt
    where isCsv = takeExtension f == ".csv"
          isFilt = "-filt" `isSuffixOf` (fst $ splitExtension f)

justCsv :: Pred
justCsv f = isCsv
    where isCsv = takeExtension f == ".csv"

filteredEpd :: Pred
filteredEpd f = isEpd -- && isFilt
    where isEpd = takeExtension f == ".epd"

-- Evaluate a loss function with the given eval state for all CSV file in the given list
evaluateLoss :: EvalAccum a => String -> Loss -> [OptParams] -> [FilePath] -> Sampler -> IO a
evaluateLoss mess loss xs files sampler = do
    putStrLn $ "--> Eval " ++ mess
    hFlush stdout
    let ess = map vecToEvalState xs
    (_, a) <- loopCount (evalFile loss sampler ess) (files, eaInit)
    return a

train :: Options -> IO ()
train opts = do
    ds <- makeDataset True opts
    stime <- getCurrentTime
    if optTrain opts > 0
       then trainParams  ds opts
       else searchParams ds opts
    etime <- getCurrentTime
    putStrLn $ "Time: " ++ show (diffUTCTime etime stime)
    return ()

optsToLossFunction :: Options -> Loss
optsToLossFunction opts
    | optLossFun opts == 1 = lossScoreSigWeight (optSigScale opts) (optWeiScale opts)
    | optLossFun opts == 2 = lossScoreOutside   (optLossLuft opts) (optSigScale opts)
    | optLossFun opts == 3 = lossSF             (optSigScale opts) (optWeiScale opts)
    | optLossFun opts == 4 = lossExpLinear      (optSigScale opts)
    | optLossFun opts == 5 = lossScoreRez       2000 (optSigScale opts) (optScoreWei opts)
    | otherwise            = lossRez            (optSigScale opts)

testLoss :: Options -> IO ()
testLoss opts = do
    ds <- makeDataset False opts
    let loss = optsToLossFunction opts
    EASimple _ l <- evaluateLoss "Train only" loss
                        [(U.fromList optSpaceInit)] (dsTrainFiles ds) (dsSampleFunc ds)
    putStrLn $ "Loss: " ++ show l
    return ()

checkStep :: Options -> IO ()
checkStep opts = do
    ds <- makeDataset True opts
    let x  = U.fromList optSpaceInit
        xs = [x] ++ genPlusVec x ++ genMinusVec x
        fs | optDebug opts = case dsTrainFiles ds of
                                 []  -> []
                                 f:_ -> [f]
           | otherwise     = dsTrainFiles ds
    eas <- evaluateLoss "Check Step" lossDist xs fs (dsSampleFunc ds)
    -- when debug $ do
    --     putStrLn "The vectors:"
    --     forM_ (take 5 xs) $ \x -> putStrLn (show x)
    reportEAStat eas
    return ()

-- Dataset: a data structure which contains the CSV files and can procces them in batches,
-- calculating the batch mean loss given an evaluation state
data Dataset = Dataset {
        dsTrainFiles  :: [FilePath],	-- list of the training files
        dsTestFiles   :: [FilePath],	-- list of the test files
        dsTrainHandle :: Maybe Handle,	-- the handle of the current open training file, if any
        dsRestFiles   :: [FilePath],	-- remaining training files, repeating
        dsSampleFunc  :: Sampler	-- this is the function used to read a sample from a line
    }

type OptParams = U.Vector Double	-- optimization space

type Sampler = String -> Maybe (MyPos, Double, Double)

makeDataset :: Bool -> Options -> IO Dataset
makeDataset training opts = do
    let filefilter | optType opts == 1 = filteredEpd	-- gets EPD files (*.epd)
                   | optType opts == 0 = justCsv	-- gets CSV files (*.csv)
                   | otherwise         = filteredCsv	-- CSV files with suffix "-filt" (*-filt.csv)
    trainFiles <- if optOptim opts > 0 || optTest opts
                     then getFileList (optCsvPath opts)             False (Just filefilter)
                     else getFileList (optCsvPath opts </> "train") False (Just filefilter)
    testFiles  <- if optOptim opts > 0
                     then return []
                     else getFileList (optCsvPath opts </>  "test") True  (Just filefilter)
    rtf <- if training && optShuffle opts
              then do
                  g <- newStdGen
                  return $ shuffleList g trainFiles
              else return trainFiles
    case optType opts of
       0 -> return Dataset {
                dsTrainFiles = rtf, dsTestFiles = testFiles, dsTrainHandle = Nothing,
                dsRestFiles = concat (repeat rtf), dsSampleFunc = sampleFenScoreLine
            }
       1 -> return Dataset {
                dsTrainFiles = rtf, dsTestFiles = testFiles, dsTrainHandle = Nothing,
                dsRestFiles = concat (repeat rtf), dsSampleFunc = sampleFenResultLine
            }
       2 -> return Dataset {
                dsTrainFiles = rtf, dsTestFiles = testFiles, dsTrainHandle = Nothing,
                dsRestFiles = concat (repeat rtf), dsSampleFunc = sampleFenScoreResultLine
            }
       i -> fail $ "Wrong input file type " ++ show i

shuffleList :: (Ord a, RandomGen g) => g -> [a] -> [a]
shuffleList g as = map snd $ sort $ zip (randInts g) as

randInts :: RandomGen g => g -> [Int]
randInts g = randoms g

-- Get a batch of training samples (position, target score, target result) from the dataset
-- This changes the state of the dataset, which is why it is returning also the "changed" dataset
getTrainBatch :: Int -> Dataset -> IO (Dataset, [(MyPos, Double, Double)])
getTrainBatch batchSize ds = do
    (ds', _, ts) <- loopCount (getSampleFromDS batchSize) (ds, 0, [])
    return (ds', ts)

-- Get one training sample from the dataset (position, target score, target result)
-- This changes the state of the dataset, which is why it is returning also the "changed" dataset
getSampleFromDS :: Int -> Int -> (Dataset, Int, [(MyPos, Double, Double)])
                -> IO (Bool, (Dataset, Int, [(MyPos, Double, Double)]))
getSampleFromDS batchSize _ (ds, n, ps)
    | n == batchSize = return (False, (ds, n, ps))
    | otherwise      =
        -- Is a file already open?
        case dsTrainHandle ds of
            Nothing -> case dsRestFiles ds of
                -- If not, open the next file and retry
                [] -> error "Dataset training file list is empty"
                inFileName : restFiles -> do
                    putStrLn $ "DS: open file " ++ inFileName
                    hFlush stdout
                    hi <- openFile inFileName ReadMode
                    return (True, (ds { dsRestFiles = restFiles, dsTrainHandle = Just hi }, n, ps))
            Just hi -> do
                -- Is end of file of the training file?
                end <- hIsEOF hi
                if end
                   then do
                       -- If yes, close it and retry
                       hClose hi
                       return (True, (ds { dsTrainHandle = Nothing }, n, ps))
                   else do
                       line <- hGetLine hi
                       case dsSampleFunc ds line of
                           Just (pos, sco, rez) -> return (True, (ds, n + 1, (pos, sco, rez) : ps))
                           Nothing              -> return (True, (ds, n, ps))

-- Get a training (or test) sample from a line: i.e. position, target score and target result
-- The lines we expect contain: fen ',' score ',' rez
-- The result is 0, 1 or 2 (loss, draw, win)
-- Here the result is from moving part POV - it must be taken as it is
sampleFenScoreResultLine :: Sampler
sampleFenScoreResultLine line
    | scs == "" || res == "" = Nothing
    | otherwise              = Just (pos, sco, rez)
    where (fen, rest)  = break ((==) ',') line
          (scs, rest1) | ',':tr <- rest = break ((==) ',') tr
                       | otherwise      = ("", "")
          pos = posFromFen fen
          -- Target score in centipawns
          sco = read scs
          -- Rezult is:
          res | ',':ress <- rest1 = ress
              | otherwise         = ""
          rez = read res

-- Get a training (or test) sample from a line with fen and target result
-- This is the format used in older training files publicly available (Ethereal, Stockfish)
-- and used to train them after the Texel method
-- The lines we expect contain: fen ',' rez
-- We still return a score, but set to 0
-- The result is -1, 0 or 1 (loss, draw, win) - so we must correct it
-- Here the result is from White POV - we must revert it from black moving positions
sampleFenResultLine :: Sampler
sampleFenResultLine line
    | ',':res <- rest =
        let rez = read res
            rezm | moving pos == White = rez + 1
                 | otherwise           = 1 - rez
        in Just (pos, 0, rezm)
    | otherwise = Nothing
    where (fen, rest)  = break ((==) ',') line
          pos = posFromFen fen

-- Get a training (or test) sample from a line with fen and target score
-- The lines we expect contain: fen ',' score
-- The score is in centipawns from moving part POV
-- A result is still returned, set to 0
sampleFenScoreLine :: Sampler
sampleFenScoreLine line
    | ',':scs <- rest = Just (pos, read scs, 0)
    | otherwise       = Nothing
    where (fen, rest)  = break ((==) ',') line
          pos = posFromFen fen

-- The training status
data TrainState = TrainState {
        tsLoss    :: Loss,	-- Fix: the loss function
        tsBatchSz :: !Int,	-- Fix: the batch size
        tsKeep    :: Maybe Int,	-- Fix: the number of best samples to keep
        tsBatches :: !Int,	-- Fix: number of batches to train
        tsNames   :: [String],	-- Fix: the names of the parameters to be optimized
        tsValidNo :: !Int,	-- Fix: validate every so many batches
        tsValHigh :: !Double,	-- Fix: validation high factor, after which we lower the LR
        tsLRLower :: !Double,	-- Fix: LR lowering factor when validation value to high
        tsLRHighr :: !Double,	-- Fix: LR increasing factor when validation value repeat
        tsMaxChg  :: !Double,	-- Fix: maximum change per parameter
        tsFix     :: !Double,	-- Fix: a fix param change for when the quadratic model won't match
        tsDataset :: Dataset,	-- the dataset of the training
        tsLR      :: !Double,	-- the current learning rate of the training
        tsBatchNo :: !Int,	-- training batches so far
        tsCurrent :: OptParams,	-- current values of the optimal parameter
        tsMinVal  :: !Double,	-- minimum reached validation value
        tsLastVal :: !Double,	-- last validation value
        tsHistory :: [(Int, OptParams, Double)]	-- training history (batch, params, validation loss)
    }

-- We have the function value for xi in k-1, k and k+1 (all other params xj being fixed)
-- and want to move towards minimizing it for integer values.
-- We have classes of cases:
-- A. All values are different (most of the time)
-- B. 2 Values are equal, one different
-- C. All three values are equal
--
-- Let's analyse the cases of A:
-- We have all together 6 cases, coded here by enumerating the values in ascending order,
-- with 1 denoting fkm1, 2 for fk and 3 for fkp1:
-- A1. 123 - the minimum is to the left of k-1 - step is -1 (i.e. go to k-1)
-- A2. 132 - we are near a maximum, but the step is like in case A1: -1
-- A3. 213 - k is the minimum, step is 0
-- A4. 231 - same as case A3, step is 0
-- A5. 312 - we are near a maximum, go right (step is +1)
-- A6. 321 - minimum is to the right (step is +1)
--
-- Let's analyse case B:
-- We have following cases:
-- B1. fk equal to some of the other
--    - B1a - the not equal one is higher - step is to the other
--    - B1b - the not equal one is lower - step is to it
-- B2. The other 2 are equal, fk not
--    - B2a - fk is higher - any step is ok (this is a local maximum) - but how to chose?
--    - B2b - fk is lower - this is a minimu: step is 0
--
-- Let's analyse case C:
-- In this case we can stay where we are, as it does not matter, step is 0
--
stepToMinimum :: Double -> (Double, Double, Double) -> Double
stepToMinimum lr (fkm1, fk, fkp1)
    | fk < fkm1  = if fk < fkp1
                      then 0	-- this is the minimum
                      else lr	-- right in any case
    | fk == fkm1 = if fk < fkp1
                      then -lr	-- left
                      else if fk == fkp1
                              then 0	-- does not matter
                              else lr	-- right
    | otherwise = if fkm1 < fkp1
                     then -lr	-- left
                     else lr	-- right

-- Given the function to optimize and the current parameter vector,
-- calculate and return the value of the next parameter vector
nextVector :: Double -> (OptParams -> Double) -> OptParams
           -> (Double, [((Double, Double, Double), Double)], OptParams)
nextVector lr f x = (loss0, zip stpa step, U.zipWith (+) x (U.fromList step))
    where loss0 = f x
          lossm = map f $ genMinusVec x
          lossp = map f $ genPlusVec  x
          -- Estimate the step per parameter by cases
          stpa = zip3 lossm (repeat loss0) lossp
          step = map (stepToMinimum lr) stpa

trainOneBatch :: Int -> TrainState -> IO (Bool, TrainState)
trainOneBatch k ts = do
    if tsBatchNo ts >= tsBatches ts
        then if not $ null $ dsTestFiles $ tsDataset ts
                then do
                    -- We trained to the end, calculate the final validation loss
                    let ds = tsDataset ts
                    EASimple _ tl <- evaluateLoss "Test" (tsLoss ts) [(tsCurrent ts)]
                                                  (dsTestFiles ds) (dsSampleFunc ds)
                    let hi = (tsBatchNo ts, tsCurrent ts, tl) : tsHistory ts
                    return (False, ts { tsHistory = hi })
                else return (False, ts)
        else do
            let validation = tsValidNo ts /= 0 && mod (tsBatchNo ts) (tsValidNo ts) == 0
            (tl, mv, lr, hi)
                <- if validation
                      then do
                          let ds = tsDataset ts
                          -- We trained enough batches to calculate the next validation loss
                          -- Record the minimum validation value and possibly recalculate the LR
                          EASimple _ tl <- evaluateLoss "Test" (tsLoss ts) [(tsCurrent ts)]
                                                        (dsTestFiles ds) (dsSampleFunc ds)
                          let mv | tl < tsMinVal ts = tl
                                 | otherwise        = tsMinVal ts
                              lr | tl > tsMinVal ts * tsValHigh ts = tsLR ts * tsLRLower ts
                                 | tl == tsLastVal ts              = tsLR ts * tsLRHighr ts
                                 | otherwise                       = tsLR ts
                          return $ (tl, mv, lr, (tsBatchNo ts, tsCurrent ts, tl) : tsHistory ts)
                      else return (tsLastVal ts, tsMinVal ts, tsLR ts, tsHistory ts)
            -- get new batch and calculate the loss for current point and the derivatives
            when (mod k 100 == 0)
                $ putStrLn $ "*** Start batch " ++ show k ++ " / " ++ show (tsBatches ts) ++ " ***"
            hFlush stdout
            (ds, batch) <- getTrainBatch (tsBatchSz ts) (tsDataset ts)
            let bsrec = 1 / fromIntegral (tsBatchSz ts) -- we have always exactly batschSize records
                -- Calculate next vector from this batch of samples
                (!loss0, _, vecn) = nextVector lr (lossPerBatch batch (tsKeep ts) (tsLoss ts) bsrec) (tsCurrent ts)
            when (mod k 100 == 0) $ do
                putStrLn $ "Current loss: " ++ show loss0
                putStrLn $ "Current LR:   " ++ show lr
                putStrLn $ "Current vec:  " ++ show (tsCurrent ts)
                putStrLn $ "Next vec:     " ++ show vecn
                hFlush stdout
            return (True, ts { tsDataset = ds, tsBatchNo = tsBatchNo ts + 1, tsCurrent = vecn,
                               tsLastVal = tl, tsMinVal = mv, tsLR = lr, tsHistory = hi })

-- Calculate the loss per batch - with the possibility to discard the worse samples
-- The first parameter gives the number of best samples to consider (the rest is discarded)
-- Unfortunately, the conversion from parameter vector to eval state must be done for every function
-- call (and there are many, because of the calculation of the derivatives), otherwise we cannot
-- abstract the optimization function from the optimization procedure
lossPerBatch :: [(MyPos, Double, Double)] -> Maybe Int -> Loss -> Double -> OptParams -> Double
lossPerBatch batch mkeep loss scale x = case mkeep of
    Just keep -> scale * sum (take keep $ sort allLosses)
    Nothing   -> scale * sum allLosses
    -- Suppose that the score is from White POV:
    where allLosses = map (\(p, s, r) -> loss s r (fromIntegral $ posExactEval p es)) batch
          es = vecToEvalState x

-- Change the i-th component of a vector by adding val on it
-- We will add 1 or -1, hence the Int here
vecChange :: OptParams -> Int -> Int -> OptParams
vecChange vec val i = vec U.// [(i, v)]
    where v = fromIntegral (round (vec U.! i) + val)

-- Generate vector for partial derivative with the minus 1 components
genMinusVec :: OptParams -> [OptParams]
genMinusVec vec = map (vecChange vec (-1)) [0..l]
    where l = U.length vec - 1

-- Generate vector for partial derivative with the plus 1 components
genPlusVec :: OptParams -> [OptParams]
genPlusVec vec = map (vecChange vec 1) [0..l]
    where l = U.length vec - 1

-- Generate the eval state from the given vector
vecToEvalState :: OptParams -> EvalState
vecToEvalState = initEvalState . zip optSpaceNames . U.toList

trainParams :: Dataset -> Options -> IO ()
trainParams ds opts = do
    let bads = round (fromIntegral (optBatchSz opts) * fromIntegral (optTolerate opts) / 100 :: Double)
        keep = optBatchSz opts - bads
        mkeep | bads == 0 = Nothing
              | otherwise = Just keep
        lossf = optsToLossFunction opts
        tsi = TrainState {
            tsLoss    = lossf,
            tsBatchSz = optBatchSz opts,
            tsKeep    = mkeep,
            tsBatches = optTrain opts,
            tsNames   = optSpaceNames,
            tsValidNo = optValidBa opts,
            tsValHigh = 1.05,
            tsLRLower = 0.5,
            tsLRHighr = 1.5,
            tsMaxChg  = optMaxChange opts,
            tsDataset = ds,
            tsLR      = optLR opts,
            tsFix     = optMaxChange opts / 10,
            tsBatchNo = 0,
            tsCurrent = U.fromList optSpaceInit,
            tsMinVal  = 1e12,	-- should be higher than any test loss
            tsLastVal = 0,
            tsHistory = []
        }
    tsf <- loopCount trainOneBatch tsi
    putStrLn "History:"
    forM_ (tsHistory tsf) $ \(i, _, l) -> putStrLn $ "Batch " ++ show i ++ ": " ++ show l
    putStrLn "Current vec:"
    forM_ (zip (tsNames tsf) (U.toList $ tsCurrent tsf)) $ \(n, v) -> putStrLn $ n ++ " = " ++ show v
    EASimple ri acci <- evaluateLoss "Train loss initial" (tsLoss tsi) [(tsCurrent tsi)]
                                   (dsTrainFiles ds) (dsSampleFunc ds)
    EASimple rf accf <- evaluateLoss "Train loss final"   (tsLoss tsf) [(tsCurrent tsf)]
                                   (dsTrainFiles ds) (dsSampleFunc ds)
    putStrLn $ "Initial: " ++ show acci ++ " / " ++ show ri ++ " = " ++ show (acci / fromIntegral ri)
    putStrLn $ "Final:   " ++ show accf ++ " / " ++ show rf ++ " = " ++ show (accf / fromIntegral rf)
    writeWeights opts (hsPreambTrain opts tsf) (tsNames tsf) (tsCurrent tsf)

hsPreambTrain :: Options -> TrainState -> Handle -> IO ()
hsPreambTrain opts ts fo = do
    hPutStrLn fo "-- This module is generated by a TuneSGD run with following parameters:"
    hPutStrLn fo $ "-- dataset " ++ optCsvPath opts ++ " with lr = " ++ show (tsLR ts)
    hPutStrLn fo $ "-- batch size = " ++ show (tsBatchSz ts) ++ " with " ++ show (tsBatches ts) ++ " batches"
    -- hPutStrLn fo $ "-- scale = " ++ show (optSigScale opts) ++ ", luft = " ++ show (optLossLuft opts)
    hPutStrLn fo $ "-- scale = " ++ show (optSigScale opts) ++ ", wscale = " ++ show (optWeiScale opts)
    when ((optTolerate opts) > 0) $
        hPutStrLn fo $ "-- drop " ++ show (optTolerate opts) ++ "% of the worse scores"

hsPreambOptim :: Options -> OptimState -> Handle -> IO ()
hsPreambOptim opts _ fo = do
    hPutStrLn fo "-- This module is generated by a TuneSGD optimization run with following parameters:"
    hPutStrLn fo $ "-- dataset " ++ optCsvPath opts
    hPutStrLn fo $ "-- scale = " ++ show (optSigScale opts) ++ ", wscale = " ++ show (optWeiScale opts)

-- Write a Haskell module with the final weights
writeWeights :: Options -> (Handle -> IO ()) -> [String] -> OptParams -> IO ()
writeWeights opts preamb names best = withFile (optOutPath opts) WriteMode $ \fo -> do
    preamb fo
    hPutStrLn fo "module Struct.Weights ("
    hPutStrLn fo "    weights,"
    hPutStrLn fo ") where"
    hPutStrLn fo "import Struct.ParamsPre"
    hPutStrLn fo "-- Every weight has 2 values, for mid and for end game"
    hPutStrLn fo "-- All weights have type MidEnd"
    hPutStrLn fo "weights :: [EvalWeightSpec]"
    hPutStrLn fo "weights = ["
    -- Because our weights have names like "mid.ewKingSafe", we misuse the FilePath functions
    -- to get the parts - as we need to get only the names, like "ewKingSafe" and the 2 values
    -- corresponding to mid & end
    let phases = map takeBaseName             names
        pnames = map (drop 1 . takeExtension) names	-- takeExtention gives ".ewKingSafe"
        npvs   = zipWith3 (\n p v -> (n, (ordPhase p, round v))) pnames phases (U.toList best)
        trips  = map snd $ sort $ consumeWeights $ sort $ zip npvs [1..]
    forM_ (zip [0::Int ..] trips) $ \(i, (n, v1, v2)) -> do
        if (i > 0) then hPutStr fo "      , " else hPutStr fo "        "
        hPutStrLn fo $ "(" ++ justifyLeft 22 ' ' ("\"" ++ n ++ "\",")
            ++ "(" ++ justifyRight 5 ' ' (show v1) ++ "," ++ justifyRight 5 ' ' (show v2) ++ "))"
    hPutStrLn fo "    ]"
    where ordPhase :: String -> Int
          ordPhase "mid" = 1
          ordPhase "end" = 2
          ordPhase p     = error $ "Wrong phase in writeWeights: " ++ p
          consumeWeights :: [((String, (Int, Int)), Int)] -> [(Int, (String, Int, Int))]
          consumeWeights = reverse . go []
              where go acc [] = acc
                    go acc (((s1, (1, v1)), i1):((s2, (2, v2)), i2):ws)
                        | s1 == s2  = go ((min i1 i2, (s1, v1, v2)):acc) ws
                        | otherwise = error "Wrong order in consumeWeights"
                    go _ _ = error "consumeWeights: odd number of weights"

-- The optimization status
data OptimState = OptimState {
        opLoss    :: Loss,	-- Fix: the loss function
        opNames   :: [String],	-- Fix: the names of the parameters to be optimized
        opDataset :: Dataset,	-- the dataset of the training
        opMax     :: Int,	-- maximum steps
        opFails   :: Int,	-- maximum fails to improve for termination
        opFixIdx  :: Int,	-- fixed weight
        opHistory :: [(OptParams, Double, Int)]	-- training history (params, loss & fails)
    }

searchParams :: Dataset -> Options -> IO ()
searchParams ds opts = do
    let lossf = optsToLossFunction opts
        opi = OptimState {
            opLoss    = lossf,
            opNames   = optSpaceNames,
            opDataset = ds,
            opMax     = optOptim opts,
            opFails   = 0,
            opFixIdx  = optFixIdx opts,
            opHistory = []
        }
    opf <- loopCount optimDim opi
    putStrLn "History:"
    forM_ (reverse $ opHistory opf) $ \(_, l, i) -> putStrLn $ "Loss " ++ show l ++ ": " ++ show i
    case opHistory opf of
        []           -> putStrLn "Empty history??"
        (cv, _, _):_ -> do
            putStrLn "Current vec:"
            forM_ (zip (opNames opf) (U.toList cv)) $ \(n, v) -> putStrLn $ n ++ " = " ++ show v
            writeWeights opts (hsPreambOptim opts opf) (opNames opf) cv

optDims :: Int
optDims = length optSpaceInit

optimDim :: Int -> OptimState -> IO (Bool, OptimState)
optimDim k op
    | k > opMax op = return (False, op)
    | opFixIdx op >= 0
      && opFixIdx op == ix = do
          putStrLn $ "<<< Skip dimension " ++ show ix ++ " (" ++ dimIdxToDimName ix ++ ")"
          return (True, op)
    | otherwise = do
    let ((best, bl, since), first)
            | hit:_ <- opHistory op = (hit, False)
            | otherwise             = ((U.fromList optSpaceInit, 1e100, 0), True)
    if opFails op > 0 && since > opFails op
       then return (False, op)
       else do
           let candidates = generateCandidates best first ix
               ds = opDataset op
               tx = "Optimize " ++ show k ++ " (" ++ dimIdxToDimName ix ++ ")"
           eam <- evaluateLoss tx (opLoss op) candidates (dsTrainFiles ds) (dsSampleFunc ds)
           let (mini, bl') = bestLoss eam
               blf | first     = firstLoss eam	-- original loss before any change
                   | otherwise = bl
           if bl' < blf
              then do
                  let best' = candidates !! mini
                      oph   = (best', bl', 0) : opHistory op
                      gain  = round ((blf - bl') * 1000000 / blf) :: Int
                  putStrLn $ "*** New best: " ++ show bl' ++ " < " ++ show blf ++ " (" ++ show gain ++ " ppm)"
                  putStrLn $ show best'
                  return (True, op { opHistory = oph })
              else do
                  let oph = (best, blf, since + 1) : drop 1 (opHistory op)
                  return (True, op { opHistory = oph })
    where ix = (k - 1) `mod` optDims

-- On the first step we also evaluate the original weights (first "candidate")
generateCandidates :: OptParams -> Bool -> Int -> [OptParams]
generateCandidates c first i
    | first     = c : rs
    | otherwise = rs
    where rs = le ++ ri
          le = map (flip (vecChange c) i) [-1, -2, -4, -8, -16]
          ri = map (flip (vecChange c) i) [ 1,  2,  4,  8,  16]

dimIdxToDimName :: Int -> String
dimIdxToDimName = (!!) optSpaceNames

-- Find the minimum of the Rosenbrock function
minRosenbrock :: Double -> Double -> Double -> Int -> IO ()
minRosenbrock x y lr steps = do
    putStrLn "Minimize Rosenbrock"
    v <- loopCount (stepRosenbrock lr steps) (U.fromList [x, y])
    putStrLn $ "Rosenbrock final: " ++ show v

printAll, doTrace :: Bool
printAll = True
doTrace  = True

stepRosenbrock :: Double -> Int -> Int -> OptParams -> IO (Bool, OptParams)
stepRosenbrock lr steps k x
    | k >= steps = return (False, x)
    | otherwise = do
        let (val, evals, xf) = nextVector lr rosenbrock x
        when (printAll || mod k 20 == 0) $ do
            putStrLn $ show k ++ ": x = " ++ show x ++ " val = " ++ show val ++ " evals: " ++ show evals
        return (True, xf)

-- Has a minimum of 0 at 10, 100
-- This is at a scale of 10x compared to the original Rosenbrock (1, 100)
rosenbrock :: OptParams -> Double
rosenbrock v
    | doTrace  = traceShow (v, val) val
    | otherwise = val
    -- where x = fromIntegral (round (v U.! 0) :: Int)
    --       y = fromIntegral (round (v U.! 1) :: Int)
    where x = v U.! 0
          y = v U.! 1
          y2  = y - x * x
          val = (a - x) * (a - x) + b * y2 * y2
          a = 10
          b = 1000
