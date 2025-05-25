{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

module Main where
import Control.Monad (when, forM_)
import Data.List (isSuffixOf)
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import Data.Time.Clock

import qualified Data.Vector.Unboxed as U

import Struct.Status (EvalState(..))
import Struct.Struct
-- import Struct.Context
-- import Struct.Config
import Struct.Params (optSpaceNames, optSpaceInit)
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
        Option "t" ["train"]  (NoArg addTeste) "Train Parameter"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName ++ " -g -i PATH -o PATH | -t -i PATH"
          idName = "TuneSGD"

main :: IO ()
main = do
    (opts, _) <- theOptions
    if optTeste opts
        then mainSGD (optCsvPath opts)
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
                hFlush stdout
                hi <- openFile inFileName ReadMode
                (r', acc') <- loopCount (evalPos loss es hi) (0, 0)
                putStrLn $ "Records: " ++ show r' ++ ", loss = " ++ show acc'
                hFlush stdout
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
           -- when (r `mod` 100000 == 0) $ do
           --     putStrLn $ "Positions completed: " ++ show r
           --     hFlush stdout
           when debug $ do
               putStrLn $ "Line: " ++ line
               hFlush stdout
           -- The lines we expect contain: fen ',' score ',' rez
           let (pos, sco, rez) = sampleFromLine line
               score = posExactEval pos es
               thisLoss = loss sco rez (fromIntegral score)
           when debug $ do
               putStrLn $ "Line: " ++ line
               putStrLn $ "- sco:   " ++ show sco
               putStrLn $ "- rez:   " ++ show rez
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
    -- putStrLn $ "All files: " ++ show fl
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
    putStrLn "--> Begin full eval"
    hFlush stdout
    (_, r, acc) <- loopCount (evalFile loss es) (files, 0, 0)
    let meanLoss = acc / fromIntegral r
    putStrLn $ "<-- Full eval: " ++ show acc ++ " / " ++ show r ++ " = " ++ show meanLoss
    hFlush stdout
    return meanLoss

mainSGD :: FilePath -> IO ()
mainSGD inPath = do
    ds <- makeDataset (inPath </> "train") (inPath </> "test")
    stime   <- getCurrentTime
    trainParams ds 1000 10240 0.001
    etime   <- getCurrentTime
    putStrLn $ "Time: " ++ show (diffUTCTime etime stime)
    return ()

-- Dataset: a data structure which contains the CSV files and can procces them in batches,
-- calculating the batch mean loss given an evaluation state
data Dataset = Dataset {
        dsTrainFiles  :: [FilePath],	-- list of the training files (repeatig!)
        dsTrainHandle :: Maybe Handle,	-- the handle of the current open training file, if any
        dsTestFiles   :: [FilePath]	-- list of the test files (once)
    }

type OptParams = U.Vector Double	-- optimization space

makeDataset :: FilePath -> FilePath -> IO Dataset
makeDataset trainDir testDir = do
    trainFiles <- getFileList trainDir (Just filteredCsv)
    testFiles  <- getFileList testDir  (Just filteredCsv)
    return Dataset {
        dsTrainFiles = concat (repeat trainFiles), dsTrainHandle = Nothing, dsTestFiles = testFiles
    }

testLoss :: EvalState -> Dataset -> IO Double
testLoss es ds = evaluateLoss lossPerScore es (dsTestFiles ds)

-- Get a batch of training samples (position, target score, target rezult) from the dataset
-- This changes the state of the dataset, which is why it is returning also the "changed" dataset
getTrainBatch :: Int -> Dataset -> IO (Dataset, [(MyPos, Double, Double)])
getTrainBatch batchSize ds = do
    (ds', _, ts) <- loopCount (getSampleFromDS batchSize) (ds, 0, [])
    return (ds', ts)

-- Get one training sample from the dataset (position, target score, target rezult)
-- This changes the state of the dataset, which is why it is returning also the "changed" dataset
getSampleFromDS :: Int -> Int -> (Dataset, Int, [(MyPos, Double, Double)])
                -> IO (Bool, (Dataset, Int, [(MyPos, Double, Double)]))
getSampleFromDS batchSize _ (ds, n, ps)
    | n == batchSize = return (False, (ds, n, ps))
    | otherwise      =
        -- Is a file already open?
        case dsTrainHandle ds of
            Nothing -> case dsTrainFiles ds of
                -- If not, open the next file and retry
                [] -> error "Dataset training file list is empty"
                inFileName : restFiles -> do
                    putStrLn $ "DS: open file " ++ inFileName
                    hFlush stdout
                    hi <- openFile inFileName ReadMode
                    return (True, (ds { dsTrainFiles = restFiles, dsTrainHandle = Just hi }, n, ps))
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
                       let (pos, sco, rez) = sampleFromLine line
                       return (True, (ds, n + 1, (pos, sco, rez) : ps))

-- Get a training (or test) sample from a line: i.e. position, target score and target rezult
-- The lines we expect contain: fen ',' score ',' rez
sampleFromLine :: String -> (MyPos, Double, Double)
sampleFromLine line = (pos, sco, rez)
    where (fen, rest)  = break ((==) ',') line
          (scs, rest1) = break ((==) ',') $ tail rest
          pos = posFromFen fen
          -- Target score in centipawns
          sco = read scs
          -- Rezult is:
          rez = read (tail rest1)

-- The training status
data TrainState = TrainState {
        tsDataset :: Dataset,	-- the dataset of the training
        tsNames   :: [String],	-- the names of the parameters to be optimized
        tsLR      :: Double,	-- the current learning rate of the training
        tsBatchNo :: Int,	-- training batches so far
        tsValidNo :: Int,	-- validate every so many batches
        tsCurrent :: OptParams,	-- current values of the optimal parameter
        tsHistory :: [(Int, OptParams, Double)]  -- training history (batch, params, validation loss)
    }

trainOneBatch :: Int -> Int -> Int -> TrainState -> IO (Bool, TrainState)
trainOneBatch maxBatches batchSize k ts = do
    es <- vecToEvalState (tsNames ts) (tsCurrent ts)
    if tsBatchNo ts == maxBatches
        then do
            -- We trained to the end, calculate the final validation loss
            tl <- testLoss es (tsDataset ts)
            let hi = (tsBatchNo ts, tsCurrent ts, tl) : tsHistory ts
            return (False, ts { tsHistory = hi })
        else do
            hi <- if mod (tsBatchNo ts) (tsValidNo ts) == 0
                     then do
                         -- We trained enough batches to calculate the next validation loss
                         tl <- testLoss es (tsDataset ts)
                         return $ (tsBatchNo ts, tsCurrent ts, tl) : tsHistory ts
                     else return (tsHistory ts)
            -- calculate evaluation states for the partial derivatives from current vector
            putStrLn "Calculate new vectors"
            let vminus  = genMinusVec (tsCurrent ts)
                vplus   = genPlusVec  (tsCurrent ts)
            putStrLn "Calculate new eval states"
            esminus <- mapM (vecToEvalState (tsNames ts)) vminus
            esplus  <- mapM (vecToEvalState (tsNames ts)) vplus
            -- get new batch and calculate the loss for currnet point and the derivatives
            putStrLn $ "*** Start batch " ++ show k ++ " ***"
            hFlush stdout
            (ds, batch) <- getTrainBatch batchSize (tsDataset ts)
            putStrLn "Calculate losses & gradient"
            let bsrec = 1 / fromIntegral batchSize -- we have always exactly batschSize records
                loss0 = bsrec * lossPerBatch lossPerScore es batch
                lossm = map (bsrec *) $ map (\esm -> lossPerBatch lossPerScore esm batch) esminus
                lossp = map (bsrec *) $ map (\esp -> lossPerBatch lossPerScore esp batch) esplus
                -- This is already the negative gradient estimate, and already multiplied with LR
                -- We need to limit the components, otherwise we get instability
                dloss = U.fromList $ map (max (-grdMax) . min grdMax) $ map (tsLR ts *) $ zipWith (-) lossm lossp
                vecn  = U.zipWith (+) (tsCurrent ts) dloss
            putStrLn $ "Current loss:   " ++ show loss0
            putStrLn $ "Current vec:    " ++ show (tsCurrent ts)
            putStrLn $ "Current change: " ++ show dloss
            putStrLn $ "Next vec:       " ++ show vecn
            hFlush stdout
            let lr = tsLR ts * 0.995
            return (True, ts { tsDataset = ds, tsLR = lr, tsBatchNo = tsBatchNo ts + 1, tsCurrent = vecn, tsHistory = hi })
    where grdMax = 1

lossPerBatch :: Loss -> EvalState -> [(MyPos, Double, Double)] -> Double
lossPerBatch loss es = sum . map (\(p, s, r) -> loss s r (fromIntegral $ posExactEval p es))

-- Change the i-th component of a vector
vecChange :: OptParams -> Double -> Int -> OptParams
vecChange vec val i = vec U.// [(i, vec U.! i + val)]

-- Generate vector for partial derivative with the minus 1 components
genMinusVec :: OptParams -> [OptParams]
genMinusVec vec = map (vecChange vec (-0.5)) [0..l]
    where l = U.length vec - 1

-- Generate vector for partial derivative with the plus 1 components
genPlusVec :: OptParams -> [OptParams]
genPlusVec vec = map (vecChange vec 0.5) [0..l]
    where l = U.length vec - 1

-- Generate the associations (name, value) for one vector
genAssocs :: OptParams -> [String] -> [(String, Double)]
genAssocs vec names = zip names $ U.toList vec

-- Generate the eval state from a given eval state and a vector
vecToEvalState :: [String] -> OptParams -> IO EvalState
vecToEvalState names vec = do
    (_, es) <- makeEvalState Nothing (genAssocs vec names) "" ""
    return es

trainParams :: Dataset -> Int -> Int -> Double -> IO ()
trainParams ds batches batchSize lr = do
    let trainState = TrainState {
            tsDataset = ds,
            tsNames   = optSpaceNames,
            tsLR      = lr,
            tsBatchNo = 0,
            tsValidNo = 1000,
            tsCurrent = U.fromList optSpaceInit,
            tsHistory = []
        }
    ts <- loopCount (trainOneBatch batches batchSize) trainState
    putStrLn "History:"
    forM_ (tsHistory ts) $ \(i, _, l) -> putStrLn $ "Batch " ++ show i ++ ": " ++ show l
    return ()
