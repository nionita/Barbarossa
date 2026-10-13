{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where
import Control.Monad (when, forM_)
import Data.List (intercalate, isPrefixOf, isSuffixOf, elemIndex, nub, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Numeric (showFFloat)
import System.Console.GetOpt
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.Random
import Data.Time.Clock
import Debug.Trace (traceShow)

import qualified Data.Vector.Unboxed as U

import Struct.Status (EvalState(..), EvalParams(..))
import Struct.MidEnd (MidEnd(MidEnd))
import Struct.Struct
import Struct.ParamsPost (optSpaceNames, optSpaceInit)
import Struct.Config (fileToParams)
import Moves.Core
import Eval.Core
import Tune.FenFiles
import Tune.EvalAccum

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
        optParams    :: [String],	-- parameters to optimize by search (empty = all)
        optRelabel   :: Int,	-- > 0: write input files with results drawn from the static eval (seed)
        optExport    :: Bool,	-- export eval features and pseudo derivatives for offline fitting
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
        optParams    = [],	-- default: all parameters
        optRelabel   = 0,
        optExport    = False,
        optX        = 0,
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

-- Comma separated list of parameter names to optimize
addParams :: String -> Options -> Options
addParams ba opt = opt { optParams = filter (not . null) (splitComma ba) }

splitComma :: String -> [String]
splitComma s = case break (== ',') s of
                   (a, [])     -> [a]
                   (a, _:rest) -> a : splitComma rest

setRelabel :: String -> Options -> Options
setRelabel ba opt = opt { optRelabel = read ba }

setExport :: Options -> Options
setExport opt = opt { optExport = True }

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
        Option "c" ["config"]   (ReqArg addConf    "STRING") "Config file with the start values",
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
        Option "p" ["params"]   (ReqArg addParams  "LIST")   "Parameters to optimize (-O), comma separated",
        Option "q" ["frasc"]    (ReqArg setScoreWei "FLOAT") "Score weight for score/result loss (0.5)",
        Option "r" ["rosen"]    (ReqArg addRosen "INT")      "Minimize Rosenbrock so many steps",
        Option "s" ["shuffle"]  (NoArg  setShuffle)          "Shuffle training files",
        Option "t" ["train"]    (ReqArg addTrain   "INT")    "Train so many batches",
        Option "v" ["validate"] (ReqArg addValidBa "INT")    "Validate every so many batches (1000)",
        Option "x" ["xi"]       (ReqArg addX "INT")          "Start X for Rosenbrock",
        Option "y" ["yi"]       (ReqArg addY "INT")          "Start Y for Rosenbrock",
        Option "L" ["loss"]     (ReqArg addLossFun "INT")    "Loss function: 0 - 3 (0)",
        Option "E" ["relabel"]  (ReqArg setRelabel "INT")    "Seed: write the input files to -o with results drawn from the static eval (-k, -c)",
        Option "F" ["fix"]      (ReqArg setFixIdx "INT")     "Fix this index 0 - dimensions (0)",
        Option "R" ["result"]   (NoArg  setResult)           "Input file format: fen,result",
        Option "S" ["score"]    (NoArg  setScore)            "Input file format: fen,score",
        Option "X" ["export"]   (NoArg  setExport)           "Export eval features of the input files to -o (-c)",
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
    if optExport opts
        then exportFiles opts
        else if optRelabel opts > 0
        then relabelFiles opts
        else if optTrain opts > 0 || optOptim opts > 0
        then train opts
        else if optGener opts
                then filterFile (optCsvPath opts) (optOutPath opts)
                else if optTest opts
                        then testLoss opts
                        -- then checkStep opts
                        else if optRosen opts > 0
                                then minRosenbrock (optX opts) (optY opts) (optLR opts) (optRosen opts)
                                else putStrLn $ "No useful option, should be one of -t, -e or -g"

-- Write copies of the input files (*-filt.csv) into the output directory, where the result is drawn
-- from the win probability of the static evaluation of the position (no search):
-- p = sigmoid(eval / k), the result is the sum of two draws with probability p each (win p^2,
-- draw 2 p (1 - p), loss (1 - p)^2), so that the expected result / 2 is p.
-- The score column is the static eval itself. The evaluation uses the compiled weights or the config
-- given by -c. With these labels the loss has no hidden variables: its minimum is at the weights used.
relabelFiles :: Options -> IO ()
relabelFiles opts = do
    when (null (optOutPath opts)) $ ioError $ userError "Relabel needs the output directory (-o)"
    x0 <- startVector opts
    let es    = vecToEvalState x0
        scale = optSigScale opts	-- is 1 / k
    files <- getFileList (optCsvPath opts) False (Just filteredCsv)
    createDirectoryIfMissing True (optOutPath opts)
    forM_ (zip [0 :: Int ..] files) $ \(i, inName) -> do
        let outName = makeFileName inName (optOutPath opts) ".csv"
            g0      = mkStdGen (optRelabel opts * 1000 + i)
        putStrLn $ inName ++ " --> " ++ outName
        hFlush stdout
        hi <- openFile inName  ReadMode
        ho <- openFile outName WriteMode
        (_, n) <- loopCount (relabelLine es scale hi ho) (g0, 0 :: Int)
        putStrLn $ show n ++ " records written"
        hClose hi
        hClose ho

relabelLine :: EvalState -> Double -> Handle -> Handle -> Int -> (StdGen, Int) -> IO (Bool, (StdGen, Int))
relabelLine es scale hi ho _ (g, n) = do
    end <- hIsEOF hi
    if end
       then return (False, (g, n))
       else do
           line <- hGetLine hi
           let (fen, _) = break ((==) ',') line
           if null fen
              then return (True, (g, n))
              else do
                  let pos     = posFromFen fen
                      e       = posExactEval pos es
                      p       = sigmoid (scale * fromIntegral e)
                      pd      = 2 * p * (1 - p)
                      pl      = (1 - p) * (1 - p)
                      (u, g') = random g :: (Double, StdGen)
                      r | u < pl      = 0
                        | u < pl + pd = 1
                        | otherwise   = 2 :: Int
                  hPutStrLn ho $ fen ++ "," ++ show e ++ "," ++ show r
                  return (True, (g', n + 1))

-- Export for offline fitting: every input file (*-filt.csv: fen, score, result) is written to the
-- output directory as <name>-exp.csv, with a header line and the columns:
--   fen, score, result: as in the input
--   eval:  posExactEval with the start weights (compiled or -c)
--   phase: gamePhase (0 = end game, 254 = opening), scale: end game scale factor (64 = not reduced),
--   m50:   50 moves counter
--   dmid, dend: centipawns per unit of a mid / end weight times its feature, i.e. the pseudo derivative
--          of eval to the mid / end weight i is f_i * dmid / f_i * dend (exact up to the integer shifts,
--          as long as the scale factor stays, it depends on the sign of the end game sum)
--   const: centipawns of the part independent of the weights (epMovingMid, epMovingEnd)
--   f_i:   one column per weight, the value of its feature (mid and end use the same feature)
-- So: eval ~ sum_i f_i * (wmid_i * dmid + wend_i * dend) + const
-- A new term with feature z and weights zmid, zend adds z * (zmid * dmid + zend * dend), in the same way
-- as it would do in the term sum of the engine. The start weights are written to weights.csv
exportFiles :: Options -> IO ()
exportFiles opts = do
    when (null (optOutPath opts)) $ ioError $ userError "Export needs the output directory (-o)"
    x0 <- startVector opts
    files <- getFileList (optCsvPath opts) False (Just filteredCsv)
    createDirectoryIfMissing True (optOutPath opts)
    let nw     = optDims `div` 2
        names  = map (drop 4) (take nw optSpaceNames)
        wmid   = map round $ take nw (U.toList x0) :: [Int]
        wend   = map round $ drop nw (U.toList x0) :: [Int]
        header = intercalate "," $ ["fen", "score", "result", "eval", "phase", "scale", "m50",
                                    "dmid", "dend", "const"] ++ names
        ex     = Export {
                     exState  = vecToEvalState x0,
                     exProbes = featureProbes nw,
                     exNumW   = nw,
                     exWMid   = wmid,
                     exWEnd   = wend
                 }
    withFile (optOutPath opts </> "weights.csv") WriteMode $ \fo -> do
        hPutStrLn fo "name,mid,end"
        forM_ (zip3 names wmid wend) $ \(n, m, e) -> hPutStrLn fo $ n ++ "," ++ show m ++ "," ++ show e
    forM_ files $ \inName -> do
        let outName = makeFileName inName (optOutPath opts) "-exp.csv"
        putStrLn $ inName ++ " --> " ++ outName
        hFlush stdout
        hi <- openFile inName  ReadMode
        ho <- openFile outName WriteMode
        hPutStrLn ho header
        ExportStats n bad nonlin maxd sumd <- loopCount (exportLine ex hi ho) (ExportStats 0 0 0 0 0)
        putStrLn $ show n ++ " records written, " ++ show bad ++ " lines skipped"
        putStrLn $ "Term sum not linear in the weights: " ++ show nonlin ++ " records"
        putStrLn $ "Eval - linear reconstruction (cp): max abs " ++ showFFloat (Just 3) maxd ""
                   ++ ", mean abs " ++ showFFloat (Just 3) (sumd / fromIntegral (max 1 n)) ""
        hClose hi
        hClose ho

data Export = Export {
        exState  :: EvalState,	-- eval state with the start weights
        exProbes :: [(Int, Maybe Int, EvalState)],	-- unit weight states, see featureProbes
        exNumW   :: !Int,	-- number of weights (each has a mid and an end value)
        exWMid   :: [Int],	-- start weights, mid
        exWEnd   :: [Int]	-- start weights, end
    }

-- Records, skipped lines, records where the term sum is not linear in the weights,
-- maximum and sum of the absolute difference between eval and its linear reconstruction
data ExportStats = ExportStats !Int !Int !Int !Double !Double

-- Eval states to extract the feature values from the term sum: the mid value of weight i and the
-- end value of weight j are 1, all others 0, so the term sum is MidEnd f_i f_j
-- With n weights we need (n + 1) / 2 evaluations per position
featureProbes :: Int -> [(Int, Maybe Int, EvalState)]
featureProbes nw = [ (i, mj i, vecToEvalState (unitVec i (mj i))) | i <- [0 .. h - 1] ]
    where h = (nw + 1) `div` 2
          mj i | i + h < nw = Just (i + h)
               | otherwise  = Nothing
          unitVec i mj' = U.replicate (2 * nw) 0 U.// ((i, 1) : maybe [] (\j -> [(nw + j, 1)]) mj')

positionFeatures :: Export -> MyPos -> U.Vector Int
positionFeatures ex p = U.replicate (exNumW ex) 0 U.// concatMap one (exProbes ex)
    where one (i, mj, es) = let MidEnd fi fj = evalTermSum p es
                            in (i, fi) : maybe [] (\j -> [(j, fj)]) mj

exportLine :: Export -> Handle -> Handle -> Int -> ExportStats -> IO (Bool, ExportStats)
exportLine ex hi ho _ st@(ExportStats n bad nonlin maxd sumd) = do
    end <- hIsEOF hi
    if end
       then return (False, st)
       else do
           line <- hGetLine hi
           case splitComma line of
               [fen, scs, res] | not (null fen) && not (null scs) && not (null res) -> do
                   let p     = posFromFen fen
                       es    = exState ex
                       ev    = posExactEval p es
                       gph   = gamePhase p
                       m50   = get50Moves p
                       MidEnd sm se = evalTermSum p es
                       sf    = evalEndScale p se
                       fs    = positionFeatures ex p
                       fl    = U.toList fs
                       lm    = sum (zipWith (*) (exWMid ex) fl)
                       le    = sum (zipWith (*) (exWEnd ex) fl)
                       f50   = fromIntegral (128 - m50) / 262144 :: Double	-- 2 ^ (shift2Cp + 15)
                       dmid  = fromIntegral gph * f50
                       dend  = fromIntegral (sf * (256 - gph)) * f50 / 64
                       cst   = fromIntegral (epMovingMid (esEParams es) * gph
                                             + epMovingEnd (esEParams es) * (256 - gph)) * f50
                       evlin = fromIntegral lm * dmid + fromIntegral le * dend + cst
                       d     = abs (fromIntegral ev - evlin)
                       nonl  | lm == sm && le == se = 0
                             | otherwise            = 1
                   hPutStrLn ho $ intercalate "," $
                       [fen, scs, res, show ev, show gph, show sf, show m50,
                        showFFloat (Just 9) dmid "", showFFloat (Just 9) dend "",
                        showFFloat (Just 4) cst ""] ++ map show fl
                   return (True, ExportStats (n + 1) bad (nonlin + nonl) (max maxd d) (sumd + d))
               _ -> return (True, ExportStats n (bad + 1) nonlin maxd sumd)

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
    x0 <- startVector opts
    let loss = optsToLossFunction opts
    EASimple _ l <- evaluateLoss "Train only" loss
                        [x0] (dsTrainFiles ds) (dsSampleFunc ds)
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
    x0 <- startVector opts
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
            tsCurrent = x0,
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
    writeConfig opts (configPreambTrain opts tsf) (tsNames tsf) (tsCurrent tsf)

configPreambTrain :: Options -> TrainState -> Handle -> IO ()
configPreambTrain opts ts fo = do
    hPutStrLn fo "-- This config is generated by a TuneSGD run with following parameters:"
    hPutStrLn fo $ "-- dataset " ++ optCsvPath opts ++ " with lr = " ++ show (tsLR ts)
    hPutStrLn fo $ "-- batch size = " ++ show (tsBatchSz ts) ++ " with " ++ show (tsBatches ts) ++ " batches"
    -- hPutStrLn fo $ "-- scale = " ++ show (optSigScale opts) ++ ", luft = " ++ show (optLossLuft opts)
    hPutStrLn fo $ "-- scale = " ++ show (optSigScale opts) ++ ", wscale = " ++ show (optWeiScale opts)
    when ((optTolerate opts) > 0) $
        hPutStrLn fo $ "-- drop " ++ show (optTolerate opts) ++ "% of the worse scores"

configPreambOptim :: Options -> OptimState -> Handle -> IO ()
configPreambOptim opts _ fo = do
    hPutStrLn fo "-- This config is generated by a TuneSGD optimization run with following parameters:"
    hPutStrLn fo $ "-- dataset " ++ optCsvPath opts
    hPutStrLn fo $ "-- scale = " ++ show (optSigScale opts) ++ ", wscale = " ++ show (optWeiScale opts)
    case optConFile opts of
        Just fn -> hPutStrLn fo $ "-- start values from " ++ fn
        Nothing -> return ()

writeConfig :: Options -> (Handle -> IO ()) -> [String] -> OptParams -> IO ()
writeConfig opts preamb names best = withFile (optOutPath opts) WriteMode $ \fo -> do
    preamb fo
    hPutStrLn fo "-- Generated weight assignments follow."
    hPutStrLn fo ""
    forM_ (zip names (U.toList best)) $ \(n, v) ->
        hPutStrLn fo $ n ++ " = " ++ show (round v :: Int)

-- The optimization status
data OptimState = OptimState {
        opLoss    :: Loss,	-- Fix: the loss function
        opNames   :: [String],	-- Fix: the names of the parameters to be optimized
        opDataset :: Dataset,	-- the dataset of the training
        opMax     :: Int,	-- maximum steps
        opFails   :: Int,	-- maximum fails to improve for termination
        opFixIdx  :: Int,	-- fixed weight
        opDims    :: [Int],	-- dimensions to optimize, in the order of the search
        opStart   :: OptParams,	-- start vector
        opHistory :: [(OptParams, Double, Int)]	-- training history (params, loss & fails)
    }

searchParams :: Dataset -> Options -> IO ()
searchParams ds opts = do
    x0 <- startVector opts
    dims <- if null (optParams opts)
               then return interleavedDims
               else case resolveDims (optParams opts) of
                        Right is -> return is
                        Left err -> ioError $ userError $ err ++ "\nValid names (with optional mid. or end.): "
                                        ++ unwords (map (drop 4) (take (optDims `div` 2) optSpaceNames))
    when (not $ null $ optParams opts) $ do
        putStrLn $ "Optimize " ++ show (length dims) ++ " dimensions: "
                   ++ unwords (map dimIdxToDimName dims)
        hFlush stdout
    let lossf = optsToLossFunction opts
        opi = OptimState {
            opLoss    = lossf,
            opNames   = optSpaceNames,
            opDataset = ds,
            opMax     = optOptim opts,
            opFails   = length dims,	-- to stop the search when nothing changed after one full round
            opFixIdx  = optFixIdx opts,
            opDims    = dims,
            opStart   = x0,
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
            writeConfig opts (configPreambOptim opts opf) (opNames opf) cv

optDims :: Int
optDims = length optSpaceInit

-- All dimensions in the order mid, end of the first weight, mid, end of the second, etc.
-- The mid and the end value of a weight are strongly correlated, so they should be
-- optimized one after the other
interleavedDims :: [Int]
interleavedDims = concat [ [i, i + h] | i <- [0 .. h - 1] ]
    where h = optDims `div` 2

-- The start vector: the compiled weights, or the values from the config file given by -c
-- Weights missing in the file keep their compiled value, unknown names are an error
startVector :: Options -> IO OptParams
startVector opts = case optConFile opts of
    Nothing -> return (U.fromList optSpaceInit)
    Just fn -> do
        fex <- doesFileExist fn
        when (not fex) $ ioError $ userError $ "Config file not found: " ++ fn
        cont <- readFile fn
        let ps      = fileToParams cont
            unknown = [ n | (n, _) <- ps, n `notElem` optSpaceNames ]
        when (not $ null unknown) $
            ioError $ userError $ "Unknown parameters in " ++ fn ++ ": " ++ unwords unknown
        putStrLn $ "Start vector from " ++ fn ++ " (" ++ show (length ps) ++ " values)"
        return $ U.fromList [ fromMaybe d (lookup n ps) | (n, d) <- zip optSpaceNames optSpaceInit ]

-- Resolve parameter names to indices in the optimization space
-- A name with prefix "mid." or "end." selects that dimension, a bare name selects both
resolveDims :: [String] -> Either String [Int]
resolveDims names = fmap (nub . concat) (mapM one names)
    where one n
              | isPhased n = maybe (Left $ unknown n) (\i -> Right [i]) (elemIndex n optSpaceNames)
              | otherwise  = case mapMaybe (\p -> elemIndex (p ++ n) optSpaceNames) ["mid.", "end."] of
                                 [] -> Left $ unknown n
                                 is -> Right is
          isPhased n = "mid." `isPrefixOf` n || "end." `isPrefixOf` n
          unknown n  = "Unknown parameter: " ++ n

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
            | otherwise             = ((opStart op, 1e100, 0), True)
    if since > opFails op
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
                  putStrLn $ "    " ++ dimIdxToDimName ix ++ ": " ++ show (round (best U.! ix) :: Int)
                             ++ " -> " ++ show (round (best' U.! ix) :: Int)
                  putStrLn $ show best'
                  return (True, op { opHistory = oph })
              else do
                  let oph = (best, blf, since + 1) : drop 1 (opHistory op)
                  return (True, op { opHistory = oph })
    where dims = opDims op
          ix   = dims !! ((k - 1) `mod` length dims)

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
