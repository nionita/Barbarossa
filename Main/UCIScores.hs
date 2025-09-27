{-# LANGUAGE PatternGuards #-}

module Main (main) where

-- We have input files with one fen per line
-- We want to analyse every fen of every file with an UCI engine at a given depth.
--
-- The purpose is to generate training files based on scores from the analysis.
--
-- All the .fen Files in the given directory will be analysed and .csv files will be created,
-- which contain per line the fen and the score, separated by ','
-- If the .csv file exists, the corresponding input will ge skipped
--

import Control.Monad (when)
import Data.Foldable (forM_)
import Data.List (isPrefixOf)
import Data.Time.Clock
import System.Console.GetOpt
import System.Directory (
           setCurrentDirectory, getCurrentDirectory, doesFileExist, doesDirectoryExist, listDirectory
       )
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.Process

debug :: Bool
debug = False

data Options = Options {
         optEngine :: String,
         optCwd    :: String,
         optDepth  :: Int,
         optMaxSc  :: Int
     } deriving Show

defaultOptions :: Options
defaultOptions = Options {
        optEngine = "",
        optCwd    = ".",
        optDepth  = 8,
        optMaxSc  = 19000
    }

setEngdir :: String -> Options -> Options
setEngdir s opt = opt { optEngine = s }

setCwd :: String -> Options -> Options
setCwd s opt = opt { optCwd = s }

setDepth :: String -> Options -> Options
setDepth s opt = opt { optDepth = read s }

setMaxSc :: String -> Options -> Options
setMaxSc s opt = opt { optMaxSc = read s }

options :: [OptDescr (Options -> Options)]
options = [
        Option "e" ["engine"] (ReqArg setEngdir "STRING") "Engine file",
        Option "c" ["chdir"]  (ReqArg setCwd    "STRING") "Working directory",
        Option "d" ["depth"]  (ReqArg setDepth  "INTEGER") "Analyse depth",
        Option "s" ["score"]  (ReqArg setMaxSc  "INTEGER") "Max score permitted"
    ]

usage :: String
usage = "Usage: UCIScores [-e engine] [-c chdir] [-d depth] input"

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo usage options))

main :: IO ()
main = do
    aopts <- theOptions
    when debug $ putStrLn $ "Options: " ++ show aopts
    let (opts, rest) = aopts
    setCurrentDirectory $ optCwd opts
    if length rest == 1 
       then do
           let input = head rest
           dex <- doesDirectoryExist input
           if dex
              then allFilesInDir opts input
              else oneFile opts input
       else if length rest == 0
               then allFilesInDir opts "."
               else do
                   putStrLn $ "Analyse list of files, skip already done"
                   cd <- getCurrentDirectory
                   putStrLn $ "To stop: touch a file with name 'stop' under " ++ cd
                   doFileList opts rest

oneFile :: Options -> String -> IO ()
oneFile opts input = do
    let output = replaceExtension input "csv"
    fex <- doesFileExist output
    if fex
       then do
           putStrLn $ "File " ++ output ++ " already exists, skip input " ++ input
           hFlush stdout
       else do
           cd <- getCurrentDirectory
           putStrLn $ "Cwd:  " ++ cd
           putStrLn $ "Analyse with " ++ (optEngine opts) ++ " at depth " ++ show (optDepth opts)
           putStrLn $ "Input:  " ++ input ++ ", output: " ++ output
           hFlush stdout
           content <- readFile input
           withFile output WriteMode $ \hout -> oneProc opts content hout

allFilesInDir :: Options -> String -> IO ()
allFilesInDir opts wdir = do
    cd <- getCurrentDirectory
    putStrLn $ "Analyse with " ++ optEngine opts ++ " at depth " ++ show (optDepth opts)
    putStrLn $ "All files in " ++ wdir
    putStrLn $ "To stop: touch a file with name 'stop' under " ++ cd
    hFlush stdout
    fenfiles <- filter ((".fen" ==) . takeExtension) <$> listDirectory wdir
    doFileList opts $ map (wdir </> ) fenfiles

doFileList :: Options -> [String] -> IO ()
doFileList opts = go
    where go []           = return ()
          go (file:files) = do
             fex <- doesFileExist "stop"
             if fex
                then putStrLn "Stopped by existence of the stop file"
                else do
                    -- let resFile = addExtension file ".epd"
                    -- rex <- doesFileExist resFile
                    -- when (not rex) $ oneFile opts file
                    oneFile opts file
                    go files

oneProc :: Options -> String -> Handle -> IO ()
oneProc opts content houtfile = do
    (hin, hout, _, _ph) <- runInteractiveProcess (optEngine opts) [] (Just (optCwd opts)) Nothing
    hSetBuffering hin LineBuffering
    hPutStrLn hin "uci"
    _ <- lineUntil hout ("uciok" `isPrefixOf`)
    startTime <- getCurrentTime
    let numlines | debug     = [1..20::Int]
                 | otherwise = [1..]
    forM_ (zip numlines (lines content)) $ \(i, line) -> do
        when debug $ putStrLn $ "*** Start fen " ++ line
        hPutStrLn hin $ "position fen " ++ line
        hPutStrLn hin $ "go depth " ++ show (optDepth opts)
        scoreAccum <- accumLines hout lastLineForPosition getScoreAccum saInit
        -- We skip stalemate and found mate positions, as they are not suited for eval training
        when (saCurrDepth scoreAccum > 0 && abs (saCurrScore scoreAccum) < optMaxSc opts)
            $ hPutStrLn houtfile $ encodeLine line (saCurrScore scoreAccum)
        when debug $ putStrLn $
            "*** Fen rezult: depth = " ++ show (saCurrDepth scoreAccum)
            ++ ", score = " ++ show (saCurrScore scoreAccum)
        when (i `mod` 100000 == 0) $ do
            crtTime <- getCurrentTime
            let diff = nominalDiffTimeToSeconds $ diffUTCTime crtTime startTime
                lps  = (round $ fromIntegral i / diff) :: Int
            putStrLn $ show i ++ " fens, " ++ show lps ++ " fens per second"
            hFlush stdout
    hPutStrLn hin "quit"

-- Normally we would get a bestmove from the engine and that would mark the end of the analysis
-- But Barbarossa has a problem with stale mate positions: it will not provide a best move
-- (Stockfish seems to provide a kind of null move)
-- In order to detect this situation, use the second condition for termination
lastLineForPosition :: String -> Bool
lastLineForPosition line = ("bestmove " `isPrefixOf` line) || ("info string empty pv" `isPrefixOf` line)

encodeLine :: String -> Int -> String
encodeLine fen score = fen ++ "," ++ show score

-- We want to collect the last score of the position
-- The source is a line containing depth & score, like:
-- info ... depth d ... score s ...
-- but depth could be repeated a few times, so only the last score for some depth has to be retained
data ScoreAccum = ScoreAccum { saCurrDepth :: Int, saCurrScore :: Int }

saInit :: ScoreAccum
saInit = ScoreAccum { saCurrDepth = 0, saCurrScore = 0 }

-- Add a new score information to the score accumulation
-- If depth repeated: just take the last score
-- otherwise: record the score of the last depth and begin a new one
addScore :: Int -> Int -> ScoreAccum -> ScoreAccum
addScore d s sa
    | d == saCurrDepth sa = sa { saCurrScore = s }
    | otherwise           = ScoreAccum { saCurrDepth = d, saCurrScore = s }

-- errorHandling = do
--     r <- catch (runFen hin hout (optDepth opts)) $ \e -> do
--         let es = ioeGetErrorString e
--         putStrLn $ "Error in everyLine: " ++ es
--         terminateProcess ph
--         throwIO e
--     putStrLn $ engine ++ ": done, with " ++ show r

-- Functions used to parse the UCI answers
--
-- lineUntil consumes engine lines until one with the given termination condition is found
-- The termination line is returned
-- h is the input file handle
-- p is the the termination predicate
lineUntil :: Handle -> (String -> Bool) -> IO String
lineUntil h p = go
    where go = do l <- hGetLine h
                  when debug $ putStrLn $ "lineUntil: " ++ l
                  if p l then return l
                         else go

-- Accumulates information from lines until one line with the termination condition is found
-- It returns the accumulated information
-- h is the input file handle
-- p is the the termination predicate
-- f is the information accumulation function: given the line and the
-- previous information, calculate the new information
accumLines :: Handle -> (String -> Bool) -> (String -> a -> a) -> a -> IO a
accumLines h p f = go
    where go a = do
             l <- hGetLine h
             when debug $ putStrLn $ "accumLines: " ++ l
             if p l then return a
                    else go $! f l a

getScoreAccum :: String -> ScoreAccum -> ScoreAccum
getScoreAccum l old
    | "info " `isPrefixOf` l
      && "score" `elem` ws
      && "depth" `elem` ws
      && hasPV ws = let s = getScore ws
                        d = getDepth ws
                    in addScore d s old
    | otherwise   = old
    where ws = words l

-- Gets the score - it can be of type "cp" or "mate"
getScore :: [String] -> Int
getScore ws
    | ("score":tp:s:_) <- dropWhile ((/=) "score") ws = readScore tp s
    | otherwise                                       = error "getScore: malformed score sequence"

readScore :: String -> String -> Int
readScore tp s
    | tp == "cp" =  k
    | k > 0      =  20000 - k
    | otherwise  = -20000 - k -- >= -20000, as k is negative (or 0?)
    where k = read s

-- Gets the depth
getDepth :: [String] -> Int
getDepth ws
    | ("depth":sd:_) <- dropWhile ((/=) "depth") ws = read sd
    | otherwise                                     = error "getDepth without depth word"

-- Gets the PV - just to be sure we got a PV, which is not the case for stale mate
hasPV :: [String] -> Bool
hasPV ws | ("pv":pv) <- dropWhile ((/=) "pv") ws = not $ null pv
         | otherwise                             = False
