{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}

module Main (main) where

import Prelude
-- import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad (forM, forM_, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Char (isSpace)
import Data.List (isPrefixOf, sortBy, groupBy, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (comparing)
-- import Data.Ratio
import Data.Typeable
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.IO.Error
import System.Process
import System.Time
import System.Timeout (timeout)
import System.Locale (defaultTimeLocale)
import System.Random
import Text.Printf

debug :: Bool
debug = False

-- Some constants for the evolving environment
playersDir, gamesDir, currentDir, statusFile, statusFileN, goonFile, configFile :: String
playersDir  = "Players"
gamesDir    = "Games"
currentDir  = "Current"
statusFile  = "status.txt"
statusFileN = "status-new.txt"
goonFile    = "running"
configFile  = "config.txt"

-- Other constants
maxMaxThreads :: Int
maxMaxThreads = 20

-- Some constants for playing one match: hard coded part
noGames, tcFixTm, expLength, toFact :: Int
secPerMv :: Double
noGames   = 2
tcFixTm   = 60	-- seconds
secPerMv  = 1	-- seconds per move
expLength = 200	-- expect a maximum game length of so may moves
toFact    = 2	-- time out factor

-- Calculations with the hardcoded parameters:
parGames, parTime :: String
parGames = "-rounds " ++ show noGames
parTime  = "tc=" ++ show tcFixTm ++ "+" ++ show secPerMv
expDur :: Double
expDur  = fromIntegral tcFixTm + fromIntegral expLength * secPerMv
timeOut :: Int	-- timeout in micro seconds, as requested by async
timeOut = round $ fromIntegral toFact * fromIntegral noGames * expDur * 1000 * 1000

-- Some constants for playing one match: configurable part
cuteChessCom, otherOpts, resp :: String
cuteChessCom = "cutechess-cli"
-- Big care here: the running engine should not produce a logfile,
-- as that would give an error when starting the same engine twice in the same directory
-- otherOpts = "-pgnin swcr-20.pgn -recover -draw 150 100 -resign 4 800 -site Sixpack"
otherOpts = "-recover -draw movenumber=20 movecount=5 score=5 -resign movecount=5 score=800 -site Sixpack"
resp  = "-pgnout "

-- Simple functions to create the cutechess-cli command line parts
engSpec :: String -> String -> String -> String
engSpec epath eexe name = "-engine cmd=" ++ (epath </> eexe) ++ " name=" ++ name ++ " arg=" ++ name

each :: String -> String
each dir = "-each restart=on option.Hash=512 arg=-l arg=5 dir=" ++ dir ++ " proto=uci " ++ parTime

-- Default parameters for optimisation
popCount, qualiFrc, eliteFrc :: Int
popCount = 12	-- population count (without witness)
qualiFrc = 50	-- percent of the population which qualifies for next tournament
eliteFrc = 50	-- percent of the qualifiers which is elite (determine the new distribution)
distStep :: Double
distStep = 0.5	-- distribution changing step

-- Parameters for variation of the random generated candidates
unchangedChances :: Int
unchangedChances = 8	-- remain unchanged in 8 from 8+2 cases

type Player = String

type Event  = String

data Result = ToPlay
            | Playing
            | Done (Int, Int, Int)
            deriving (Show, Read)

type ResReturn = Maybe (Int, Int, Int)

data Pairing = Pairing {
                  pair   :: (Int, Int),
                  result :: Result
               } deriving (Show, Read)

type Vec = [Double]		-- Vector in list form

data Distrib = Distrib {
                   distDim :: Int,
                   distPNs :: [String],
                   distMin, distMax :: Vec,
                   distAvg, distVar :: Vec
               } deriving (Show, Read)

data Tournament
    = Tournament {
        event   :: Event,
        players :: [Player],
        games   :: [Pairing]
      } deriving (Show, Read)

data Phase = Initialize | Prepare | Play deriving (Show, Read)

data EvolvePersistentState
    = Pers {
        evName     :: String,		-- evolve name
        evPopCount :: Int,		-- population count (without witness)
        evPhase    :: Phase,		-- current phase
        evDistrib  :: Distrib,		-- current distribution
        evCycle    :: Int,		-- current cycle (number of tournaments begun)
        evPParams  :: [(Player, Vec)],	-- current player parameters
        evActSucc  :: [(Player, Rational)],	-- points of active players
        evCurTour  :: Maybe Tournament,	-- current (or last) tournament
        evWitness  :: Maybe Player,	-- witness player
        evWitSucc  :: [Rational]	-- points of the witness over time (reverse)
      } deriving (Show, Read)

data EvolveState
    = EvSt {
        stConfig   :: Config,			-- the current config
        stPers     :: EvolvePersistentState,	-- the persistent state
        stAsync    :: Map (Async ResReturn) (Int, Int),	-- list from asynchronous actions pair
        stMaxThr   :: Int,			-- maximum number of running games
        stCurThr   :: Int 			-- current number of running games
      }

initEvSt :: Config -> EvolvePersistentState -> Int -> EvolveState
initEvSt cf pstate mxt = EvSt { stConfig = cf, stPers = pstate, stAsync = M.empty, stMaxThr = mxt, stCurThr = 0 }

type Config = [(String, String)]

-- The global optimisation method used here is an adapted cross entropy method
-- where the new samples are added to the best samples of the previous step
-- This makes sense only when the comparison of the samples is not deterministic
-- which is the case when comparing them by playing tournaments (the results
-- of the games are not deterministic)

-- To start a new evolve give directory and a name
-- To continue an interrupted evolve, just give the directory (default: pwd)
-- The directory must always exist and must contain a config.txt file
main :: IO ()
main = do
    args  <- getArgs
    evDir <- if null args then getCurrentDirectory else return (head args)
    -- kern <- getNumCapabilities
    let evn = if length args > 1 then Just (args!!1) else Nothing
    setCurrentDirectory evDir
    cf <- readConfigFile
    let mxt = getConfigVal cf "threads" (Just 1)
    pstate <- initState cf evDir evn
    writeFile goonFile $ show mxt
    _ <- runStateT evolveOrStop $ initEvSt cf pstate mxt
    return ()

initState :: Config -> FilePath -> Maybe String -> IO EvolvePersistentState
initState cf dir mname
    | Just evname <- mname = do
         fex <- doesFileExist statusFile
         if fex
            then fail $ "There is already a status file in " ++ dir ++ ", abort!"
            else do
                putStrLn $ "Initialize new evolve " ++ evname ++ " in " ++ dir
                initNewState cf evname
    | otherwise = initOldState

-- Initialize a new evolve, return state
-- If a status file already exists, we fail (means: this is not really new)
initNewState :: Config -> String -> IO EvolvePersistentState
initNewState cf evname = do
    createDirectory playersDir
    createDirectory gamesDir
    createDirectory currentDir
    let initDist = makeInitDist cf	-- has to be done from the optim params
        evs = Pers { evName = evname, evPopCount = getConfigVal cf "popCount" (Just popCount),
                     evCycle = 0, evPhase = Initialize,
                     evDistrib = initDist, evPParams = [], evCurTour = Nothing,
                     evWitness = Nothing, evWitSucc = [], evActSucc = []
                   }
    saveState evs
    return evs

-- Initialize an old evolve (status from file)
-- We have to find a method to combine the information from the status file
-- with the one from the config file, as we wish to be able to change
-- some of the config parameters (which ones?)
initOldState :: IO EvolvePersistentState
initOldState = do
    sf <- readFile statusFile
    return $ correctTour $ read sf

-- When reading the persistent state from file, all the games which were
-- "playing" must be transformed in "to play", because at that moment
-- no thread is running
correctTour :: EvolvePersistentState -> EvolvePersistentState
correctTour eps = case evCurTour eps of
    Nothing  -> eps
    Just trn -> eps { evCurTour = Just $ noPlaying trn }
    where noPlaying t = t { games = map f (games t) }
          f (Pairing p Playing) = Pairing p ToPlay
          f x                   = x

-- We control the number of threads as a number written in the "running" file
howManyThreads :: IO Int
howManyThreads = do
    ifc <- threadsFromFile `catch` retZero	-- should check what kind of exception
    -- kern <- getNumCapabilities
    -- return $ max 0 $ min ifc kern	-- give some reasonable limits
    return $ max 0 $ min ifc maxMaxThreads	-- give some reasonable limits
    where retZero :: SomeException -> IO Int
          retZero = \_ -> return 0

threadsFromFile :: IO Int
threadsFromFile = readFile goonFile >>= return . read . head . lines

-- Generate an initial distribution, with gaussian in every component (parameter to optimize)
-- The mean is in the middle of the parameter range from config
-- The standard deviation is the half of the parameter range from config
makeInitDist :: Config -> Distrib
makeInitDist cf = Distrib { distDim = n, distPNs = pars, distMin = mins,
                            distMax = maxs, distAvg = avgs, distVar = vars }
    where parcf = map paramParse $ filter (isPrefixOf "param." . fst) cf
          pars  = map fst parcf
          n     = length pars
          mins  = map (fst . snd) parcf
          maxs  = map (snd . snd) parcf
          avgs  = zipWith (\x y -> (x + y) / 2) mins maxs
          vars  = zipWith (\m a -> m - a) maxs avgs

paramParse :: (String, String) -> (String, (Double, Double))
paramParse (pn, vp)
    | Just okpn     <- stripPrefix "param." pn,
      (mins, ',':maxs) <- break (== ',') vp,
      (vmin, ""):[] <- reads mins,
      (vmax, ""):[] <- reads maxs,
      vmin < vmax  = (okpn, (vmin, vmax))
    | otherwise    = error $ "Parameter " ++ pn ++ " must have min & max values delimited by ',':\n"
                             ++ vp

getPersState :: StateT EvolveState IO EvolvePersistentState
getPersState = gets stPers

putPersState :: EvolvePersistentState -> StateT EvolveState IO ()
putPersState p = modify $ \s -> s { stPers = p }

-- The gracefully exit is by deleting the witness file
-- (which is named "running"), after which at the next loop
-- the program will terminate
-- To change the number of threads, echo the new number to the file:
-- echo 2 > running
-- It takes some time (seconds to minutes) until the reaction to
-- these changes is visible
evolveOrStop :: StateT EvolveState IO ()
evolveOrStop = do
    goon <- lift $ howManyThreads
    evst <- get
    when (goon /= stMaxThr evst) $ lift
        $ putStrLn $ "Max threads set to " ++ show goon
                   ++ ", currently " ++ show (stCurThr evst) ++ " threads running"
    if goon > 0 || stCurThr evst > 0
       then evolveState goon >> gets stPers >>= \s -> lift (saveState s) >> evolveOrStop
       else lift $ putStrLn "Exiting"

evolveState :: Int -> StateT EvolveState IO ()
evolveState goon = do
    phase <- gets $ evPhase . stPers
    case phase of
        Initialize -> stateInitial
        Prepare    -> statePrepare
        Play       -> statePlay goon

stateInitial :: StateT EvolveState IO ()
stateInitial = do
    est <- getPersState
    lift $ putStrLn $ "Initialising new evolve " ++ evName est
    newVecs <- lift $ genCandidates (evDistrib est) (evPopCount est)
    let cyc    = evCycle est + 1
        candps = nameCandidates (evName est) cyc newVecs
        cands  = map fst candps
        scc    = zip cands $ repeat 0
        tour   = makeTournament (evName est ++ "-" ++ show cyc) cands
    lift $ writeCandidates (distPNs $ evDistrib est) candps
    putPersState est {
           evPhase = Play, evCycle = cyc, evPParams = candps, evCurTour = Just tour, evActSucc = scc
        }

statePrepare :: StateT EvolveState IO ()
statePrepare = do
    est <- getPersState
    cf  <- gets stConfig
    lift $ putStrLn $ "Preparing new run for evolve " ++ evName est
    let cyc = evCycle est + 1
        ltour  = fromJust $ evCurTour est
        evalt  = evalTournament ltour
        selecs = reverse . sortBy (comparing snd)
                     $ makeSelectionBase (evWitness est) evalt (evActSucc est)
        scc    = map (\(p, (s1, s2)) -> (p, s1 + s2)) selecs
        qquota = getQuota (getConfigVal cf "qualiFrc" (Just qualiFrc)) (evPopCount est)
        equota = getQuota (getConfigVal cf "eliteFrc" (Just eliteFrc)) qquota
        (good, weak) = splitAt qquota $ map fst scc
        elite  = take equota good
        pars   = filter (not . flip elem weak . fst) (evPParams est)
        dist   = newDist (evDistrib est) $ mapMaybe (getVect pars) elite
        miss   = evPopCount est - length good
    lift $ do
        writeTourTop (event ltour ++ "-top.txt") selecs
        writeTourRes (event ltour ++ "-res.txt") (games ltour) (players ltour)
        putStr "Elite:"
        forM_ elite $ \e -> putStr (" " ++ e)
        putStrLn ""
    newVecs <- lift $ genCandidates dist miss
    let ncandps = nameCandidates (evName est) cyc newVecs
        ncands  = map fst ncandps
        nscc    = zip ncands $ repeat 0
        tour    = makeTournament (evName est ++ "-" ++ show cyc) $ ncands ++ good
    -- move all weak players to the player directory
    lift $ do
        forM_ weak $ \p -> renameFile (currentDir </> p) (playersDir </> p)
        writeCandidates (distPNs dist) ncandps
    putPersState est {
           evPhase = Play, evDistrib = dist, evCycle = cyc, evPParams = pars ++ ncandps,
           evCurTour = Just tour, evActSucc = scc ++ nscc
        }

getVect :: [(Player, Vec)] -> Player -> Maybe Vec
getVect = flip lookup

getQuota :: Int -> Int -> Int
getQuota q k = max 2 $ (k * q + 50) `div` 100

statePlay :: Int -> StateT EvolveState IO ()
statePlay goon = do
    currt <- gets stMaxThr
    when (goon /= currt) $ modify $ \s -> s { stMaxThr = goon }
    est <- getPersState
    (done, mtrn) <- tourStep $ fromJust $ evCurTour est
    if done then putPersState est { evPhase = Prepare }
            else case mtrn of
                Just _ -> putPersState est { evCurTour = mtrn }
                _      -> return ()

-- Give names to the candidates
nameCandidates :: String -> Int -> [Vec] -> [(String, Vec)]
nameCandidates evn cyc = zip (map label [1..])
    where label i = evn ++ "-" ++ show cyc ++ "-" ++ show i ++ ".txt"

writeCandidates :: [String] -> [(String, Vec)] -> IO ()
writeCandidates pnames cs = do
    ctime <- getClockTime >>= toCalendarTime >>= return . calendarTimeToString
    mapM_ (writeCandidate pnames ctime) cs

-- Write the candidate configuration to a file
writeCandidate :: [String] -> String -> (String, Vec) -> IO ()
writeCandidate pnames ctime (name, vec) = writeFile (currentDir </> name) (showConfig pnames vec comm)
    where comm = "-- Candidate " ++ name ++ " created on " ++ ctime

-- Consider the older successes of the players and eventually remove the witness
makeSelectionBase :: Maybe Player -> [(Player, Rational)] -> [(Player, Rational)]
                  -> [(Player, (Rational, Rational))]
makeSelectionBase mwit ordl olds = remWitn $ map addOlds ordl
    where addOlds (p, s) = case lookup p olds of Just os -> (p, (s, os)); Nothing -> (p, (s, 0))
          remWitn sbs = case mwit of Just p -> filter ((/= p) . fst) sbs; Nothing -> sbs

-- Saving the status file to disk in order to recover, if necessary
-- To be more robust, write to a new file and then renaming
saveState :: EvolvePersistentState -> IO ()
saveState st = do
    writeFile statusFileN $ show st
    renameFile statusFileN statusFile

writeTourTop :: String -> [(String, (Rational, Rational))] -> IO ()
writeTourTop fil selecs = do
    let info = unlines
             $ map (\(i, (pl, (pt, pg)))
                      -> printf "%2d. %-30s %5.1g %7.1g %5.1g" i pl (f pt) (f pg) (p pt))
             $ zip places selecs
    writeFile (gamesDir </> fil) info
    where places = [1..] :: [Int]
          f :: Rational -> Double
          -- f r = fromIntegral (numerator r) / fromIntegral (denominator r)
          f r = fromRational r
          p :: Rational -> Double
          p r = fromRational r * 100 / maxp
          maxp = fromIntegral $ noGames * (length selecs - 1)

writeTourRes :: String -> [Pairing] -> [String] -> IO ()
writeTourRes fil gs ps = do
    let info = unlines
             $ map (\(a, b, w, l, d)
                      -> printf "%-50s %2d wone, %2d lost, %2d draw" (a ++ " - " ++ b) w l d)
             $ map toTuple
             $ filter gameDone	-- just to be sure...
             $ sortBy (comparing pair) gs
    writeFile (gamesDir </> fil) info
    where toTuple (Pairing (a, b) (Done (w, l, d))) = (ps!!a, ps!!b, w, l, d)
          toTuple _                                 = error "in toTuple"
          gameDone (Pairing _ (Done _)) = True
          gameDone _                    = False

eventName :: Event -> String
eventName = id

-- eventDir :: Event -> String
-- eventDir = (outDir </>) . eventName

oneMatch :: Config -> Event -> Bool -> Player -> Player -> IO (Int, Int, Int)
oneMatch cf ev pgn p1 p2 = do
    cdir <- getCurrentDirectory
    let edir = cdir </> currentDir
        evname = eventName ev
        pfname = evname ++ ".pgn"
        pfil = cdir </> gamesDir </> pfname
        bdir = getConfigStr cf "engineDir" Nothing
        eexe = getConfigStr cf "engineExe" Nothing
        args1 = concatMap words [
                  parGames,
                  otherOpts,
                  "-event", evname,
                  each edir,
                  engSpec bdir eexe p1,
                  engSpec bdir eexe p2
               ]
        args2 = concatMap words [ resp, pfil ]
        args = if pgn then args1 ++ args2 else args1
        ccDir = getConfigStr cf "cuteChessDir" Nothing
        ccCom = getConfigStr cf "cuteChessCom" (Just cuteChessCom)
        cccli = ccDir </> ccCom
    when debug $ putStrLn $ "Start: " ++ unwords (cccli : args)
    (_, Just hout, _, ph)
            <- createProcess (proc cccli args) { std_out = CreatePipe, cwd = Just ccDir }
    catch (everyLine hout (0, 0, 0) noGames) $ \e -> do
        let es = ioeGetErrorString e
        putStrLn $ "Error in everyLine: " ++ es
        terminateProcess ph
        throwIO e

everyLine :: Handle -> (Int, Int, Int) -> Int -> IO (Int, Int, Int)
everyLine _ r 0 = return r
everyLine h r g = do
    lin <- hGetLine h
    when debug $ putStrLn $ "Got: " ++ lin
    let (r1, g1) = if "Score of" `isPrefixOf` lin
                      then (getScore lin, g-1)
                      else (r, g)
    everyLine h r1 g1

-- The line has the following structure:
-- Score of x vs y: a - b - c [prc] n
-- where x and y are the opponents, a = wins of x, b = wins of y, c = draws
getScore :: String -> (Int, Int, Int)
getScore
    = listToTrio
    . map (read . snd)
    . filter (even . fst)
    . zip [0..]
    . take 5
    . drop 5
    . words

listToTrio :: [Int] -> (Int, Int, Int)
listToTrio (x:y:z:_) = (x, y, z)
listToTrio s         = error $ "Not a trio: " ++ show s

pairings :: [a] -> [(a,a)]
pairings [] = []
pairings (a:as) = zip (repeat a) as ++ pairings as

alternate :: Bool -> [(a,a)] -> [(a,a)]
alternate _ [] = []
alternate False (xy : xys) = xy : alternate True xys
alternate True ((x, y) : xys) = (y, x) : alternate False xys

makeTournament :: Event -> [Player] -> Tournament
makeTournament ev ps = Tournament { event = ev, players = ps, games = prs }
    where prs = map (\xy -> Pairing xy ToPlay) $ alternate False $ pairings [0..n-1]
          n = length ps

categorize :: [Pairing] -> ([Pairing], [Pairing], [Pairing])
categorize = go ([], [], [])
    where go a [] = a
          go (d, p, w) (g:gs)
              | ToPlay  <- result g = go (d, p, g:w) gs
              | Playing <- result g = go (d, g:p, w) gs
              | otherwise           = go (g:d, p, w) gs

findGame :: Int -> Int -> [Pairing] -> Maybe (Pairing, [Pairing])
findGame i j = go []
    where go _  [] = Nothing
          go as (g@(Pairing (k, l) _) : gs)
              | k == i && l == j = Just (g, as ++ gs)
              | otherwise        = go (g:as) gs

-- Exception to throw when an unexpected pairing finishes
data GameNotFoundException = GameNotFoundException deriving (Show, Typeable)
instance Exception GameNotFoundException

tourStep :: Tournament -> StateT EvolveState IO (Bool, Maybe Tournament)
tourStep trn
    | null playing && null waiting = return (True, Nothing)
    | otherwise = do
        evst <- get
        -- Start as many games as alowed
        let tostart = stMaxThr evst - stCurThr evst
        -- lift $ putStrLn $ "Max " ++ show (stMaxThr evst) ++ " crt " ++ show (stCurThr evst)
        --                     ++ " to start " ++ show tostart
        asyncs' <- lift $ startNewGames (stConfig evst) (event trn) tostart waiting (players trn)
        let (playing'', waiting') = splitAt tostart waiting
            playing' = map nowPlaying playing''
            trn' = trn { games = done ++ playing ++ waiting' ++ playing' }
            started = length playing'
            asyncs = stAsync evst `M.union` asyncs'
        -- lift $ putStrLn $ "Playing list: " ++ show (playing ++ playing')
        -- wait for one of the games to finish
        (a, eir) <- lift $ waitAnyCatch $ M.keys asyncs
        let mij = M.lookup a asyncs	-- which game finnished?
        case mij of
            Nothing -> do
                lift $ putStrLn $ "Async not found in map, ignored"
                put evst { stCurThr = stCurThr evst + started,
                           stAsync = M.delete a asyncs }
                return (False, Just trn')
            Just (i, j) -> do
                let pl1 = players trn !! i
                    pl2 = players trn !! j
                    mfg = findGame i j $ playing ++ playing'
                -- lift $ putStrLn $ "Received " ++ show i ++ " / " ++ show j
                case mfg of
                    Nothing -> lift $ do
                        putStrLn $ "Pair " ++ show i ++ ", " ++ show j ++ " not playing"
                        throwIO GameNotFoundException
                    Just (game, stillplaying) -> do
                        -- lift $ putStrLn $ "Game: " ++ show game
                        -- lift $ putStrLn $ "Still: " ++ show stillplaying
                        let rest = done ++ stillplaying ++ waiting'
                        ngame <- case eir of
                            Left  e    -> do
                               lift $ putStrLn $ "async ended with exception: " ++ show e
                               return game { result = ToPlay }
                            Right mrez -> do
                               lift $ putStrLn $ "Match " ++ pl1 ++ " against " ++ pl2
                                          ++ " ended: " ++ show mrez
                               case mrez of
                                   Nothing -> return game { result = ToPlay }
                                   Just rz -> return game { result = Done rz }
                        put evst { stCurThr = stCurThr evst + started - 1,
                                   stAsync = M.delete a asyncs }
                        return (False, Just trn' { games = ngame : rest })
    where (done, playing, waiting) = categorize $ games trn
          nowPlaying g = g { result = Playing }

startNewGames :: Config -> Event -> Int -> [Pairing] -> [Player]
              -> IO (M.Map (Async ResReturn) (Int, Int))
startNewGames cf ev n ps pls = do
    as <- forM (take n ps) $ \(Pairing (i, j) _) -> do
        let pl1 = pls !! i
            pl2 = pls !! j
            pgn = i == 6 || j == 6
        stime <- getClockTime >>= toCalendarTime
                              >>= return . formatCalendarTime defaultTimeLocale "%H:%M:%S"
        putStrLn $ stime ++ ": playing " ++ pl1 ++ " against " ++ pl2
        -- The timeout is in microseconds, which is bad for longer matches,
        -- because we reach the Int max bound! So we trust on cutechess_cli
        -- that it will not hang...
        -- New: with 64 bit we don't have this problem anymore
        a <- async $ (timeout timeOut $ oneMatch cf ev pgn pl1 pl2) `catch` retNothing
        -- a <- async $ (oneMatch ev pgn pl1 pl2 >>= return . Just) `catch` retNothing
        return (a, (i, j))
    return $ M.fromList as
    where retNothing :: SomeException -> IO (Maybe a)
          retNothing = \_ -> return Nothing

evalTournament :: Tournament -> [(Player, Rational)]
evalTournament trn
    = map inReal
    . reverse
    . sortBy (comparing snd)
    . map fact
    . groupBy samePlayer
    . sortBy (comparing fst)
    . concatMap toPoints $ games trn
    where samePlayer (i, _) (j, _) = i == j
          fact as = (fst $ head as, sum $ map snd as)
          inReal (i, s) = (players trn !! i, fromIntegral s / 2)

toPoints :: Pairing -> [(Int, Int)]
toPoints (Pairing { pair = (i, j), result = Done (w, l, r) })
    = [(i, 2*w + r), (j, 2*l + r)]
toPoints _ = []

showConfig :: [String] -> Vec -> String -> String
showConfig pnames cnf comm = comm ++ "\n" ++ lins
    where lins = unlines $ map (\(n, v) -> n ++ " = " ++ show v) $ zip pnames cnf

-- Generate new candidates from the distribution
genCandidates :: Distrib -> Int -> IO [Vec]
genCandidates dist n = sequence $ take n $ repeat $ genOneCand dist

-- Generating one candidate given the means and variations of the parameters
-- We must generate gaussian random numbers around the means with the corresponding sigma
-- and then bring them in limits (a simple minimum/maximum operation)
-- We then round each value (as the actual parameters will be the integer ones)
-- and random variate some of them with +1 or -1 (with low probability)
genOneCand :: Distrib -> IO Vec
genOneCand dist = do
    pars  <- mapM (uncurry fitNormal) $ zip (distAvg dist) $ map sqrt (distVar dist)
    pars' <- mapM variate pars
    return $ map rounding $ zipWith min (distMax dist) $ zipWith max (distMin dist) pars'
    where rounding x = let y = round x :: Int in fromIntegral y

uniform :: IO Double
uniform = getStdRandom (randomR (-1, 1))

variate :: Double -> IO Double
variate x = do
    r <- getStdRandom (randomR (-1, unchangedChances))
    case r of
        -1 -> return $ x - 1
        0  -> return $ x + 1
        _  -> return x

-- Marsaglia polar method for normal standard distribution:
genNormal :: IO Double
genNormal = do
    u <- uniform
    v <- uniform
    let s = u * u + v * v
        s2s = sqrt $ (-2) * log s / s
        x = u * s2s
        -- y = v * s2s
    if s >= 1 then genNormal else return x

-- Translate a normal standard distribution to a different mean and standard deviation
fitNormal :: Double -> Double -> IO Double
fitNormal mu sigma = genNormal >>= \x -> case mu + sigma * x of y -> return y

-- Calculate the new distribution based on the elite samples
newDist :: Distrib -> [Vec] -> Distrib
newDist odist vs = odist { distAvg = nmeans, distVar = nvars }
    where means = map ( / fln)  $ foldr (zipWith (+)) zero vs
          vars  = map ( / fln1) $ foldr (zipWith (+) . zipWith (\x y -> let d = x - y in d * d) means) zero vs
          nmeans = shiftTo (distAvg odist) means
          nvars  = shiftTo (distVar odist)  vars
          fln  = fromIntegral $ length vs
          fln1 = fln - 1
          zero = replicate (distDim odist) 0

shiftTo :: Vec -> Vec -> Vec
shiftTo old new = zipWith (+) (map (* ost) old) (map (* distStep) new)
    where ost = 1 - distStep

-- Convert the content of a file with assignments par=val (one per line)
-- and possibly some comments (Haskell style) into pairs (name, value)
-- Name & Value are both strings
fileToLookup :: String -> [(String, String)]
fileToLookup = catMaybes . map readSParam . nocomments . lines
    where nocomments = filter (not . iscomment)
          iscomment [] = True
          iscomment ('-':'-':_) = True
          iscomment (c:cs) | isSpace c = iscomment cs
          iscomment _ = False

readConfigFile :: IO Config
readConfigFile = readFile configFile >>= return . fileToLookup

-- Param names should not contain spaces (will be deleted)
-- Values can't begin with spaces (any space prefix will be deleted)
readSParam :: String -> Maybe (String, String)
readSParam s = let (ns, vs) = span (/= '=') s
               in case vs of
                      ('=' : rs) -> Just (strip ns, dropWhile isSpace rs)
                      _          -> Nothing	-- did not contain '='
    where strip = filter (not . isSpace)

getConfigStr :: Config -> String -> Maybe String -> String
getConfigStr cf key mdef = s
    where s = case lookup ckey cf of
                  Nothing -> case mdef of
                                 Nothing -> error $ ckey ++ " not found in " ++ configFile
                                 Just s' -> s'
                  Just s' -> s'
          ckey = "config." ++ key

getConfigVal :: Read a => Config -> String -> Maybe a -> a
getConfigVal cf key mdef
    = case lookup ckey cf of
          Nothing -> case mdef of
                         Nothing -> error $ ckey ++ " not found in " ++ configFile
                         Just v  -> v
          Just s  -> case reads s of
                         (v, ""):[] -> v
                         _          -> error $ "Can't read " ++ ckey ++ " " ++ s ++ ", wrong type"
    where ckey = "config." ++ key
