{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where
import Control.Monad (when, forM_)
-- import Control.Monad.Reader
-- import Control.Monad.State
-- import Control.Concurrent
-- import Control.Applicative ((<$>))
-- import Data.List (intersperse, delete, isPrefixOf, stripPrefix, foldl')
import Data.Bits
import Data.List (intercalate, foldl')
import Data.List.Split (splitOn)
-- import Data.Maybe (catMaybes)
-- import Data.Monoid
-- import Data.ByteString (ByteString(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Serialize as S
import Data.Serialize.Get
-- import Network
-- import Numeric (showHex)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.FilePath
import System.Directory
import System.IO
-- import System.Time

import Struct.Struct
import Struct.Status
-- import Struct.Context
import Struct.Config
-- import Hash.TransTab
-- import Moves.BaseTypes
-- import Search.AlbetaTypes
-- import Moves.Base
import Moves.Fen (fenFromString)
-- import Moves.Board (posFromFen, initPos)
-- import Moves.History
import Moves.Notation
-- import Search.CStateMonad (runCState)
import Eval.FileParams (makeEvalState)
import Eval.Eval (posEval, posEvalSpec)
import Moves.Fen (posFromFen)

debug :: Bool
debug = False

data Options = Options {
        optConfFile :: Maybe String,	-- config file
        optMinWidth    :: Double,	-- luft min width (e.g. for reference score of 0)
        optWidthGrowth :: Double,	-- growth rate of the luft width with reference score
        optTanhScale   :: Double,	-- scale oh tanh function argument (diff - margin)
        optParams   :: [String],	-- list of eval parameter assignements
        -- optLogging  :: LogLevel,	-- logging level
        optNThreads :: Int,		-- number of threads
        optAFenFile :: Maybe FilePath,	-- annotated fen file for self analysis
        optConvert  :: Bool,	        -- convert epd to bin
        optReverse  :: Bool,	        -- reverse fens & check eval
        optServMode :: Bool,		-- run as server
        optWorkDir  :: Maybe FilePath	-- working dir
    } deriving Show

data HyperParams = HyperParams {
        hpMinWidth    :: Double,	-- luft min width (e.g. for reference score of 0)
        hpWidthGrowth :: Double,	-- growth rate of the luft width with reference score
        hpTanhScale   :: Double		-- scale oh tanh function argument (diff - margin)
    }

defaultOptions :: Options
defaultOptions = Options {
        optConfFile = Nothing,
        optMinWidth = 20,		-- luft min width (e.g. for reference score of 0)
        optWidthGrowth = - log 0.5 / 400,	-- growth rate of the luft width with reference score
        optTanhScale   = 0.05,		-- scale oh tanh function argument (diff - margin)
        optParams   = [],
        -- optLogging  = DebugUci,
        optNThreads = 1,
        optAFenFile = Nothing,
        optConvert  = False,
        optReverse  = False,
        optServMode = False,
        optWorkDir = Nothing
    }

setConfFile :: String -> Options -> Options
setConfFile cf opt = opt { optConfFile = Just cf }

setWidthGrowth :: String -> Options -> Options
setWidthGrowth wg opt = opt { optWidthGrowth = - log 0.5 / read wg }

setMinWidth :: String -> Options -> Options
setMinWidth mw opt = opt { optMinWidth = read mw }

setTanhScale :: String -> Options -> Options
setTanhScale ts opt = opt { optTanhScale = read ts }

addParam :: String -> Options -> Options
addParam pa opt = opt { optParams = pa : optParams opt }

-- setLogging :: String -> Options -> Options
-- setLogging lev opt = opt { optLogging = llev }
--     where llev = case levi of
--                    0 -> DebugSearch
--                    1 -> DebugUci
--                    2 -> LogInfo
--                    3 -> LogWarning
--                    4 -> LogError
--                    _ -> if levi < 0 then DebugSearch else LogNever
--           levi = read lev :: Int

addNThrds :: String -> Options -> Options
addNThrds ns opt = opt { optNThreads = read ns }

addAFile :: FilePath -> Options -> Options
addAFile fi opt = opt { optAFenFile = Just fi }

addConvert :: Options -> Options
addConvert opt = opt { optConvert  = True }

setReverse :: Options -> Options
setReverse opt = opt { optReverse  = True }

addServ :: Options -> Options
addServ opt = opt { optServMode = True }

addWDir :: String -> Options -> Options
addWDir cl opt = opt { optWorkDir = Just cl }

options :: [OptDescr (Options -> Options)]
options = [
        Option "c" ["config"]  (ReqArg setConfFile "STRING") "Configuration file",
        Option "m" ["minwidth"] (ReqArg setMinWidth "STRING") "Luft width for score 0 (in cp)",
        Option "g" ["growth"]  (ReqArg setWidthGrowth "STRING") "Score to double the luft width (in cp)",
        Option "b" ["tanhscale"] (ReqArg setTanhScale "STRING") "Scale of the tanh argument",
        Option "p" ["param"]   (ReqArg addParam "STRING")    "Eval/search/time parameters: name=value,...",
        Option "a" ["analyse"] (ReqArg addAFile "STRING")   "Analysis file",
        Option "t" ["threads"] (ReqArg addNThrds "STRING")  "Number of threads",
        Option "o" ["convert"] (NoArg  addConvert)  "Convert epd file to bin",
        Option "r" ["reverse"] (NoArg  setReverse)  "Check epd file with the reverse fens",
        Option "s" ["server"]  (NoArg  addServ)     "Run as server",
        Option "w" ["workdir"] (ReqArg addWDir "STRING")     "Change working directory"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName
              ++ " [-c CONF] [-m MINW] [-g GROWTH] [-b SCALE] [-p name=val[,...]] -a AFILE [-o|-r] [-w WORKDIR]"
          idName = "BCE"

{--
initContext :: Options -> IO Context
initContext opts = do
    clktm <- getClockTime
    let llev = optLogging opts
    lchan <- newChan
    wchan  <- newChan
    ichan <- newChan
    ha <- newCache 1	-- it will take the minimum number of entries
    hi <- newHist
    let paramList = stringToParams $ intercalate "," $ optParams opts
    -- putStrLn "Before eval state"
    (parc, evs) <- makeEvalState (optConfFile opts) paramList "progver" "progsuf"
    -- putStrLn "eval state:"
    -- putStrLn $ show evs
    let chg = Chg {
            working = False,
            compThread = Nothing,
            crtStatus = posToState initPos ha hi evs,
            forGui = Nothing,
            srchStrtMs = 0,
            myColor = White,
            prvMvInfo = Nothing
         }
    ctxVar <- newMVar chg
    let context = Ctx {
            logger = lchan,
            writer = wchan,
            inform = ichan,
            strttm = clktm,
            change = ctxVar,
            loglev = llev,
            evpid  = parc,
            tipars = npSetParm (colParams paramList :: CollectFor TimeParams)
         }
    return context
--}

-- Client/server works only for analysis, filter is always local
main :: IO ()
-- main = withSocketsDo $ do
main = do
    (opts, _) <- theOptions
    when debug $ putStrLn $ "Options: " ++ show opts
    case optWorkDir opts of
        Nothing -> return ()
        Just wd -> setCurrentDirectory wd
    case optAFenFile opts of
        Nothing -> error "EPD input file is require"
        Just fi -> do
            let ext = takeExtension fi
            if ext == ".epd"
               then if optConvert opts
                       then epdToBin fi
                       else if optReverse opts
                               then checkAllFile fi
                               else do
                                   let hp = optsToHyper opts
                                   scoreAllFile fi hp (optConfFile opts) (optParams opts) False
               else do
                   let hp = optsToHyper opts
                   if ext == ".bin"
                      then scoreAllFile fi hp (optConfFile opts) (optParams opts) True
                      else do
                          isdir <- doesDirectoryExist fi
                          if isdir
                             then scoreAllDirFiles fi hp (optConfFile opts) (optParams opts)
                             else error $ "Cannot process input " ++ fi

optsToHyper :: Options -> HyperParams
optsToHyper opts = HyperParams {
        hpMinWidth    = optMinWidth opts,
        hpWidthGrowth = optWidthGrowth opts,
        hpTanhScale   = optTanhScale opts
    }

checkAllFile :: FilePath -> IO ()
checkAllFile fi = do
    putStrLn $ "Check reverse fens over " ++ fi
    (_, es) <- makeEvalState Nothing [] "progver" "progsuf"
    -- clines <- take 10000 . lines <$> readFile fi
    clines <- lines <$> readFile fi
    forM_ clines $ \line -> do
        let fcr = checkPosRev es line
        case fcr of
            FenRev   f1 f2 f3 -> putStrLn $ "RR: " ++ f1 ++ " <-> " ++ f2 ++ " <-> " ++ f3
            FenPos   f1 f2    -> putStrLn $ "FP: " ++ f1 ++ " <-> " ++ f2
            FenScore p1 a1 f1 s1 p2 a2 f2 s2 -> do
                putStrLn $ "SC: " ++ f1 ++ ": " ++ show s1 ++ " <--> " ++ f2 ++ ": " ++ show s2
                putStrLn $ "Pos 1: " ++ show p1
                putStrLn $ "Ass 1: " ++ show a1
                putStrLn $ "Pos 2: " ++ show p2
                putStrLn $ "Ass 2: " ++ show a2
            FenOk                -> return ()
    -- probs <- catMaybes . map (checkPosRev es) . lines <$> readFile fi
    -- putStrLn "Problems:"
    -- mapM_ (\p -> putStrLn (show p)) probs

scoreAllFile :: FilePath -> HyperParams -> Maybe String -> [String] -> Bool -> IO ()
scoreAllFile fi hp mconf params bin = do
    let paramList
            | null params = []
            | otherwise   = stringToParams $ intercalate "," params
    (_, es) <- makeEvalState mconf paramList "progver" "progsuf"
    if bin
       then optFromBinFile fi es hp
       else optFromEpdFile fi es hp

scoreAllDirFiles :: FilePath -> HyperParams -> Maybe String -> [String] -> IO ()
scoreAllDirFiles fi hp mconf params = do
    let paramList
            | null params = []
            | otherwise   = stringToParams $ intercalate "," params
    (_, es) <- makeEvalState mconf paramList "progver" "progsuf"
    optFromDirBinFiles fi es hp

optFromEpdFile :: FilePath -> EvalState -> HyperParams -> IO ()
optFromEpdFile fi es hp = do
    putStrLn $ "Optimizing over " ++ fi
    (cou, err) <- foldl' accumErrorCnt (0, 0) . map (posRegrError es hp . makePosVal) . lines <$> readFile fi
    let ave = err / fromIntegral cou
    putStrLn $ "BCE error (cnt/sum/avg): " ++ show cou ++ " / " ++ show err ++ " / " ++ show ave

accumErrorCnt :: (Int, Double) -> Maybe (Double, Int) -> (Int, Double)
accumErrorCnt (cnt, acc) Nothing         = (cnt,     acc      )
accumErrorCnt (cnt, acc) (Just (err, _)) = (cnt + 1, acc + err)

epdToBin :: FilePath -> IO ()
epdToBin fi = do
    let fo = addExtension (dropExtension fi) ".bin"
    putStrLn $ "Transform " ++ fi ++ " to " ++ fo
    h <- openFile fo WriteMode
    readFile fi >>= \cnt -> return (map (S.encode . makePosVal) (lines cnt)) >>= mapM_ (B.hPut h)
    hClose h

{--

filterFile :: FilePath -> FilePath -> CtxIO ()
filterFile fi fo = do
    inp <- liftIO $ readFile fi
    lift $ putStrLn $ "Filtering " ++ fi
    chg <- readChanging
    let crts = crtStatus chg
    h <- liftIO $ openFile fo WriteMode
    mapM_ (makeMovePos crts (Just h)) $ lines inp
    liftIO $ hClose h

showAgr :: Agreg -> IO ()
showAgr agr = do
    putStrLn $ "Function value: " ++ show (agrCumErr agr)
    putStrLn $ "Total nodes:    " ++ show (agrCumNds agr)

-- Agregate function values in parallel by many threads
parallelAgregate :: [a] -> (MVar Agreg -> a -> CtxIO ()) -> CtxIO Agreg
parallelAgregate es act = do
    vs <- forM es $ \e -> do
        v <- lift $ newEmptyMVar
        void $ newThread $ act v e
        return v
    as <- forM vs $ \v -> liftIO $ takeMVar v
    return $! foldr mappend mempty as

-- For client mode we need to know which hosts & port we should use
myServerPort :: PortID
myServerPort = PortNumber 4321

serverPrefix :: String
serverPrefix = "Server value: "

endOfParams :: String
endOfParams = "EOP"

clientMode :: [String] -> [String] -> CtxIO ()
clientMode hostsstrs params = do
    let hosts = splitOn "," $ intercalate "," hostsstrs
    liftIO $ setNumCapabilities $ length hosts + 1
    agr <- parallelAgregate hosts (askServer params)
    liftIO $ showAgr agr
    -- liftIO $ putStrLn $ "Function value: " ++ show r

-- Currently only parameters on command line will be sent over
-- Config file is not supported over network!!
askServer :: [String] -> MVar Agreg -> HostName -> CtxIO ()
askServer params mvar host = liftIO $ do
    h <- connectTo host myServerPort
    hSetBuffering h LineBuffering
    forM_ params $ \ps -> hPutStrLn h ps
    hPutStrLn h endOfParams
    resline <- lineUntil False h (isPrefixOf serverPrefix)
    let vs = maybe nl (splitOn " ") $ stripPrefix serverPrefix resline
        vd:vn:_ = vs ++ nl
        nl = ["0", "0"]
        !d = read vd
        !n = read vn
    putMVar mvar $! Agreg { agrCumErr = d, agrCumNds = n }

serverMode :: FilePath -> Int -> CtxIO ()
serverMode fi n = do
    lift $ putStrLn $ "Optimizing over " ++ fi
    liftIO $ setNumCapabilities $ n + 1
    chg <- readChanging
    let crts = crtStatus chg
    ts <- (spread n . catMaybes) <$> (liftIO (readFile fi) >>= mapM (makeMovePos crts Nothing) . lines)
    sock <- liftIO $ listenOn myServerPort
    forever $ do
        (h, es) <- liftIO $ do
            (h, host, _) <- accept sock
            putStrLn $ "Accepting request from host " ++ host
            (params, _) <- accumLines False h (== endOfParams) (:) []
            let paramList = stringToParams $ intercalate "," params
            (_, evs) <- makeEvalState Nothing paramList "progver" "progsuf"	-- no config file
            return (h, evs)
        agr <- parallelAgregate ts (agregMVar $ Just es)
        liftIO $ hPutStrLn h $ serverPrefix ++ show (agrCumErr agr) ++ " " ++ show (agrCumNds agr)

-- Read lines until one condition occurs
lineUntil :: Bool -> Handle -> (String -> Bool) -> IO String
lineUntil verb h p = go
    where go = do l <- hGetLine h
                  when verb $ putStrLn $ "lU: " ++ l
                  if p l then return l
                         else go

-- Read lines until one condition occurs, accumulating some information from the read lines
-- (except the one with the condition, which is returned together with the accumulator)
accumLines :: Bool -> Handle -> (String -> Bool) -> (String -> a -> a) -> a -> IO (a, String)
accumLines verb h p f = go
    where go a = do
             l <- hGetLine h
             when verb $ putStrLn $ "aL: " ++ l
             if p l then return (a, l)
                    else go $! f l a

-- Spread a list over many lists
spread :: Int -> [a] -> [[a]]
spread n = go $ take n $ repeat []
    where go (l:ls) (a:as) = go (ls ++ [a:l]) as
          go acc    _      = acc

data Agreg = Agreg {
         agrCumErr :: !Double,	-- accumulated error
         agrCumNds :: !Int
     } deriving Show

instance Monoid Agreg where
    mempty = Agreg { agrCumErr = 0, agrCumNds = 0 }
    a `mappend` b = Agreg { agrCumErr = agrCumErr a + agrCumErr b,
                            agrCumNds = agrCumNds a + agrCumNds b }
--}

makePosVal :: String -> (MyPos, Double)
makePosVal fenval = (pos, val)
    where (fen:sval:_) = splitOn "," fenval
          (pos, _) = posFromFen fen
          val = read sval

-- Use the incremental interface of the Get monad
optFromBinFile :: FilePath -> EvalState -> HyperParams -> IO ()
optFromBinFile fi es hp = do
    putStrLn $ "Optimizing over " ++ fi
    hFlush stdout
    h <- openFile fi ReadMode
    (cou, err) <- iterateBinFile es h hp 0 0 B.empty
    let ave = err / fromIntegral cou
    putStrLn $ "BCE error (cnt/sum/avg): " ++ show cou ++ " / " ++ show err ++ " / " ++ show ave
    hClose h

iterateBinFile :: EvalState -> Handle -> HyperParams -> Int -> Double -> ByteString -> IO (Int, Double)
iterateBinFile es h hp = go
    where go !cnt !err bs = do
             (diri, bsne) <- if bs == B.empty
                                then do
                                    bs1 <- B.hGet h bufsize
                                    return (True, bs1)
                                else return (False, bs)
             if diri && bsne == B.empty
                then return (cnt, err)
                else do
                    let result = runGetPartial S.get bsne
                    (r, rbs) <- iterGet result
                    -- Ignore evaluations with special functions
                    -- putStrLn $ "Debug: " ++ show r ++ "/" ++ show hp
                    let mpe = posRegrError es hp r
                    case mpe of
                        Nothing       -> go  cnt       err       rbs
                        Just (er, st) -> do
                            when (debug && er > 1000000) $ do
                                putStrLn $ "Debug: " ++ posToFen (fst r)
                                putStrLn $ "Debug: " ++ show (snd r) ++ " / " ++ show st ++ " --> " ++ show er
                            go (cnt + 1) (err + er) rbs
          bufsize = 1024 * 8
          iterGet result =
              case result of
                  Fail msg _   -> error msg
                  Partial cont -> do
                      nbs <- B.hGet h bufsize
                      iterGet (cont nbs)
                  Done r rbs   -> return (r, rbs)

-- Use the incremental interface of the Get monad, for all bin files in a directory
optFromDirBinFiles :: FilePath -> EvalState -> HyperParams -> IO ()
optFromDirBinFiles fi es hp = do
    putStrLn $ "Optimizing over directory " ++ fi
    allfiles <- listDirectory fi
    -- putStrLn $ "All files " ++ show allfiles
    let binfiles = filter ((".bin" ==) . takeExtension) allfiles
    -- putStrLn $ "Bin files " ++ show binfiles
    (cnt, err) <- goFile (0::Int) 0 binfiles
    let ave = err / fromIntegral cnt
    putStrLn $ "BCE error (cnt/sum/avg): " ++ show cnt ++ " / " ++ show err ++ " / " ++ show ave
    where goFile !cnt !err []     = return (cnt, err)
          goFile !cnt !err (f:fs) = do
              let fullf = fi ++ "/" ++ f
              putStrLn $ "Bin file " ++ fullf
              hFlush stdout
              h <- openFile fullf ReadMode
              (cnt1, err1) <- iterateBinFile es h hp cnt err B.empty
              hClose h
              goFile cnt1 err1 fs
{-
          bufsize = 1024 * 8
          iterGet result h =
              case result of
                  Fail msg _   -> error msg
                  Partial cont -> do
                      nbs <- B.hGet h bufsize
                      iterGet (cont nbs) h
                  Done r rbs   -> return (r, rbs)
          go !cnt !err bs h = do
             (diri, bsne) <- if bs == B.empty
                                then do
                                    bs1 <- B.hGet h bufsize
                                    return (True, bs1)
                                else return (False, bs)
             if diri && bsne == B.empty
                then return (cnt, err)
                else do
                    let result = runGetPartial S.get bsne
                    (r, rbs) <- iterGet result h
                    -- Ignore evaluations with special functions
                    -- putStrLn $ "Debug: " ++ show r ++ "/" ++ show hp
                    let mpe = posRegrError es hp r
                    when debug $ putStrLn $ "Debug: " ++ show mpe ++ "/" ++ show cnt ++ "/" ++ show err
                    case mpe of
                        Nothing       -> go  cnt       err       rbs h
                        Just (er, _t) -> go (cnt + 1) (err + er) rbs h

-- Calculate evaluation error for one position - binary cross entropy
-- Because this is a binary classification it is very important that we have only 2 classes of positions:
-- won and lost. The game result can be only 1 (won) and -1 (lost)
-- The game result is in val and it is from white point of view
-- Our static score is from side to move point of view, so we have to change sign if black is to move
-- We don't use for tuning special eval (i.e. a specific evaluation function, like passed pawns or so)
posError :: EvalState -> HyperParams -> (MyPos, Double) -> Maybe Double
posError es hp (pos, val)
    | spec      = Nothing
    -- | val ==  1 = Just $ - log myval / complexity pos
    -- | val == -1 = Just $ - log (1 - myval) / complexity pos
    | val ==  1 = Just $ (1 - myval) * (1 - myval) / complexity pos
    | val == -1 = Just $ myval       * myval       / complexity pos
    | otherwise = error $ "Position has wrong result: " ++ show val
    where (!stc, spec) = posEvalSpec pos es
          !myval | moving pos == White =     logisticFunction stc hp
                 | otherwise           = 1 - logisticFunction stc hp

-- We chose the codomain between 0 and 1
logisticFunction :: Int -> HyperParams -> Double
logisticFunction score hp = 1 / (1 + exp (-hp * fromIntegral score))

-- For complex positions with many tactical moves we cannot expect the eval to be very accurate
-- We consider 2 different factors for complexity:
-- - total number of pieces (probably far away from final)
-- - number of attacked pieces (position is unquiet)
complexity :: MyPos -> Double
complexity pos = 1 + atcoeff * fromIntegral atcs + pccoeff * fromIntegral pces
    where atcs = popCount $ (myAttacs pos .&. yo pos) .|.  (yoAttacs pos .&. me pos)
          pces = popCount $ occup pos
          atcoeff = 0.1
          pccoeff = 0.01
-}

-- Calculate evaluation error for one position - regression error, i.e. relative to a reference score
-- The value (or reference score) is the one obtained by a search to some depth, in centipawns,
-- from p.o.v. of side to move
-- Our static score is also from side to move point of view
-- We ignore special eval (i.e. a specific evaluation function, like e.g. for no pawns) and mate scores
-- This is how the error is calculated:
-- - there is a region around the reference score in which the error is zero
-- - outside of that region the error is a tanh function with some scaling, meaning that for a very large
--   score difference the error will be 1 at limit
-- - the zero region is thinner for small scores: +/- 20 cp
-- - for (absolute) higher scores the region is larger, in an exponential manner
posRegrError :: EvalState -> HyperParams -> (MyPos, Double) -> Maybe (Double, Int)
posRegrError es hp (pos, val)
    | mate || spec = Nothing
    | otherwise    = Just (err, stc)
    where (stc, spec) = posEvalSpec pos es
          mate = val >= wemate || val <= yomate
          wemate =  20000 - 100	-- minimum mate score when we mate (like "mate in 100")
          yomate = -20000 + 100	-- minimum mate score when we are mated
          stcr   = fromIntegral stc
          margin = hpMinWidth hp * exp (hpWidthGrowth hp * abs val)
          diff   = stcr - val
          err | abs diff <= margin = 0
              | otherwise          = tanh . abs $ (diff - margin) * hpTanhScale hp

-- Reverse a fen: white <-> black
reverseFen :: String -> String
reverseFen fen = intercalate " " [fen1r, fstmr, fcstr, fenpr, f50m, fmv]
    where fen1:fstm:fcst:fenp:f50m:fmv:_ = fenFromString fen
          -- Tail here is because we end the fen part in "/"
          fen1ri = map revUpLow $ intercalate "/" $ tail $ reverse $ splitFenLines fen1
          fen1r  = fen1ri ++ "/"
          revUpLow 'p' = 'P'
          revUpLow 'P' = 'p'
          revUpLow 'r' = 'R'
          revUpLow 'R' = 'r'
          revUpLow 'n' = 'N'
          revUpLow 'N' = 'n'
          revUpLow 'b' = 'B'
          revUpLow 'B' = 'b'
          revUpLow 'q' = 'Q'
          revUpLow 'Q' = 'q'
          revUpLow 'k' = 'K'
          revUpLow 'K' = 'k'
          revUpLow x   = x
          -- This should be a split:
          splitFenLines s = l : if s' == "" then [] else splitFenLines (tail s') where (l, s') = break ((==) '/') s
          fstmr | fstm == "w" = "b"
                | fstm == "b" = "w"
                | otherwise   = error $ "Wrong side to move: " ++ show fstm
          fcstr = map revUpLow fcst
          fenpr | f:r:_ <- fenp, f `elem` "abcdefgh", r `elem` "36"
                            = f : reve r : []
                | otherwise = fenp
          reve '3' = '6'
          reve '6' = '3'
          reve x   = x

{--
makeMovePos :: MyState -> Maybe Handle -> String -> CtxIO (Maybe (Move, MyState))
makeMovePos crts mh fenLine = do
    let (refmv:fen:_) = splitOn "\t" fenLine
        pos  = posFromFen fen
        euci = fromNiceNotation pos refmv
    case euci of
        Left s  -> do
            lift $ do
                putStrLn $ "Move: " ++ refmv ++ " fen " ++ fen
                putStrLn $ "Wrong move: " ++ show s
            return Nothing
        Right m -> do
            let mystate = posToState pos (hash crts) (hist crts) (evalst crts)
            (ok, _) <- runCState (canDoMove m) mystate
            if ok
               then do
                  case mh of
                      Nothing -> return ()
                      Just h  -> lift $ hPutStrLn h fenLine
                  return $ Just (m, mystate)
               else do
                   lift $ do
                       putStrLn $ "Move: " ++ refmv ++ " fen " ++ fen
                       putStrLn $ "Illegal move"
                   return Nothing

agregAll :: Maybe EvalState -> [(Move, MyState)] -> CtxIO Agreg
agregAll mest = case mest of
    Nothing -> foldM agregPos mempty
    Just es -> foldM agregPos mempty . map (\(m, ms) -> (m, ms { evalst = es }))

agregPos :: Agreg -> (Move, MyState) -> CtxIO Agreg
agregPos agr (m, mystate) = do
    (e, s) <- runCState (searchTestPos m) mystate
    return $! aggregateError agr e (nodes $ stats s)

agregMVar :: Maybe EvalState -> MVar Agreg -> [(Move, MyState)] -> CtxIO ()
agregMVar mest mvar mss = do
    myd <- liftIO $ myThreadId
    liftIO $ putStrLn $ "Thread " ++ show myd ++ " started"
    agr <- agregAll mest mss
    liftIO $ putMVar mvar agr
    liftIO $ putStrLn $ "Thread " ++ show myd ++ " ended"

aggregateError :: Agreg -> Double -> Int -> Agreg
aggregateError agr e n = agr { agrCumErr = agrCumErr agr + e, agrCumNds = agrCumNds agr + n }

newThread :: CtxIO () -> CtxIO ThreadId
newThread a = do
    ctx <- ask
    liftIO $ forkIO $ runReaderT a ctx
--}

type Assoc = [(Square, (Color, Piece))]
data FenCheck = FenOk
              | FenRev String String String
              | FenPos String String
              | FenScore MyPos Assoc String Int MyPos Assoc String Int

-- Check eval for reverse fens
-- 1: reverse reverse = original (check of the reverse fen function itself)
-- 2: fen to pos to fen = original fen
-- 3: eval(fen) = eval(reverse fen)
checkPosRev :: EvalState -> String -> FenCheck
checkPosRev es fenval
    | feno /= fen = FenRev fen fenr feno
    | fenp /= fen = FenPos fen fenp
    | eo   /= er  = FenScore pos ass fen eo posr assr fenr er
    | otherwise   = FenOk
    where (fen:_) = splitOn "," fenval
          fenr = reverseFen fen
          feno = reverseFen fenr
          fenp = posToFen pos
          (pos,  ass)  = posFromFen fen
          (posr, assr) = posFromFen fenr
          eo   = posEval pos  es
          er   = posEval posr es
