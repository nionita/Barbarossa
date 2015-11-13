{-# LANGUAGE BangPatterns
           , PatternGuards
  #-}

module Main (
    main
) where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Data.Bits
import Data.Char (isSpace)
import Data.List
import Data.Maybe (catMaybes)
import System.Console.GetOpt
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO
import System.IO.Error
import System.Process
import System.Random
import System.Timeout

import Struct.Struct
import Moves.BitBoard (uBit)
import Moves.Board
-- import Moves.ShowMe
import Moves.Notation
import Uci.UCI

-- import SSSPSA
import Adam

data Options = Options {
         optConFile :: Maybe String,	-- configuration file
         optRestart :: Bool,		-- save status file
         optNoThrds :: Int,		-- number of threads to run
         optNoSamps :: Maybe Int	-- number of samples to generate
     }

defaultOptions :: Options
defaultOptions = Options {
        optConFile = Nothing, optRestart = False, optNoThrds = 1, optNoSamps = Nothing
    }

setConFile :: String -> Options -> Options
setConFile s opt = opt { optConFile = Just s }

setRestart :: Options -> Options
setRestart opt = opt { optRestart = True }

setNoThrds :: String -> Options -> Options
setNoThrds s opt = opt { optNoThrds = read s }

setNoSamps :: String -> Options -> Options
setNoSamps s opt = opt { optNoSamps = Just $ read s }

options :: [OptDescr (Options -> Options)]
options = [
        Option "c" ["config"]   (ReqArg setConFile "STRING") "Config file",
        Option "r" ["restart"]  (NoArg  setRestart         ) "Restart from a saved checkpoint file",
        Option "g" ["generate"] (ReqArg setNoSamps "STRING") "Number of samples to generate",
        Option "t" ["threads"]  (ReqArg setNoThrds "STRING") "Number of threads"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: Evalo [-c config] [-s savefile]"

type Config = [(String, String)]

-- Default config parameters:
defConBatch, defConDepth, defConLength :: Int
defConBatch   = 256
defConDepth   = 7
defConLength  = 8

defConfig :: Config
defConfig = []

data Score = Cp Int | Mt Int
    deriving Show

data MyState = MyState {
         stRemMvs :: Int,	-- remaining real moves to analyse
         stIniCol :: Color,	-- initial analysis moving color
         stIniFen :: String,	-- initial analysis fen
         stCrtPos :: MyPos,	-- current position
         stScDMvs :: [(Score, Move)],	-- scores and played moves (reversed)
         stZobKys :: [ZKey]	-- zobrist keys seen so far to detect repetition
     }

-- We generate the move, but need no sorting
genMoves :: MyPos -> [Move]
genMoves p
    | isCheck p c = genMoveFCheck p
    | otherwise   = genMoveCast p ++ genMoveTransf p ++ wcs ++ lcs ++ genMoveNCapt p
    where c = moving p
          (wcs, lcs) = genMoveCaptWL p

-- showMyPos :: MyPos -> String
-- showMyPos p = showTab (black p) (slide p) (kkrq p) (diag p) ++ "================ " ++ mc ++ "\n"
--     where mc = if moving p == White then "w" else "b"

main :: IO ()
main = do
    (opts, _) <- theOptions
    case optNoSamps opts of
        Just samps -> generateSamples opts samps
        Nothing    -> optimiseParams  opts

optimiseParams :: Options -> IO ()
optimiseParams opts = do
    config <- case optConFile opts of
                  Just cf -> readConfigFile cf
                  Nothing -> return defConfig
    let save    = getConfigStr config "checkpoint" $ Just ""
        ovs     = getOptimVars config
        pNames  = map fst ovs
        ranges  = map snd ovs
        maxost  = getConfigVal config "maxSteps" $ Just 50
        -- Some parameters for Adam:
        alpha   = getConfigVal config "alpha" $ Just 0.001
        beta1   = getConfigVal config "beta1" $ Just 0.9
        beta2   = getConfigVal config "beta2" $ Just 0.999
        eps     = getConfigVal config "eps"   $ Just 1E-8
        batch   = getConfigVal config "sampBatch" $ Just defConBatch
        threads = optNoThrds opts
        engine  = getConfigStr config "engCom" Nothing
        eopts   = ["-l", "5"]
        playlen = getConfigVal config "playLength" $ Just defConLength
        playdep = getConfigVal config "playDepth" $ Just defConDepth
        -- spsaParams = defSpsaParams { verb = True, nmax = maxost }
        adamParams = defAdamParams { verb = True, alfa = alpha, bet1 = beta1,
                                     bet2 = beta2, epsi = eps, nmax = maxost }
    so <- case save of
               "" -> if optRestart opts
                             then error "Restart requested, but no checkpoint file in config!"
                             else return $ SOStartNoCheckpoint ranges
               _  -> do
                  sfex <- doesFileExist save
                  if sfex
                     then if optRestart opts
                             then return $ SORestart save
                             else error $ "No restart requested, but checkpoint file " ++ save ++ " exists"
                     else if optRestart opts
                             then error $ "Restart requested, but checkpoint file " ++ save ++ " does not exists"
                             else return $ SOStartWithCheckpoint save ranges
    (h, sz) <- openFenFile (getConfigStr config "fenFile" Nothing)
    -- opars <- ssSPSA (bigPointCost h sz batch threads playlen playdep engine eopts pNames) (Just spsaParams) so
    opars <- adam (bigPointCost h sz batch threads playlen playdep engine eopts pNames) (Just adamParams) so
    putStrLn "Optimal params so far:"
    forM_ (zip pNames opars) $ \(n, v) -> putStrLn $ n ++ " = " ++ show v

-- This will calculate the error or cost for a point in the parameter space
-- It will start more threads for this, including a batch reader
bigPointCost :: Handle -> Integer -> Int -> Int -> Int -> Int -> String -> [String]
             -> [String] -> [Double] -> IO Double
bigPointCost h sz bsize threads mvs depth engine eopts pnames params = do
    putStrLn "Cont called with params:"
    let eparams = zip pnames $ map show params
    forM_ eparams $ \(n, v) -> putStrLn $ n ++ " = " ++ v
    chn <- newChan
    _   <- async $ batchReader sz bsize h chn threads
    aes <- sequence $ map async $ take threads $ repeat
               $ oneProc engine eopts eparams chn mvs depth
    -- Now everything is started & calculating; wait for the results & return the sum of costs
    cs <- mapM wait aes
    let (n, s) = foldr (\(a,b) (c,d) -> (a+c, b+d)) (0, 0) cs
        !val   = negate $ s / fromIntegral n	-- we want minimum, so negate
    return val

generateSamples :: Options -> Int -> IO ()
generateSamples opts samps = do
    config <- case optConFile opts of
                  Just cf -> readConfigFile cf
                  Nothing -> return defConfig
    (h, sz) <- openFenFile (getConfigStr config "fenFile" Nothing)
    chn <- newChan
    sequence_ $ take samps $ repeat $ do
        _   <- async $ batchReader sz
                                   (getConfigVal config "sampBatch" $ Just defConBatch)
                                   h chn (optNoThrds opts)
        aes <- sequence $ map async $ take (optNoThrds opts) $ repeat
                   $ oneProc (getConfigStr config "engCom" Nothing) ["-l", "5"] [] chn
                             (getConfigVal config "playLength" $ Just defConLength)
                             (getConfigVal config "playDepth" $ Just defConDepth)
        -- Now everything is started & calculating; wait for the results & sum
        cs <- mapM wait aes
        let (n, s) = foldr (\(a,b) (c,d) -> (a+c, b+d)) (0, 0) cs
        putStrLn $ show n ++ "\t" ++ show (s / fromIntegral n)

-- Open the fen file and return handle & size
openFenFile :: FilePath -> IO (Handle, Integer)
openFenFile fp = do
    h <- openFile fp ReadMode
    hSetBuffering h (BlockBuffering Nothing)
    s <- hFileSize h
    return (h, s)

-- The batch reader reads randomly a number of fens from a fen file,
-- makes them to stati and write them to the channel
-- 1. We would want uniform distribution per fen (i.e. per file line)
-- but what we get here is a distribution with higher probability
-- for fens followint longer lines
-- 2. First line must be some dummy, because it will be always discarded when read
batchReader :: Integer -> Int -> Handle -> Chan (Maybe MyState) -> Int -> IO ()
batchReader fsize bsize h chn thr = do
    sequence_ $ take bsize $ repeat $ readOneFen fsize h chn
    sequence_ $ take thr $ repeat $ writeChan chn Nothing	-- signal the end of the batch

batDebug :: Bool
batDebug = False

readOneFen :: Integer -> Handle -> Chan (Maybe MyState) -> IO ()
readOneFen fsize h chn = do
    st <- randomFen fsize h
    when batDebug $ putStrLn $ "Bat: have new fen: " ++ stIniFen st
    writeChan chn (Just st)

randomFen :: Integer -> Handle -> IO MyState
randomFen fsize h = do
    when batDebug $ putStrLn $ "Bat: want new fen, fen file size is " ++ show fsize
    go
    where fenMinSize = 16	-- to reach even the last fen
          go = do
              r <- randomRIO (0, fsize - fenMinSize)
              when batDebug $ putStrLn $ "Bat: random is " ++ show r
              hSeek h AbsoluteSeek r
              _ <- hGetLine h	-- we discard first read line (incomplete)
              eof <- hIsEOF h
              if eof
                 then do
                     when batDebug $ putStrLn $ "Bat: we got EOF"
                     go
                 else do
                     fen <- hGetLine h
                     when batDebug $ putStrLn $ "Bat: we got fen: " ++ fen
                     -- take one move randomly
                     -- resulting position must be legal
                     -- try 3 times, then abandon
                     mss <- randomMove 3 fen
                     case mss of
                         Just ss -> return ss
                         Nothing -> go

minMvs :: Int
minMvs = 3	-- we skip position which do not have at least so many moves

randomMove :: Int -> String -> IO (Maybe MyState)
randomMove trys fen = do
    let opo = posFromFen fen
        mvs = genMoves opo
        mvl = length mvs
    when batDebug $ putStrLn $ "Bat: pos has " ++ show mvl ++ " moves"
    go trys opo mvs mvl
    where go 0   _   _   _   = return Nothing
          go _   _   _   mvl
             | mvl <= minMvs = return Nothing
          go trs opo mvs mvl = do
              rm <- randomRIO (0, mvl-1)
              when batDebug $ putStrLn $ "Bat: random for move is " ++ show rm
              let mv = mvs !! rm
                  po = doFromToMove mv opo
              when batDebug $ putStrLn $ "Bat: random move is " ++ show mv
              if not $ checkOk po
                 then do
                     when batDebug $ putStrLn $ "Bat: move was not legal"
                     go (trs-1) opo mvs mvl
                 else do
                     when batDebug $ putStrLn $ "Bat: move was ok"
                     return $ Just MyState {
                                       stRemMvs = 0,	-- has to be set later
                                       stIniCol = moving po,
                                       stIniFen = posToFen po,
                                       stCrtPos = po,
                                       stScDMvs = [],
                                       stZobKys = []
                                   }

-- Start an engine with given parameters and drive it to analyse
-- states take from the batch channel as long as there are some more
-- Calculate the error as sum of errors of all analysed states
oneProc :: String -> [String] -> [(String, String)] -> Chan (Maybe MyState) -> Int -> Int -> IO (Int, Double)
oneProc engine eopts params chn mvs depth = do
    let crp | null params = proc engine eopts
            | otherwise   = proc engine (eopts ++ "-p" : intersperse "," (map f params))
    (Just hin, Just hout, _, ph)
         <- createProcess crp { std_in = CreatePipe, std_out = CreatePipe }
    hSetBuffering hin  LineBuffering
    hSetBuffering hout LineBuffering
    hPutStrLn hin "uci"
    _ <- accumLines hout hin ("uciok" `isPrefixOf`) (\_ _ -> ()) ()
    -- Should send options like hash size here...
    r <- catch (runPos hin hout chn mvs depth 0 0) $ \e -> do
        let es = ioeGetErrorString e
        putStrLn $ "Error reading from engine: " ++ es
        terminateProcess ph
        throwIO e
    hPutStrLn hin "quit"
    when funDebug $ putStrLn $ engine ++ ": done, with result " ++ show r
    threadDelay 50000	-- wait to allow smooth quit of engine
    hClose hin
    hClose hout
    _ <- waitForProcess ph
    return r
    where f (pn, pv) = pn ++ "=" ++ pv

uciDebug, funDebug :: Bool
uciDebug = False
funDebug = False

-- Analyse positions through a UCI chess engine connection, returning the error
runPos :: Handle -> Handle -> Chan (Maybe MyState) -> Int -> Int -> Int -> Double -> IO (Int, Double)
runPos hi ho chn mvs depth npos erac = do
    when batDebug $ putStrLn $ "Bat: waiting for new fen from channel..."
    mst <- readChan chn
    case mst of
        Nothing -> do
            when batDebug $ putStrLn $ "Bat: got nothing from chan, exit"
            return (npos, erac)
        Just st -> do
            when (batDebug || funDebug) $ putStrLn $ "Fen to analyse: " ++ stIniFen st
            -- threadDelay 30000
            sf <- execStateT go st { stRemMvs = mvs }
            let merr = calcErrorN sf
            when funDebug $ do
                putStrLn $ "Fen done: " ++ stIniFen st
                putStrLn $ "Fen collects:"
                forM_ (reverse $ stScDMvs sf) $ \s -> putStrLn (show s)
                putStrLn $ "Fen error: " ++ show merr
            -- Remaining moves 0 means we could analyse without errors
            if stRemMvs sf /= 0
               then runPos hi ho chn mvs depth npos erac	-- ignore the failed pos
               else case merr of
                       Nothing   -> runPos hi ho chn mvs depth npos erac	-- ignore
                       Just ferr -> do
                           let !erac' = erac + ferr
                           runPos hi ho chn mvs depth (npos+1) erac'
    where showPl (_, mv) = " " ++ show mv
          go = stopWhenFinalPos $ do
             s <- get
             let ucipos = "position fen " ++ stIniFen s
                 ucimvs | null (stScDMvs s) = ""
                        | otherwise         = " moves" ++ concatMap showPl (reverse $ stScDMvs s)
             (ma, ls) <- lift $ do
                 discardLines ho
                 hPutStrLn hi $ ucipos ++ ucimvs
                 when uciDebug $ putStrLn $ "Sent: " ++ ucipos ++ ucimvs
                 -- let ucitime | moving (stCrtPos s) == White = "wtime "
                 --             | otherwise                    = "btime "
                 -- hPutStrLn hi $ "go movestogo 1 " ++ ucitime ++ show ms
                 -- when uciDebug $ putStrLn $ "Sent: go movestogo 1 " ++ ucitime ++ show ms
                 hPutStrLn hi $ "go depth " ++ show depth
                 when uciDebug $ putStrLn $ "Sent: go depth " ++ show depth
                 accumLines ho hi engCommEnd getSearchResults Nothing
             case ma of
                 Nothing -> lift $ reportEngineProblem s ls "Engine sent no info pv"
                 Just (sc, bm') -> do
                     let p'  = stCrtPos s
                         src = fromSquare bm'
                     case tabla p' src of
                         Empty      -> lift $ reportEngineProblem s ls "Engine moves inexistent piece"
                         Busy c fig -> do
                             let bm | moveIsNormal bm' = moveAddColor c $ moveAddPiece fig bm'
                                    | otherwise        = bm'
                                 m = checkCastle p' $ checkEnPas p' bm
                             if not $ legalMove p' m
                                then lift $ reportEngineProblem s ls "Engine sent illegal best move"
                                else do
                                    let p  = doFromToMove m p'
                                        a  = (sc, m)
                                        as = stScDMvs s
                                        zs = zobkey p : stZobKys s
                                    if not $ checkOk p
                                       then lift $ error $ "Wrong move from engine, illegal position: " ++ show p
                                       else case sc of
                                               -- we stop after a mate was found
                                               Mt _ -> put s { stRemMvs = 0, stCrtPos = p, stScDMvs = a : as, stZobKys = zs }
                                               Cp _ -> do
                                                   let rmvs = stRemMvs s - 1
                                                   put s { stRemMvs = rmvs, stCrtPos = p, stScDMvs = a : as, stZobKys = zs }
                                                   if rmvs > 0 then go else return ()

stopWhenFinalPos :: StateT MyState IO () -> StateT MyState IO ()
stopWhenFinalPos act = do
    s <- get
    let p = stCrtPos s
    -- first check the 50 moves rule:
    if remis50Moves p
       then do
           when funDebug $ lift $ putStrLn $ "Position " ++ posToFen p ++ " : 50 moves rule remis"
           return ()
       else -- check if we have repetition:
           case filter (== zobkey (stCrtPos s)) $ stZobKys s of
               (_:_:_:_) -> do
                   when funDebug $ lift $ putStrLn $ "Position " ++ posToFen p ++ " : 3 times repetition"
                   return ()
               _         -> -- check if there is no move or all are illegal
                   if someLegalMoves p
                      then do
                          when funDebug $ lift $ putStrLn $ "Position " ++ posToFen p ++ " : ok"
                          act
                      else do
                          when funDebug $ lift $ putStrLn $ "Position " ++ posToFen p ++ " : no legal moves"
                          return ()

someLegalMoves :: MyPos -> Bool
someLegalMoves p = or $ map legal mvs
    where mvs | isCheck p (moving p) = genMoveFCheck p
              | otherwise            = let (l2w, l2l) = genMoveCaptWL p
                                       in genMoveCast p ++ genMoveTransf p ++ genMoveNCapt p ++ l2w ++ l2l
          legal m = (occup p .&. bbf /= 0) && (kings p .&. bbt == 0) && (checkOk $ doFromToMove m p)
              where bbf = uBit $ fromSquare m
                    bbt = uBit $ toSquare m

engErrFile :: FilePath
engErrFile = "engErrors.txt"

reportEngineProblem :: MyState -> [String] -> String -> IO ()
reportEngineProblem st ls pre = withFile engErrFile AppendMode $ \h -> do
    hPutStrLn h "*** Problem in mini play ***"
    hPutStrLn h pre
    hPutStrLn h $ "Initial fen: " ++ stIniFen st
    hPutStrLn h "Moves & scores:"
    mapM_ (hPutStrLn h . show) $ reverse $ stScDMvs st
    hPutStrLn h $ "Current fen: " ++ posToFen (stCrtPos st)
    hPutStrLn h "Lines from engine:"
    mapM_ (hPutStrLn h) $ reverse ls

liTout :: Int
liTout = 60000000 -- this is in microseconds; i.e. we wait max 60 s

accumLines :: Handle -> Handle -> (String -> Bool) -> (String -> a -> a) -> a -> IO (a, [String])
accumLines hi ho p f = go []
    where go ls a = do
             eel <- try $ timeout liTout $ hGetLine hi
             case eel of
                 Left e  -> do
                     let es = ioeGetErrorString e
                     when uciDebug $ putStrLn $ "Got: " ++ es
                     let ls' = es : ls
                     return (a, ls')
                 Right ml -> case ml of
                                 Nothing -> do
                                     when uciDebug $ putStrLn $ "Timeout in hGetLine"
                                     hPutStrLn ho "stop"
                                     discardLines hi
                                     return (a, ls)
                                 Just l  -> do
                                     when uciDebug $ putStrLn $ "Got: " ++ l
                                     let ls' = l : ls
                                     if p l then return (a, ls')
                                            else do
                                                let !a' = f l a
                                                go ls' a'

engCommEnd :: String -> Bool
engCommEnd s
    | "bestmove "            `isPrefixOf` s = True
    | "info string empty pv" `isPrefixOf` s = True
    | otherwise                             = False

getSearchResults :: String -> Maybe (Score, Move) -> Maybe (Score, Move)
getSearchResults l old
    | "info score " `isPrefixOf` l = getSB l
    | otherwise                    = old

-- Get score, nodes & best move from a info score line, a primitive approach
getSB :: String -> Maybe (Score, Move)
getSB l
    | ("score":st:sv:rest1) <- dropWhile (/= "score") ws,
      ("depth":_:rest2)     <- dropWhile (/= "depth") rest1,
      ("pv":bm:_)           <- dropWhile (/= "pv")    rest2
        = let sc | st == "cp" = Cp (read sv)
                 | otherwise  = Mt (read sv)
          in case parseMoveStr bm of
              Right mv -> Just (sc, mv)
              _        -> Nothing
    | otherwise = Nothing
    where ws = words l

discardLines :: Handle -> IO ()
discardLines h = do
    threadDelay discTo
    go
    where discTo = 50000	-- 50 ms
          go = do
              has <- hReady h
              if has
                 then do
                     _ <- hGetLine h
                     go
                 else return ()

-- This is a decay in weights of successive score differences
-- Further differences count less and less
lamDecay :: Double
lamDecay = 0.7

-- Some parameters which define the error between expected
-- and actual score in future positions given the score
-- in current position
mateScoreLim, equalScoreLim, winMvs :: Int
equalScoreLim =  100	-- for this score we must win (should be 25?)
mateScoreLim  = 1000	-- from this score we wone
winMvs        =   20	-- defines the score increase rate

-- Old error calculation
calcError :: MyState -> Maybe Double
calcError st
    | null (stScDMvs st) = Nothing
    | otherwise          = Just $ diffs 0 1 $ reverse $ map fst (stScDMvs st)
    where diffs acc w (Cp x : s : ss) = diffs (mulwadd acc w $ errorPerPly x s) (w * lamDecay) (s : ss)
          diffs acc _ _               = acc
          mulwadd a w n = a + w * fromIntegral n

-- This is the error per ply
errorPerPly :: Int -> Score -> Int
errorPerPly x0 (Cp x1) = abs (x0 + x1)
errorPerPly x0 (Mt n )
    | n < 0     = max 0 $ mateScoreLim - x0
    | otherwise = max 0 $ mateScoreLim + x0

-- New error calculation
calcErrorN :: MyState -> Maybe Double
calcErrorN st
    | null (stScDMvs st) = Nothing
    | i > 0              = Just $ s / fromIntegral i
    | otherwise          = Nothing
    where scs = reverse $ map limitScore $ map fst (stScDMvs st)
          dcs = zip [1..] $ iterate (* lamDecay) 1
          (s, i) = foldr (\(s, i) (s0, i0) -> (s+s0, i+i0)) (0, 0)
                       $ map (errorN scs)
                       $ zip dcs $ tail $ tails scs

errorN :: [Int] -> ((Int, Double), [Int]) -> (Double, Int)
errorN ss ((n, w), ns) = (w * fromIntegral (sum (zipWith (f n) ss ns)), l)
    where l = length ns
          f | odd n     = errorPerPlyNO
            | otherwise = errorPerPlyNE

-- When the position is more or less equal, the current score is the target
-- for future scores and can be used directly to compute the errors
-- But when the position is not equal, then the better side is expected to
-- increase the advantage by future moves, so that an increased score
-- has to be taken into consideration - higher for further positions
-- The model is exponential in number of moves
-- For the losing part it is the other way round
targetScore :: Int -> Int -> Int
targetScore n x
    | x >=  mateScoreLim  =  mateScoreLim	-- not much more to win here
    | x >   equalScoreLim =  expScore (n0 + n)
    | x >= -equalScoreLim =  x	-- should we use 0 here? Or something toward 0?
    | x <  -equalScoreLim = -(expScore (n0 + n))
    | x <= -mateScoreLim  = -mateScoreLim	-- not much more to lose here
    where xp = abs x
          n0 = revExp xp

-- expScore 0      = equalScoreLim
-- expScore winMvs = mateScoreLim
-- Scould we go with double from here, to be more smooth?
expScore :: Int -> Int
expScore n = round $ ax * exp (bx * fromIntegral n)

-- Inverse:
revExp :: Int -> Int
revExp n = round $ log (fromIntegral n / ax) / bx

ax, bx :: Double
ax = fromIntegral equalScoreLim
bx = log (fromIntegral mateScoreLim / ax) / fromIntegral winMvs

limitScore :: Score -> Int
limitScore (Cp x)
    | x >  mateScoreLim =  mateScoreLim
    | x < -mateScoreLim = -mateScoreLim
    | otherwise         = x
limitScore (Mt n)
    | n <= 0    = -mateScoreLim
    | otherwise =  mateScoreLim

-- This is the error per ply: the odd part
errorPerPlyNO :: Int -> Int -> Int -> Int
errorPerPlyNO n x0 x1 = abs (targetScore n x0 + x1)

-- This is the error per ply: the even part
errorPerPlyNE :: Int -> Int -> Int -> Int
errorPerPlyNE n x0 x1 = abs (targetScore n x0 - x1)

-- This part with the config is used in Evolve too, better do a separate module!!
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

readConfigFile :: FilePath -> IO Config
readConfigFile cf = readFile cf >>= return . fileToLookup

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
                                 Nothing -> error $ ckey ++ " not found in config"
                                 Just s' -> s'
                  Just s' -> s'
          ckey = "config." ++ key

getConfigVal :: Read a => Config -> String -> Maybe a -> a
getConfigVal cf key mdef
    = case lookup ckey cf of
          Nothing -> case mdef of
                         Nothing -> error $ ckey ++ " not found in config"
                         Just v  -> v
          Just s  -> case reads s of
                         (v, ""):[] -> v
                         _          -> error $ "Can't read " ++ ckey ++ " " ++ s ++ ", wrong type"
    where ckey = "config." ++ key

{--
getOptimVars :: Config -> [(String, ((Double, Double), Double))]
getOptimVars cf = map mkParam $ filter (isPrefixOf "optim." . fst) cf
    where remPrfx = tail . dropWhile (/= '.')
          triple s = let (s1, r1) = break (== ',') s
                         (s2, r2) = break (== ',') (tail r1)
                         s3       = tail r2
                     in ((read s1, read s2), read s3)
          mkParam (s, v) = (remPrfx s, triple v)
--}

getOptimVars :: Config -> [(String, (Double, Double))]
getOptimVars cf = map mkParam $ filter (isPrefixOf "optim." . fst) cf
    where remPrfx = tail . dropWhile (/= '.')
          doub s  = let (s1, r1) = break (== ',') s
                        s2       = tail r1
                    in (read s1, read s2)
          mkParam (s, v) = (remPrfx s, doub v)
