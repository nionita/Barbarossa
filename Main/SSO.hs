{-# LANGUAGE BangPatterns
             #-}

module Main (
    main
) where

import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import Control.Monad.State
import Data.List
import System.FilePath
import System.IO
import System.IO.Error
import System.Process
import System.Random

import Struct.Struct
import Moves.Board
import Moves.ShowMe
import Moves.Notation
import Uci.UCI

data Score = Cp Int | Mt Int
    deriving Show

data MyState = MyState {
         stRemMvs :: Int,	-- remaining real moves to analyse
         stIniCol :: Color,	-- initial analysis moving color
         stIniFen :: String,	-- initial analysis fen
         stCrtPos :: MyPos,	-- current position
         stScDMvs :: [(Score, Int, Move)]	-- scores, depths, played moves (reversed)
     }

-- We generate the move, but need no sorting
genMoves :: MyPos -> [Move]
genMoves p
    | isCheck p c = genMoveFCheck p
    | otherwise   = genMoveCast p ++ genMoveTransf p ++ wcs ++ lcs ++ genMoveNCapt p
    where c = moving p
          (wcs, lcs) = genMoveCaptWL p

showMyPos :: MyPos -> String
showMyPos p = showTab (black p) (slide p) (kkrq p) (diag p) ++ "================ " ++ mc ++ "\n"
    where mc = if moving p == White then "w" else "b"

testEngine, theFenFile :: String
testEngine = "dist" </> "build" </> "Barbarossa" </> "Barbarossa"
theFenFile = "test.fen"

noThreads, batchSize, noSamps :: Int
noThreads =  5
batchSize = 128
noSamps   = 16

main :: IO ()
main = do
    (h, sz) <- openFenFile theFenFile
    chn <- newChan
    sequence_ $ take noSamps $ repeat $ do
        _   <- async $ batchReader sz batchSize h chn noThreads
        aes <- sequence $ map async $ take noThreads $ repeat $ oneProc testEngine ["-l", "5"] [] chn 8 7
        -- Now everything is started & calculating; wait for the results & return the sum of costs
        cs <- mapM wait aes
        putStrLn $ show $ sum cs

-- Open the fen file and return handle & size
openFenFile :: FilePath -> IO (Handle, Integer)
openFenFile fp = do
    h <- openFile fp ReadMode
    hSetBuffering h (BlockBuffering Nothing)
    s <- hFileSize h
    return (h, s)

-- This will calculate the error or cost for a point in the parameter space
-- It will start more threads for this, including a batch reader
bigPointCost :: Handle -> Integer -> Int -> Int -> Int -> Int -> String -> [String]
             -> [(String, String)] -> IO Double
bigPointCost h fsize bsize threads mvs depth engine eopts params = do
    chn <- newChan
    _   <- async $ batchReader fsize bsize h chn threads
    aes <- sequence $ map async $ take threads $ repeat $ oneProc engine eopts params chn mvs depth
    -- Now everything is started & calculating; wait for the results & return the sum of costs
    cs <- mapM wait aes
    return $! sum cs

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
                                       stScDMvs = []
                                   }

-- Start an engine with given parameters and drive it to analyse
-- states take from the batch channel as long as there are some more
-- Calculate the error as sum of errors of all analysed states
oneProc :: String -> [String] -> [(String, String)] -> Chan (Maybe MyState) -> Int -> Int -> IO Double
oneProc engine eopts params chn mvs depth = do
    let crp | null params = proc engine eopts
            | otherwise   = proc engine (eopts ++ "-p" : intersperse "," (map f params))
    (Just hin, Just hout, _, ph)
         <- createProcess crp { std_in = CreatePipe, std_out = CreatePipe }
    hSetBuffering hin  LineBuffering
    hSetBuffering hout LineBuffering
    hPutStrLn hin "uci"
    _ <- accumLines hout ("uciok" `isPrefixOf`) (\_ _ -> ()) ()
    -- Should send options like hash size here...
    r <- catch (runPos hin hout chn mvs depth 0) $ \e -> do
        let es = ioeGetErrorString e
        putStrLn $ "Error reading from engine: " ++ es
        terminateProcess ph
        throwIO e
    hPutStrLn hin "quit"
    when funDebug $ putStrLn $ engine ++ ": done, with result " ++ show r
    hClose hin
    hClose hout
    _ <- waitForProcess ph
    return r
    where f (pn, pv) = pn ++ "=" ++ pv

uciDebug, funDebug :: Bool
uciDebug = False
funDebug = False

-- Analyse positions through a UCI chess engine connection, returning the error
runPos :: Handle -> Handle -> Chan (Maybe MyState) -> Int -> Int -> Double -> IO Double
runPos hi ho chn mvs depth acc = do
    when batDebug $ putStrLn $ "Bat: waiting for new fen from channel..."
    mst <- readChan chn
    case mst of
        Just st -> do
            when (batDebug || funDebug) $ putStrLn $ "Fen to analyse: " ++ stIniFen st
            sf <- execStateT go st { stRemMvs = mvs }
            let !acc' = acc + ferr
                !ferr = calcError sf
            when funDebug $ do
                putStrLn $ "Fen done: " ++ stIniFen st
                putStrLn $ "Fen collects:"
                forM_ (reverse $ stScDMvs sf) $ \s -> putStrLn (show s)
                putStrLn $ "Fen error: " ++ show ferr
            runPos hi ho chn mvs depth acc'
        Nothing -> do
            when batDebug $ putStrLn $ "Bat: got nothing from chan, exit"
            return acc
    where go = do
             s <- get
             let ucipos = "position fen " ++ stIniFen s
                 ucimvs | null (stScDMvs s) = ""
                        | otherwise         = " moves" ++ concatMap f (reverse $ stScDMvs s)
             Just a@(sc, _, bm) <- lift $ do
                 hPutStrLn hi $ ucipos ++ ucimvs
                 when uciDebug $ putStrLn $ "Sent: " ++ ucipos ++ ucimvs
                 -- let ucitime | moving (stCrtPos s) == White = "wtime "
                 --             | otherwise                    = "btime "
                 -- hPutStrLn hi $ "go movestogo 1 " ++ ucitime ++ show ms
                 -- when uciDebug $ putStrLn $ "Sent: go movestogo 1 " ++ ucitime ++ show ms
                 hPutStrLn hi $ "go depth " ++ show depth
                 when uciDebug $ putStrLn $ "Sent: go depth " ++ show depth
                 -- We don't check the time - but what if process is stuck?
                 accumLines ho ("bestmove " `isPrefixOf`) getSearchResults Nothing
             let p   = stCrtPos s
                 bm' = checkCastle (checkEnPas bm p) p
                 p'  = doFromToMove bm' p
             if not $ checkOk p'
                then lift $ error $ "Wrong move from engine, illegal position: " ++ show p'
                else do
                    let rmvs = stRemMvs s - 1
                    put s { stRemMvs = rmvs, stCrtPos = p', stScDMvs = a : stScDMvs s }
                    case sc of
                        Cp _ -> if rmvs > 0 then go else return ()
                        Mt _ -> return ()	-- we stop after a mate
          f (_, _, mv) = " " ++ show mv

accumLines :: Handle -> (String -> Bool) -> (String -> a -> a) -> a -> IO a
accumLines h p f = go
    where go a = do
             l <- hGetLine h
             when uciDebug $ putStrLn $ "Got: " ++ l
             let !a' = f l a
             if p l then return a
                    else go a'

getSearchResults :: String -> Maybe (Score, Int, Move) -> Maybe (Score, Int, Move)
getSearchResults l old
    | "info score " `isPrefixOf` l = Just $ getSDB l
    -- | "bestmove "   `isPrefixOf` l = old
    | otherwise                    = old

-- Get score, nodes & best move from a info score line, a primitive approach
getSDB :: String -> (Score, Int, Move)
getSDB l = (sc, read n, mv)
    where ws = words l
          ("score":st:sv:rest1) = dropWhile (/= "score") ws
          ("depth":n:rest2)     = dropWhile (/= "depth") rest1
          ("pv":bm:_)           = dropWhile (/= "pv")    rest2
          sc | st == "cp" = Cp (read sv)
             | otherwise  = Mt (read sv)
          Right mv = parseMoveStr bm

-- This is a decay in weights of successive score differences
-- Further differences count less and less
lamDecay :: Double
lamDecay = 0.7

calcError :: MyState -> Double
calcError st
    | null (stScDMvs st) = error "Status with no moves!"
    | otherwise          = diffs 0 1 $ reverse $ map (\(s, _, _) -> s) (stScDMvs st)
    where diffs acc w (Cp x : s : ss) = diffs (mulwadd acc w $ errorPerPly x s) (w * lamDecay) (s : ss)
          diffs acc _ _               = acc
          mulwadd a w n = a + w * fromIntegral n

mateScoreMax :: Int
mateScoreMax = 1000

-- This is the error per ply
errorPerPly :: Int -> Score -> Int
errorPerPly x0 (Cp x1) = abs (x0 + x1)
errorPerPly x0 (Mt n )
    | n < 0     = max 0 $ mateScoreMax - x0
    | otherwise = max 0 $ mateScoreMax + x0
