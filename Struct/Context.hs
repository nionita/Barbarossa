{-# LANGUAGE TypeFamilies #-}
module Struct.Context (
    Context(..),
    CtxIO,
    LogLevel(..),
    Changing(..),
    IterResult,
    levToPrf, readChanging, modifyChanging, ctxLog, logging,
    getMyTime, formatMyTime, startSecond, currMilli,
    answer, bestMove, infos,
    informGuiBM, informGuiCM, informGuiDraft, informGuiSt
) where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Int
import Data.Time.Clock (UTCTime(..), getCurrentTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import Struct.Struct
import Struct.Status

mateScore :: Int
mateScore = 20000

data LogLevel = DebugSearch | DebugUci | LogInfo | LogWarning | LogError | LogNever
    deriving (Eq, Ord)

levToPrf :: LogLevel -> String
levToPrf DebugSearch = "DebugS"
levToPrf DebugUci    = "DebugU"
levToPrf LogInfo     = "Info"
levToPrf LogWarning  = "Warning"
levToPrf LogError    = "Error"
levToPrf LogNever    = "Never"

-- This is the context in which the other components run
-- it has a fix part, established at the start of the programm,
-- and a variable part (type Changing) which is kept in an MVar
data Context = Ctx {
        logger :: Chan String,          -- the logger channel
        writer :: Chan String,          -- the writer channel
        strttm :: UTCTime,              -- the program start time
        loglev :: LogLevel,             -- loglevel, only higher messages will be logged
        evpid  :: String,		-- identifier for the eval parameter config
        change :: MVar Changing         -- the changing context
    }

-- This is the variable context part (global mutable context)
data Changing = Chg {
        working    :: Bool,             -- are we in tree search?
        compThread :: Maybe ThreadId,   -- the search thread id
        crtStatus  :: MyState,          -- current state
        realPly    :: Maybe Int,	-- real ply so far (if defined)
        forGui     :: Maybe [Move],     -- info for gui: best path so far (if any)
        srchStrtMs :: Int,              -- search start time (milliseconds)
        myColor    :: Color,            -- our play color
        totBmCh    :: Int,		-- all BM changes in this search
        lastChDr   :: Int,		-- last draft which changed root BM
        lmvScore   :: Maybe Int		-- last move score
    }

type CtxIO = ReaderT Context IO

-- Result of a search
type IterResult = ([Move], Int, [Move], Bool, MyState, Int)

readChanging :: CtxIO Changing
readChanging = do
    ctx <- ask
    liftIO $ readMVar $ change ctx

modifyChanging :: (Changing -> Changing) -> CtxIO ()
modifyChanging f = do
    ctx <- ask
    liftIO $ modifyMVar_ (change ctx) (return . f)

ctxLog :: LogLevel -> String -> CtxIO ()
ctxLog lev mes = do
    ctx <- ask
    when (lev >= loglev ctx) $ liftIO $ logging (logger ctx) (strttm ctx) (levToPrf lev) mes

logging :: Chan String -> UTCTime -> String -> String -> IO ()
logging lchan refs prf mes = do
    cms <- currMilli refs
    writeChan lchan $ show cms ++ " [" ++ prf ++ "]: " ++ mes

-- A few time related convenience functions
getMyTime :: IO UTCTime
getMyTime = getCurrentTime

formatMyTime :: UTCTime -> String
formatMyTime utc = formatTime defaultTimeLocale "%F %T" utc

startSecond :: Context -> Integer
startSecond ctx = truncate s
    where UTCTime _ s = strttm ctx

-- Current time in ms since program start
currMilli :: UTCTime -> IO Int
currMilli ref = do
    utc <- getCurrentTime
    return $ truncate $ diffUTCTime utc ref * 1000

-- A few functions to communicate with the GUI
-- Communicate best path so far
informGuiBM :: Int -> Int -> Int -> Int64 -> [Move] -> CtxIO ()
informGuiBM sc depth seld nds path = do
    ctx <- ask
    chg <- readChanging
    currt <- lift $ currMilli $ strttm ctx
    let infoTime = currt - srchStrtMs chg
    answer $ formInfoBM sc depth seld infoTime nds path

-- Communicate the current move
informGuiCM :: Move -> Int -> CtxIO ()
informGuiCM m = answer . formInfoCM m

-- Communicate the current depth
informGuiDraft :: Int -> CtxIO ()
informGuiDraft = answer . formInfoDraft

informGuiSt :: String -> CtxIO ()
informGuiSt = answer . infos

-- Helper: Answers the GUI with a string
answer :: String -> CtxIO ()
answer s = do
    ctx <- ask
    liftIO $ writeChan (writer ctx) s

-- Functions to format the UCI answers to GUI
bestMove :: Move -> Maybe Move -> String
bestMove m mp = s
    where s = "bestmove " ++ toString m ++ sp
          sp = maybe "" (\v -> " ponder " ++ toString v) mp

-- Format best move info:
formInfoBM :: Int -> Int -> Int -> Int -> Int64 -> [Move] -> String
formInfoBM sc depth seld time nodes path = "info"
    ++ formScore esc
    ++ " depth " ++ show depth
    ++ " seldepth " ++ show seld
    ++ " time " ++ show time
    ++ " nodes " ++ show nodes
    ++ nps'
    ++ " pv" ++ concatMap (\m -> ' ' : toString m) path
    where nps' = case time of
                     0 -> ""
                     x -> " nps " ++ show (nodes `div` fromIntegral x * 1000)
          esc = scoreToExtern sc (length path)

data ExternScore = Score Int | Mate Int

-- The internal score is weird for found mates (always mate)
-- Turn it to nicer score by considering path lenght to mate
scoreToExtern :: Int -> Int -> ExternScore
scoreToExtern sc le
    | sc ==  mateScore = Mate $ (le + 1) `div` 2
    | sc == -mateScore = Mate $ negate $ le `div` 2
    | otherwise        = Score sc

-- formInfoB :: InfoToGui -> String
-- formInfoB itg = "info"
--     -- ++ " score cp " ++ show isc
--     ++ formScore isc
--     ++ " pv" ++ concatMap (\m -> ' ' : toString m) (infoPv itg)
--     where isc = infoScore itg

formScore :: ExternScore -> String
formScore (Score s) = " score cp " ++ show s
formScore (Mate n)  = " score mate " ++ show n

formInfoDraft :: Int -> String
formInfoDraft depth = "info depth " ++ show depth

formInfoCM :: Move -> Int -> String
formInfoCM mv n
    = "info currmove " ++ toString mv ++ " currmovenumber " ++ show n

infos :: String -> String
infos s = "info string " ++ s
