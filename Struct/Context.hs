module Struct.Context where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.State.Strict
import Control.Monad.Reader
import System.Time

import Struct.Struct
import Struct.Status
import Search.SearchMonad

data InfoToGui = Info {
                    infoDepth :: Int,
                    -- infoSelDepth :: Int,
                    infoTime :: Int,
                    infoNodes :: Int,
                    infoPv :: [Move],
                    infoScore :: Int
                }
                | InfoB {
                    infoPv :: [Move],
                    infoScore :: Int
                }
                | InfoD { infoDepth :: Int }
                | InfoCM {
                    infoMove :: Move,
                    infoCurMove :: Int
                }
                | InfoS { infoString :: String }

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
        inform :: Chan InfoToGui,       -- the gui informer channel
        strttm :: ClockTime,            -- the program start time
        loglev :: LogLevel,             -- loglevel, only higher messages will be logged
        evpid  :: String,		-- identifier for the eval parameter config
        change :: MVar Changing         -- the changing context
    }

-- This is the variable context part (global mutable context)
data Changing = Chg {
        working :: Bool,                -- are we in tree search?
        compThread :: Maybe ThreadId,   -- the search thread id
        crtStatus :: MyState,           -- current state
        forGui :: Maybe InfoToGui,      -- info for gui
        srchStrtMs :: Int,              -- search start time (milliseconds)
        myColor :: Color                -- our play color
    }

type CtxIO = ReaderT Context IO

-- Result of a search
type IterResult = ([Move], Int, [Move], MyState)

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
    when (lev >= loglev ctx) $ liftIO $ logging (logger ctx) (startSecond ctx) (levToPrf lev) mes

startSecond :: Context -> Integer
startSecond ctx = s
    where TOD s _ = strttm ctx

logging lchan refs prf mes = do
    cms <- currMilli refs
    writeChan lchan $ show cms ++ " [" ++ prf ++ "]: " ++ mes

-- Current time in ms since program start
currMilli :: Integer -> IO Int
currMilli ref = do
    TOD s ps   <- liftIO getClockTime
    return $ fromIntegral $ (s-ref)*1000 + ps `div` 1000000000

-- Communicate the best path so far
informGui :: Int -> Int -> Int -> [Move] -> CtxIO ()
informGui sc tief nds path = do
    ctx <- ask
    chg <- readChanging
    currt <- lift $ currMilli $ startSecond ctx
    let gi = Info {
                infoDepth = tief,
                infoTime = currt - srchStrtMs chg,
                infoNodes = nds,
                infoPv = path,
                infoScore = sc
             }
    liftIO $ writeChan (inform ctx) gi

-- Communicate the current move
informGuiCM :: Move -> Int -> CtxIO ()
informGuiCM m cm = do
    ctx <- ask
    let gi = InfoCM { infoMove = m, infoCurMove = cm }
    liftIO $ writeChan (inform ctx) gi

-- Communicate the current depth
informGuiDepth :: Int -> CtxIO ()
informGuiDepth tief = do
    ctx <- ask
    let gi = InfoD { infoDepth = tief }
    liftIO $ writeChan (inform ctx) gi

informGuiString :: String -> CtxIO ()
informGuiString s = do
    ctx <- ask
    let gi = InfoS { infoString = s }
    liftIO $ writeChan (inform ctx) gi
