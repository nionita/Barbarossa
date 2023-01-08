{-# LANGUAGE BangPatterns #-}

module Tune.Reinforce (
    evalSearchPos
) where
import Control.Monad.Reader
import System.IO

import Struct.Context
import Struct.Status
import Moves.Fen
import Moves.Base
import Tune.Base

debug :: Bool
debug = False

-- We search a position to a specified depth and put FEN & eval to a file
evalSearchPos :: Handle -> Handle -> Int -> Maybe Int -> Int -> () -> CtxIO (Bool, ())
evalSearchPos hi ho depth mn k () = do
    end <- case mn of
               Nothing -> lift $ hIsEOF hi
               Just n  -> if k <= n then lift $ hIsEOF hi else return True
    if end
       then return (False, ())
       else do
           fen <- lift $ hGetLine hi
           when debug $ lift $ do
               putStrLn $ "Fen: " ++ fen
               hFlush stdout
           when (k `mod` 10000 == 0) $ do
               ctx <- ask
               currms <- lift $ currMilli (strttm ctx)
               lift $ do
                   putStrLn $ "Positions completed: " ++ show k ++ " ("
                       ++ show (k * 1000 `div` currms) ++ " positions per second)"
                   hFlush stdout
           let pos = posFromFen fen
           chg <- readChanging
           let crts = crtStatus chg
               sini = posToState pos (hash crts) (hist crts) (evalst crts)
           modifyChanging $ \c -> c { crtStatus = sini }
           (msc, path, _) <- iterativeDeepening depth Nothing
           case msc of
               Nothing -> return ()
               Just sc -> if null path
                             then return ()
                             else lift $ hPutStrLn ho $ fen ++ "," ++ show sc
           return (True, ())
