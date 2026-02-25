{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where
import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Control.Exception
import Data.Array.Unboxed
import Data.Foldable (foldrM)
import Data.List (intersperse)
import Data.Maybe
import Data.Time.Clock (UTCTime)
import Data.Typeable
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO
import System.Random
import Data.Vector.Unboxed (toList)

import Struct.Struct
import Struct.Status
import Struct.Context
import Struct.Config
import Hash.TransTab
import Uci.UCI
import Uci.UciGlue
import Moves.ShowMe
import Moves.Core (movesInit, reverseMoving, genMoveNCapt, doFromToMove)
import Moves.History
import Search.CStateMonad (execCState)
import Eval.FileParams (posToIndexes)

main :: IO ()
main = do
    let fen1 = "8/8/8/8/4k3/2K5/8/8 w - - 0 1"
        fen2 = "8/8/8/4r3/4k3/1QK5/8/8 w - - 0 1"
        mypos = setStaticScore initPos 64
    printPos mypos "Before"
    let mvs = genMoveNCapt mypos
    putStrLn $ "Moves: " ++ show mvs
    when (not $ null mvs) $ do
        let pos1 = doFromToMove (head mvs) mypos
        printPos pos1 "After"

printPos :: MyPos -> String -> IO ()
printPos p zus = do
    putStrLn $ "*** Position: " ++ zus ++ " ***"
    forM_ (zip [0..] $ toList p) $ \(i, w) -> do
        putStrLn $ "* Index " ++ show i
        putStrLn $ showBB w
