-- {-# LANGUAGE PatternGuards #-}
-- {-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where
import Control.Monad (when)
import Data.Aeson (eitherDecodeStrict)
import Data.Aeson.Types (FromJSON)
import qualified Data.ByteString as B

import GHC.Generics ()

import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO
import Eval.NNUE

instance FromJSON Layer
instance FromJSON FinalLayer
instance FromJSON NNUE

debug :: Bool
debug = False

data Options = Options {
        optOutFile :: FilePath	-- output file for binary model
    }

defaultOptions :: Options
defaultOptions = Options {
        optOutFile = "model"
    }

addOFile :: FilePath -> Options -> Options
addOFile fi opt = opt { optOutFile = fi }

options :: [OptDescr (Options -> Options)]
options = [
        Option "o" ["output"] (ReqArg addOFile "STRING") "Output file"
    ]

theOptions :: IO (Options, [String])
theOptions = do
    args <- getArgs
    case getOpt Permute options args of
        (o, n, []) -> return (foldr ($) defaultOptions o, n)
        (_, _, es) -> ioError (userError (concat es ++ usageInfo header options))
    where header = "Usage: " ++ idName ++ " -o PATH"
          idName = "JsonToBin"

main :: IO ()
main = do
    (opts, _) <- theOptions
    transformModel (optOutFile opts)

-- Read a Json string from standard input (representing a model)
-- and serialize the model to a binary file
transformModel :: FilePath -> IO ()
transformModel outFile = do
    putStrLn $ "Write the model to " ++ outFile
    ins <- B.getContents
    case eitherDecodeStrict ins of
        Left mes -> putStrLn $ "Cannot decode: " ++ mes
        Right mo -> modelSave mo outFile
