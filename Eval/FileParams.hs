module Eval.FileParams (
    makeEvalRO,
    fileToState
  ) where

-- import Data.Char (isSpace)
import Data.List (tails, intersperse)
import System.Directory

import Struct.Status(EvalRO)
import Struct.Config
import Eval.Eval (initEvalRO)

-- Opens a parameter file for eval, read it and create an eval state
makeEvalRO :: Maybe FilePath -> [(String, Double)] -> String -> String -> IO (FilePath, EvalRO)
makeEvalRO argfile assigns pver psuff = do
    -- putStrLn $ "makeEvalRO: " ++ show argfile
    case argfile of
        Just afn -> do	-- config file as argument
            fex <- doesFileExist afn
            if fex then filState afn afn assigns else error $ "makeEvalRO: no such file: " ++ afn
        Nothing  -> go $ configFileNames pver psuff
    where defState = return ("", initEvalRO assigns)
          go [] = defState
          go (f:fs) = do
             fex <- doesFileExist f
             if fex then filState f "" assigns else go fs

filState :: FilePath -> String -> [(String, Double)] -> IO (String, EvalRO)
filState fn ident ass = do
    est <- fileToState fn ass
    return (ident, est)

fileToState :: FilePath -> [(String, Double)] -> IO EvalRO
fileToState fn ass = do
    fCont <- readFile fn
    -- putStrLn $ "This is the file " ++ fn ++ ":" ++ fCont
    let ies = initEvalRO $ ass ++ fileToParams fCont
    -- putStrLn $ "This is state: " ++ show ies
    return ies

-- This produces a list of config file names depending on
-- program version and programm version suffix
-- The most specific will be first, the most general last
configFileNames :: String -> String -> [String]
configFileNames pver psuff = map cfname $ tails [psuff, pver]
    where fnprf = "evalParams"
          fnsuf = ".txt"
          cfname = concat . (++ [fnsuf]) . intersperse "-" . (fnprf :) . reverse
