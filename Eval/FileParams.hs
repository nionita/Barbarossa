module Eval.FileParams (
    makeEvalState,
    fileToState
  ) where

-- import Data.Char (isSpace)
import Data.List (tails, intersperse)
import System.Directory

import Struct.Status(EvalState)
import Struct.Config
import Eval.Eval (initEvalState)

-- Opens a parameter file for eval, read it and create an eval state
makeEvalState :: Maybe FilePath -> [(String, Double)] -> String -> String -> IO (FilePath, EvalState)
makeEvalState argfile assigns pver psuff =
    case argfile of
        Just afn -> do	-- config file as argument
            fex <- doesFileExist afn
            if fex then filState afn afn assigns else defState
        Nothing  -> go $ configFileNames pver psuff
    where defState = return ("", initEvalState assigns)
          go [] = defState
          go (f:fs) = do
             fex <- doesFileExist f
             if fex then filState f "" assigns else go fs

filState :: FilePath -> String -> [(String, Double)] -> IO (String, EvalState)
filState fn ident ass = do
    est <- fileToState fn ass
    return (ident, est)

fileToState :: FilePath -> [(String, Double)] -> IO EvalState
fileToState fn ass = do
    fCont <- readFile fn
    return $ initEvalState $ ass ++ fileToParams fCont

-- This produces a list of config file names depending on
-- program version and programm version suffix
-- The most specific will be first, the most general last
configFileNames :: String -> String -> [String]
configFileNames pver psuff = map cfname $ tails [psuff, pver]
    where fnprf = "evalParams"
          fnsuf = ".txt"
          cfname = concat . (++ [fnsuf]) . intersperse "-" . (fnprf :) . reverse
