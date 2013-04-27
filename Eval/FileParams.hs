module Eval.FileParams (
    makeEvalState,
    fileToState
  ) where

import Data.Char (isSpace)
import Data.List (tails, intersperse)
import System.Directory

import Struct.Status(EvalState)
import Eval.Eval (initEvalState)

-- Opens a parameter file for eval, read it and create an eval state
makeEvalState :: Maybe FilePath -> String -> String -> IO (FilePath, EvalState)
makeEvalState argfile pver psuff =
    case argfile of
        Just afn -> do	-- config file as argument
            fex <- doesFileExist afn
            if fex then filState afn afn else defState
        Nothing  -> go $ configFileNames pver psuff
    where defState = return ("", initEvalState [])
          go [] = defState
          go (f:fs) = do
             fex <- doesFileExist f
             if fex then filState f "" else go fs

filState :: FilePath -> String -> IO (String, EvalState)
filState fn ident = do
    est <- fileToState fn
    return (ident, est)

fileToState :: FilePath -> IO EvalState
fileToState fn = fileToParams `fmap` readFile fn >>= return . initEvalState

-- This produces a list of config file names depending on
-- program version and programm version suffix
-- The most specific will be first, the most general last
configFileNames :: String -> String -> [String]
configFileNames pver psuff = map cfname $ tails [psuff, pver]
    where fnprf = "evalParams"
          fnsuf = ".txt"
          cfname = concat . (++ [fnsuf]) . intersperse "-" . (fnprf :) . reverse

fileToParams :: String -> [(String, Double)]
fileToParams = map readParam . nocomments . lines
    where nocomments = filter (not . iscomment)
          iscomment [] = True
          iscomment ('-':'-':_) = True
          iscomment (c:cs) | isSpace c = iscomment cs
          iscomment _ = False

readParam :: String -> (String, Double)
readParam s = let (ns, vs) = span (/= '=') s in (strip ns, cleanread vs)
    where strip = filter (not . isSpace)
          cleanread = read . tail . strip
