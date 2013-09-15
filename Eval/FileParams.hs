module Eval.FileParams (
    makeEvalState,
    fileToState
  ) where

import Data.Char (isSpace)
import Data.List (tails, intersperse)
import Data.Maybe (catMaybes)
import System.Directory

import Struct.Status(EvalState)
import Eval.Eval (initEvalState)

-- Opens a parameter file for eval, read it and create an eval state
makeEvalState :: Maybe FilePath -> String -> String -> String -> IO (FilePath, EvalState)
makeEvalState argfile assigns pver psuff =
    case argfile of
        Just afn -> do	-- config file as argument
            fex <- doesFileExist afn
            if fex then filState afn afn assigns else defState
        Nothing  -> go $ configFileNames pver psuff
    where defState = return ("", initEvalState $ stringToParams assigns)
          go [] = defState
          go (f:fs) = do
             fex <- doesFileExist f
             if fex then filState f "" assigns else go fs

filState :: FilePath -> String -> String -> IO (String, EvalState)
filState fn ident ass = do
    est <- fileToState fn ass
    return (ident, est)

fileToState :: FilePath -> String -> IO EvalState
fileToState fn ass = do
    fCont <- readFile fn
    return $ initEvalState $ stringToParams ass ++ fileToParams fCont

-- This produces a list of config file names depending on
-- program version and programm version suffix
-- The most specific will be first, the most general last
configFileNames :: String -> String -> [String]
configFileNames pver psuff = map cfname $ tails [psuff, pver]
    where fnprf = "evalParams"
          fnsuf = ".txt"
          cfname = concat . (++ [fnsuf]) . intersperse "-" . (fnprf :) . reverse

-- Convert the content of a file with assignments par=val (one per line)
-- and possibly some comments (Haskell style) into pairs (name, value)
fileToParams :: String -> [(String, Double)]
fileToParams = catMaybes . map readParam . nocomments . lines
    where nocomments = filter (not . iscomment)
          iscomment [] = True
          iscomment ('-':'-':_) = True
          iscomment (c:cs) | isSpace c = iscomment cs
          iscomment _ = False

-- Convert a string "par=val,..." into pairs (name, value)
stringToParams :: String -> [(String, Double)]
stringToParams = catMaybes . map readParam . items	-- items is like lines (copied from Data.List)
    where items s = cons (case break (== ',') s of
                              (i, s') -> (i, case s' of
                                                 [] -> []
                                                 _:s'' -> items s''))
          cons ~(h, t) = h : t

readParam :: String -> Maybe (String, Double)
readParam s = let (ns, vs) = span (/= '=') s
              in case vs of
                     ('=' : rs) -> case reads (strip rs) of
                                       (v, ""):[] -> Just (strip ns, v)
                                       _          -> Nothing	-- did not read
                     _         -> Nothing	-- did not contain '='
    where strip = filter (not . isSpace)
