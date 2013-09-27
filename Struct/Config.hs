{-# LANGUAGE TypeFamilies #-}

module Struct.Config (
    CollectParams(..),
    fileToParams, stringToParams,
    readParam, colParams, lookApply
) where

import Data.Char (isSpace)
import Data.Maybe (catMaybes)

-- A class of generic parameter records with numeric values
class CollectParams p where
    type CollectFor p
    npColInit :: p
    npColParm :: (String, Double) -> p -> p
    npSetParm :: p -> CollectFor p

colParams :: CollectParams p => [(String, Double)] -> p
colParams = foldr npColParm npColInit

readParam :: String -> Maybe (String, Double)
readParam s = let (ns, vs) = span (/= '=') s
              in case vs of
                     ('=' : rs) -> case reads (strip rs) of
                                       (v, ""):[] -> Just (strip ns, v)
                                       _          -> Nothing	-- did not read
                     _         -> Nothing	-- did not contain '='
    where strip = filter (not . isSpace)

type Setter a = Double -> a -> a

lookApply :: String -> Double -> a -> [(String, Setter a)] -> a
lookApply s v a = maybe a (($ a) . ($ v)) . lookup s

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
