{-# LANGUAGE BangPatterns #-}
module Moves.History (
        History, newHist, toHist, valHist
    ) where

import Control.Monad (when)
import Control.Arrow (first)

import qualified Data.Vector.Unboxed.Mutable as V
import Data.Bits

import Struct.Struct

type History = V.IOVector Int

rows, cols, depths, vsize :: Int
rows = 64
cols = 64
depths = 20
vsize = rows * cols * depths

adr :: Int -> Int -> Int -> Int
adr r c d = (r * rows + c) * depths + d

newHist :: IO History
newHist = V.replicate vsize 0

ad00 = 8	-- 8	-- 1
-- adp2 = 8	-- 16	-- 1
-- adp4 = 8	-- 32	-- 1
-- adp6 = 8	-- 64	-- 1
-- adm2 = 4	-- 1	-- 4
-- adm4 = 2	-- 1	-- 2
-- adm6 = 1

pos = [16, 16, 16, 16]
neg = [8, 4, 2, 1]

toHist :: History -> Bool -> Square -> Square -> Int -> IO ()
toHist !h True !f !t !d = do
    let ad = adr f t d
    addHist h ad ad00
    mapM_ (uncurry (addHist h) . (first ((+) ad)))
             (zip (takeWhile (< dd) [2, 4 ..]) pos)
    mapM_ (uncurry (addHist h) . (first ((-) ad)))
             -- $ takeWhile ((>= d) . fst) $ zip [2, 4 ..] neg
             (zip (takeWhile (<= d) [2, 4 ..]) neg)
    where dd = depths - d
toHist !h False !f !t !d = do
    let ad = adr f t d
    subHist h ad ad00
    mapM_ (uncurry (subHist h) . (first ((+) ad)))
             (zip (takeWhile (< dd) [2, 4 ..]) pos)
    mapM_ (uncurry (subHist h) . (first ((-) ad)))
             (zip (takeWhile (<= d) [2, 4 ..]) neg)
    where dd = depths - d

valHist :: History -> Square -> Square -> Int -> IO Int
valHist !h !f !t !d = V.unsafeRead h $! adr f t d

addHist :: History -> Int -> Int -> IO ()
addHist h ad p = do
    a <- V.unsafeRead h ad
    V.unsafeWrite h ad (a - p)	-- trick here: we subtract, so that the sort is big to small

subHist :: History -> Int -> Int -> IO ()
subHist h ad p = do
    a <- V.unsafeRead h ad
    V.unsafeWrite h ad (a + p)	-- trick here: we add, so that the sort is big to small
