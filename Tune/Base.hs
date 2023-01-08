{-# LANGUAGE BangPatterns #-}

module Tune.Base (
    iterativeDeepening,
    aloNodes, subNodes, stopNodes
) where

import Search.AlbetaTypes
import Struct.Context
import Struct.Status
import Struct.Struct
import Uci.UciGlue

-- debug :: Bool
-- debug = False

iterativeDeepening :: Int -> Maybe Int -> CtxIO (Maybe Int, [Move], Int)
iterativeDeepening depth maybeMaxNodes = do
    --when debug $ lift $ do
    --    putStrLn $ "In iter deep: " ++ show depth
    --    hFlush stdout
    chg <- readChanging
    go 1 (crtStatus chg) Nothing [] []
    where go d sini lsc lpv rmvs = do
              --when debug $ lift $ do
              --    putStrLn $ "In iter deep go: " ++ show d
              --    hFlush stdout
              (path, sc, rmvsf, _timint, sfin, _) <- bestMoveCont d 0 0 sini lsc lpv rmvs
              let nodes = fromIntegral $ sNodes $ mstats sfin
              -- We don't want to search less than depth 2, because depth 1 delivers error moves
              -- by currently not updating the best score
              if d > 1 && (null path || d >= depth || stopNodes maybeMaxNodes nodes)
                 then return (Just sc, path, nodes)
                 else go (d+1) sfin (Just sc) path rmvsf

-- We may play with or without a search nodes budget, then we must be able to
-- make some operations with ints & maybe ints
aloNodes :: Maybe Int -> Int -> Maybe Int
aloNodes Nothing   _  = Nothing
aloNodes (Just n1) n2 = Just $ n1 + n2

subNodes :: Maybe Int -> Int -> Int
subNodes Nothing   _  = 0
subNodes (Just n1) n2 = n1 - n2

stopNodes :: Maybe Int -> Int -> Bool
stopNodes Nothing _    = False
stopNodes (Just n1) n2 = n2 >= n1
