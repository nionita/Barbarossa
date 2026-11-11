-- Diagnostic counters of the search (global, for single process measurements like SelfPlay -P):
-- nodes per LMR level and the late move pruning shadow (moves which would be pruned and are searched anyway)
module Search.Diag (
    diagLevel, diagLimited, diagShadow, diagDump, diagReport
) where

import Control.Monad (forM, when)
import Data.Array.IO
import Data.Int (Int32)
import Data.IORef
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

-- Layout: 0 .. 15 nodes by LMR level, 16 + d zero window nodes with an LMP limit of remaining depth d (1 .. 7),
-- 24 + 8 * d + k moves which LMP would prune (k = moves over the limit, 0 .. 7+), 88 + 8 * d + k of them fail high
counters :: IOUArray Int Int
counters = unsafePerformIO $ newArray (0, 151) 0
{-# NOINLINE counters #-}

-- Number of the current zero window node with an LMP limit, and the dump file of the shadow moves
nodeNo :: IORef Int
nodeNo = unsafePerformIO $ newIORef 0
{-# NOINLINE nodeNo #-}

dumpH :: IORef (Maybe Handle)
dumpH = unsafePerformIO $ newIORef Nothing
{-# NOINLINE dumpH #-}

incr :: Int -> IO ()
incr i = readArray counters i >>= writeArray counters i . (+ 1)

diagLevel :: Int -> IO ()
diagLevel = incr

diagLimited :: Int -> IO ()
diagLimited d = when (d >= 1 && d <= 7) $ do
    incr (16 + d)
    modifyIORef' nodeNo (+ 1)

-- One line per shadow move in lmp-shadow.csv (current directory): node, depth, moves over the limit, history,
-- continuation rows by the move before and by the own move before, fail high
diagDump :: Int -> Int -> Int32 -> Int32 -> Int32 -> Bool -> IO ()
diagDump d k h c1 c2 fh = do
    mh <- readIORef dumpH
    hd <- case mh of
        Just hd -> return hd
        Nothing -> do
            hd <- openFile "lmp-shadow.csv" WriteMode
            hSetBuffering hd (BlockBuffering Nothing)
            hPutStrLn hd "node,d,k,h,c1,c2,fh"
            writeIORef dumpH (Just hd)
            return hd
    n <- readIORef nodeNo
    hPutStrLn hd $ show n ++ "," ++ show d ++ "," ++ show k ++ "," ++ show h ++ "," ++ show c1 ++ "," ++ show c2
        ++ "," ++ (if fh then "1" else "0")

-- Remaining depth, moves over the limit, fail high
diagShadow :: Int -> Int -> Bool -> IO ()
diagShadow d k fh = when (d >= 1 && d <= 7) $ do
    let i = 8 * d + min 7 (max 0 k)
    incr (24 + i)
    when fh $ incr (88 + i)

diagReport :: IO ()
diagReport = do
    readIORef dumpH >>= mapM_ hClose
    vs <- getElems counters
    let lv = take 16 vs
        tl = sum lv
    when (tl > 0) $ do
        putStrLn "Nodes by LMR level:"
        _ <- forM (zip [0 :: Int ..] lv) $ \(l, n) ->
            when (n > 0) $ printf "  level %2d: %10d %6.2f%%\n" l n (100 * fromIntegral n / fromIntegral tl :: Double)
        printf "  mean level %.3f\n" (sum (zipWith (*) [0 ..] (map fromIntegral lv)) / fromIntegral tl :: Double)
    let sh = sum (take 64 (drop 24 vs))
    when (sh > 0) $ do
        putStrLn "LMP shadow: depth, moves over the limit (k), prunable moves, fail high, rate"
        _ <- forM [1 .. 7] $ \d -> do
            let lim = vs !! (16 + d)
                pr  = [vs !! (24 + 8 * d + k) | k <- [0 .. 7]]
                fh  = [vs !! (88 + 8 * d + k) | k <- [0 .. 7]]
            when (sum pr > 0) $ do
                printf "  d %d: nodes with limit %d, prunable %d, fail high %d (%.3f%%)\n" d lim (sum pr) (sum fh)
                    (100 * fromIntegral (sum fh) / fromIntegral (sum pr) :: Double)
                _ <- forM (zip3 [0 :: Int ..] pr fh) $ \(k, p, f) ->
                    when (p > 0) $ printf "    k %d%s: %10d %8d %7.3f%%\n" k (if k == 7 then "+" else "") p f
                        (100 * fromIntegral f / fromIntegral p :: Double)
                return ()
        return ()
