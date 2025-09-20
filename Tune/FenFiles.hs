-- {-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
-- {-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE MultiWayIf #-}

module Tune.FenFiles (
    skipWithIndex, loopCount, skipLines
) where
-- import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad (when)
-- import Data.List (intersperse)
import Foreign hiding (void)
-- import System.Console.GetOpt
import System.Directory
-- import System.Environment (getArgs)
import System.FilePath
-- import Data.Word
import System.IO

debug :: Bool
debug = False

-- Functions to work with fen files, mostly used in tuning

-- If the fen file has an index, use it to skip to the wanted line
-- The index must be created with binary IndexTxt from Bartest
skipWithIndex :: MonadIO m => String -> Handle -> Int -> m Bool
skipWithIndex fn h k = liftIO $ do
    let fni = replaceExtension fn "idx"
    fniex <- doesFileExist fni
    if fniex
       then do
           withBinaryFile fni ReadMode $ \ih ->
               allocaBytes 4 $ \ptr -> do
                   when debug $ putStrLn $ "Skip to index entry " ++ show k
                   hSeek ih AbsoluteSeek (fromIntegral k * 4)
                   rb <- hGetBuf ih ptr 4
                   if rb < 4
                      then error "Unexpected EOF in index file"
                      else do
                          wo <- peek ptr :: IO Word32
                          when debug $ putStrLn $ "Skip to file byte " ++ show wo
                          hSeek h AbsoluteSeek (fromIntegral wo)
                          return True
       else return False

-- Loop and count with a monadic action which gets 2 parameters, a number of the loop
-- (e.g. line number or file number) and a status, returning continuation & new status
-- (i.e.: when the loop has to stop, return (False, status)
loopCount :: Monad m => (Int -> a -> m (Bool, a)) -> a -> m a
loopCount act = go 1
    where go !k a = do
              (r, b) <- act k a
              if r then go (k+1) b else return b

-- Skip lines in a file in a monad
skipLines :: MonadIO m => Handle -> Int -> Int -> () -> m (Bool, ())
skipLines hi m k () = do
    end <- if k <= m then liftIO $ hIsEOF hi else return True
    if end
       then return (False, ())
       else do
           _ <- liftIO $ hGetLine hi
           when (k `mod` 100000 == 0) $ liftIO $ do
               putStrLn $ "Positions skipped: " ++ show k
               hFlush stdout
           return (True, ())
