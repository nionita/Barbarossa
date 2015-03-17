{-# LANGUAGE BangPatterns #-}

module Search.AlbetaTypes (
    DoResult,
    dorIllegal, isDoRIllegal,
    dorRepet, isDoRRepet,
    dorNormal,
    setDoRExten, getDoRExten,
    setDoRSpecial, isDoRSpecial,
    setDoRInCheck, isDoRInCheck,
    setDoRChecking, isDoRChecking,
    setDoRTreat, isDoRTreat,
    Comm(..),
    ABControl(..)
) where

import Data.Bits
import Data.Word

import Struct.Struct

data ABControl = ABC {
        maxdepth  :: Int,
        lastpv    :: [Move],
        lastscore :: Maybe Int,
        rootmvs   :: [Move],
        window    :: Int,
        best      :: Bool,
        stoptime  :: Int
    } deriving Show

data Comm = LogMes String
          | BestMv Int Int Int [Move]
          | CurrMv Move Int
          | InfoStr String

-- Do move returns some flags in a word32
data DoResult = DoResult !Word32 deriving Eq

setDoRExten, setDoRSpecial, setDoRInCheck, setDoRChecking, setDoRTreat :: Bool -> DoResult -> DoResult
isDoRIllegal, isDoRRepet, isDoRSpecial, isDoRInCheck, isDoRChecking, isDoRTreat :: DoResult -> Bool
getDoRExten :: DoResult -> Int

dorNormal, dorIllegal, dorRepet :: DoResult
dorNormal  = DoResult    0
dorIllegal = DoResult 1024
dorRepet   = DoResult  512

{-# INLINE isDoRIllegal #-}
isDoRIllegal = (==) dorIllegal

{-# INLINE isDoRRepet #-}
isDoRRepet = (==) dorRepet

{-# INLINE setDoRExten #-}
setDoRExten b d@(DoResult w)
    | b         = DoResult $ w .|. 1	-- only 1 or 0 for now
    | otherwise = d

{-# INLINE getDoRExten #-}
getDoRExten (DoResult w) = fromIntegral $ w .&. 3

{-# INLINE setDoRSpecial #-}
setDoRSpecial b d@(DoResult w)
    | b         = DoResult $ w .|. 4
    | otherwise = d

{-# INLINE isDoRSpecial #-}
isDoRSpecial (DoResult w) = w .&. 4 /= 0

{-# INLINE setDoRInCheck #-}
setDoRInCheck b d@(DoResult w)
    | b         = DoResult $ w .|. 8
    | otherwise = d

{-# INLINE isDoRInCheck #-}
isDoRInCheck (DoResult w) = w .&. 8 /= 0

{-# INLINE setDoRChecking #-}
setDoRChecking b d@(DoResult w)
    | b         = DoResult $ w .|. 16
    | otherwise = d

{-# INLINE isDoRChecking #-}
isDoRChecking (DoResult w) = w .&. 16 /= 0

{-# INLINE setDoRTreat #-}
setDoRTreat b d@(DoResult w)
    | b         = DoResult $ w .|. 32
    | otherwise = d

{-# INLINE isDoRTreat #-}
isDoRTreat (DoResult w) = w .&. 32 /= 0
