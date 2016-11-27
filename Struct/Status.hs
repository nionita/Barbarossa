{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Struct.Status (
    Stats(..),
    MyState(..),
    EvalState(..),
    EvalParams(..),
    EvalWeights(..),
    MidEnd(..),
    mad, madm, made, tme
) where

import Struct.Struct
import Struct.Config
import Moves.History
import Hash.TransTab

data Stats = Stats {
        nodes :: !Int,
        maxmvs :: !Int
    } deriving Show

data MyState = MyState {
        stack :: [MyPos],	-- stack of played positions
        hash  :: Cache,		-- transposition table
        hist  :: History,	-- history table
        stats :: !Stats,	-- statistics
        evalst :: EvalState	-- eval status (parameter & statistics)
    }

data EvalState = EvalState {
        esEParams   :: EvalParams,
        esEWeights  :: EvalWeights
    } deriving Show

data MidEnd = MidEnd { mid, end :: !Int } deriving Show

-- Helper for MidEnd operations:
{-# INLINE madm #-}
madm :: MidEnd -> MidEnd -> Int -> MidEnd
madm !mide0 !mide !v = mide0 { mid = mid mide0 + mid mide * v }

{-# INLINE made #-}
made :: MidEnd -> MidEnd -> Int -> MidEnd
made !mide0 !mide !v = mide0 { end = end mide0 + end mide * v }

{-# INLINE mad #-}
mad :: MidEnd -> MidEnd -> Int -> MidEnd
mad !mide0 !mide !v = MidEnd { mid = mid mide0 + mid mide * v, end = end mide0 + end mide * v }

-- {-# INLINE (<+>) #-}
-- (<+>) :: MidEnd -> MidEnd -> MidEnd
-- mide1 <+> mide2 = MidEnd { mid = mid mide1 + mid mide2, end = end mide1 + end mide2 }

{-# INLINE tme #-}
tme :: Int -> Int -> MidEnd
tme a b = MidEnd a b

-- This is the parameter record for characteristics evaluation
data EvalParams
    = EvalParams {
          epMovingMid  :: !Int,
          epMovingEnd  :: !Int,
          epMaterMinor :: !Int,
          epMaterRook  :: !Int,
          epMaterQueen :: !Int,
          epMaterScale :: !Int,
          epMaterBonusScale :: !Int,
          epPawnBonusScale  :: !Int,
          epPassKingProx    :: !Int,
          epPassBlockO :: !Int,
          epPassBlockA :: !Int,
          epPassMin    :: !Int,
          epPassMyCtrl :: !Int,
          epPassYoCtrl :: !Int
      } deriving Show

data EvalWeights
    = EvalWeights {
          ewMaterialDiff    :: !MidEnd,
          ewKingSafe        :: !MidEnd,
          ewKingOpen        :: !MidEnd,
          ewKingPlaceCent   :: !MidEnd,
          ewKingPlacePwns   :: !MidEnd,
          ewRookHOpen       :: !MidEnd,
          ewRookOpen        :: !MidEnd,
          ewRookConn        :: !MidEnd,
          ewMobilityKnight  :: !MidEnd,
          ewMobilityBishop  :: !MidEnd,
          ewMobilityRook    :: !MidEnd,
          ewMobilityQueen   :: !MidEnd,
          ewCenterPAtts     :: !MidEnd,
          ewCenterNAtts     :: !MidEnd,
          ewCenterBAtts     :: !MidEnd,
          ewCenterRAtts     :: !MidEnd,
          ewCenterQAtts     :: !MidEnd,
          ewCenterKAtts     :: !MidEnd,
          ewSpace           :: !MidEnd,
          ewAdvAtts         :: !MidEnd,
          ewIsolPawns       :: !MidEnd,
          ewIsolPassed      :: !MidEnd,
          ewBackPawns       :: !MidEnd,
          ewBackPOpen       :: !MidEnd,
          ewEnpHanging      :: !MidEnd,
          ewEnpEnPrise      :: !MidEnd,
          ewEnpAttacked     :: !MidEnd,
          ewLastLinePenalty :: !MidEnd,
          ewBishopPair      :: !MidEnd,
          ewRedundanceRook  :: !MidEnd,
          ewRookPawn        :: !MidEnd,
          ewAdvPawn6        :: !MidEnd,
          ewAdvPawn5        :: !MidEnd,
          ewPawnBlockP      :: !MidEnd,
          ewPawnBlockO      :: !MidEnd,
          ewPawnBlockA      :: !MidEnd,
          ewPassPawnLev     :: !MidEnd
      } deriving Show

instance CollectParams EvalParams where
    type CollectFor EvalParams = EvalParams
    npColInit = EvalParams {
                    epMovingMid  = 8,		-- SGD with combined target
                    epMovingEnd  = 8,		-- SGD with combined target
                    epMaterMinor = 1,
                    epMaterRook  = 4,
                    epMaterQueen = 13,
                    epMaterScale = 1,
                    epMaterBonusScale = 5,
                    epPawnBonusScale  = 1,
                    epPassKingProx    = 12,	-- max after ~12k Clop games (ELO +23 +- 12)
                    epPassBlockO = 11,
                    epPassBlockA = 17,
                    epPassMin    = 30,
                    epPassMyCtrl = 6,
                    epPassYoCtrl = 7
                }
    npColParm = collectEvalParams
    npSetParm = id

collectEvalParams :: (String, Double) -> EvalParams -> EvalParams
collectEvalParams (s, v) ep = lookApply s v ep [
        ("epMovingMid",       setEpMovingMid),
        ("epMovingEnd",       setEpMovingEnd),
        ("epMaterMinor",      setEpMaterMinor),
        ("epMaterRook",       setEpMaterRook),
        ("epMaterQueen",      setEpMaterQueen),
        ("epMaterScale",      setEpMaterScale),
        ("epMaterBonusScale", setEpMaterBonusScale),
        ("epPawnBonusScale",  setEpPawnBonusScale),
        ("epPassKingProx",    setEpPassKingProx),
        ("epPassBlockO",      setEpPassBlockO),
        ("epPassBlockA",      setEpPassBlockA),
        ("epPassMin",         setEpPassMin),
        ("epPassMyCtrl",      setEpPassMyCtrl),
        ("epPassYoCtrl",      setEpPassYoCtrl)
    ]
    where setEpMovingMid       v' ep' = ep' { epMovingMid       = round v' }
          setEpMovingEnd       v' ep' = ep' { epMovingEnd       = round v' }
          setEpMaterMinor      v' ep' = ep' { epMaterMinor      = round v' }
          setEpMaterRook       v' ep' = ep' { epMaterRook       = round v' }
          setEpMaterQueen      v' ep' = ep' { epMaterQueen      = round v' }
          setEpMaterScale      v' ep' = ep' { epMaterScale      = round v' }
          setEpMaterBonusScale v' ep' = ep' { epMaterBonusScale = round v' }
          setEpPawnBonusScale  v' ep' = ep' { epPawnBonusScale  = round v' }
          setEpPassKingProx    v' ep' = ep' { epPassKingProx    = round v' }
          setEpPassBlockO      v' ep' = ep' { epPassBlockO      = round v' }
          setEpPassBlockA      v' ep' = ep' { epPassBlockA      = round v' }
          setEpPassMin         v' ep' = ep' { epPassMin         = round v' }
          setEpPassMyCtrl      v' ep' = ep' { epPassMyCtrl      = round v' }
          setEpPassYoCtrl      v' ep' = ep' { epPassYoCtrl      = round v' }

instance CollectParams EvalWeights where
    type CollectFor EvalWeights = EvalWeights
    npColInit = EvalWeights {
          ewMaterialDiff    = tme 8 8,
          ewKingSafe        = tme 1 0,
          ewKingOpen        = tme 5 0,
          ewKingPlaceCent   = tme 6 0,
          ewKingPlacePwns   = tme 0 6,
          ewRookHOpen       = tme 182 176,
          ewRookOpen        = tme 207 229,
          ewRookConn        = tme 106 55,
          ewMobilityKnight  = tme 51 62,
          ewMobilityBishop  = tme 58 27,
          ewMobilityRook    = tme 24 31,
          ewMobilityQueen   = tme 2 13,
          ewCenterPAtts     = tme 81 71,
          ewCenterNAtts     = tme 49 51,
          ewCenterBAtts     = tme 56 46,
          ewCenterRAtts     = tme 12 36,
          ewCenterQAtts     = tme 11 50,
          ewCenterKAtts     = tme (-4) 43,
          ewSpace           = tme  1 0,
          ewAdvAtts         = tme  6 15,
          ewIsolPawns       = tme (-42) (-119),
          ewIsolPassed      = tme (-155) (-84),
          ewBackPawns       = tme (-157) (-137),
          ewBackPOpen       = tme 19 (-76),
          ewEnpHanging      = tme (-24) (-34),
          ewEnpEnPrise      = tme (-34) (-4),
          ewEnpAttacked     = tme 7 (-24),
          ewLastLinePenalty = tme 116 (-12),
          ewBishopPair      = tme 384 310,
          ewRedundanceRook  = tme 14 (-135),
          ewRookPawn        = tme (-44) (-37),
          ewAdvPawn5        = tme (-1) 129,
          ewAdvPawn6        = tme 443 356,
          ewPawnBlockP      = tme (-139) (-113),
          ewPawnBlockO      = tme (-32) (-6),
          ewPawnBlockA      = tme (-23) (-56),
          ewPassPawnLev     = tme 0 8
        }
    npColParm = collectEvalWeights
    npSetParm = id

collectEvalWeights :: (String, Double) -> EvalWeights -> EvalWeights
collectEvalWeights (s, v) ew = lookApply s v ew [
        ("mid.kingSafe",       setMidKingSafe),
        ("end.kingSafe",       setEndKingSafe),
        ("mid.kingOpen",       setMidKingOpen),
        ("end.kingOpen",       setEndKingOpen),
        ("mid.kingPlaceCent",  setMidKingPlaceCent),
        ("end.kingPlaceCent",  setEndKingPlaceCent),
        ("mid.kingPlacePwns",  setMidKingPlacePwns),
        ("end.kingPlacePwns",  setEndKingPlacePwns),
        ("mid.rookHOpen",      setMidRookHOpen),
        ("end.rookHOpen",      setEndRookHOpen),
        ("mid.rookOpen",       setMidRookOpen),
        ("end.rookOpen",       setEndRookOpen),
        ("mid.rookConn",       setMidRookConn),
        ("end.rookConn",       setEndRookConn),
        ("mid.mobilityKnight", setMidMobilityKnight),
        ("end.mobilityKnight", setEndMobilityKnight),
        ("mid.mobilityBishop", setMidMobilityBishop),
        ("end.mobilityBishop", setEndMobilityBishop),
        ("mid.mobilityRook",   setMidMobilityRook),
        ("end.mobilityRook",   setEndMobilityRook),
        ("mid.mobilityQueen",  setMidMobilityQueen),
        ("end.mobilityQueen",  setEndMobilityQueen),
        ("mid.centerPAtts",    setMidCenterPAtts),
        ("end.centerPAtts",    setEndCenterPAtts),
        ("mid.centerNAtts",    setMidCenterNAtts),
        ("end.centerNAtts",    setEndCenterNAtts),
        ("mid.centerBAtts",    setMidCenterBAtts),
        ("end.centerBAtts",    setEndCenterBAtts),
        ("mid.centerRAtts",    setMidCenterRAtts),
        ("end.centerRAtts",    setEndCenterRAtts),
        ("mid.centerQAtts",    setMidCenterQAtts),
        ("end.centerQAtts",    setEndCenterQAtts),
        ("mid.centerKAtts",    setMidCenterKAtts),
        ("end.centerKAtts",    setEndCenterKAtts),
        ("mid.space",          setMidSpace),
        ("end.space",          setEndSpace),
        ("mid.adversAtts",     setMidAdvAtts),
        ("end.adversAtts",     setEndAdvAtts),
        ("mid.isolPawns",      setMidIsolPawns),
        ("end.isolPawns",      setEndIsolPawns),
        ("mid.isolPassed",     setMidIsolPassed),
        ("end.isolPassed",     setEndIsolPassed),
        ("mid.backPawns",      setMidBackPawns),
        ("end.backPawns",      setEndBackPawns),
        ("mid.backPOpen",      setMidBackPOpen),
        ("end.backPOpen",      setEndBackPOpen),
        ("mid.enpHanging",     setMidEnpHanging),
        ("end.enpHanging",     setEndEnpHanging),
        ("mid.enpEnPrise",     setMidEnpEnPrise),
        ("end.enpEnPrise",     setEndEnpEnPrise),
        ("mid.enpAttacked",    setMidEnpAttacked),
        ("end.enpAttacked",    setEndEnpAttacked),
        ("mid.lastLinePenalty", setMidLastLinePenalty),
        ("end.lastLinePenalty", setEndLastLinePenalty),
        ("mid.bishopPair",      setMidBishopPair),
        ("end.bishopPair",      setEndBishopPair),
        ("mid.redundanceRook",  setMidRedundanceRook),
        ("end.redundanceRook",  setEndRedundanceRook),
        ("mid.rookPawn",        setMidRookPawn),
        ("end.rookPawn",        setEndRookPawn),
        ("mid.advPawn5",        setMidAdvPawn5),
        ("end.advPawn5",        setEndAdvPawn5),
        ("mid.advPawn6",        setMidAdvPawn6),
        ("end.advPawn6",        setEndAdvPawn6),
        ("mid.pawnBlockP",      setMidPawnBlockP),
        ("end.pawnBlockP",      setEndPawnBlockP),
        ("mid.pawnBlockO",      setMidPawnBlockO),
        ("end.pawnBlockO",      setEndPawnBlockO),
        ("mid.pawnBlockA",      setMidPawnBlockA),
        ("end.pawnBlockA",      setEndPawnBlockA),
        ("mid.passPawnLev",     setMidPassPawnLev),
        ("end.passPawnLev",     setEndPassPawnLev)
    ]
    where setMidKingSafe        v' ew' = ew' { ewKingSafe        = (ewKingSafe        ew') { mid = round v' }}
          setEndKingSafe        v' ew' = ew' { ewKingSafe        = (ewKingSafe        ew') { end = round v' }}
          setMidKingOpen        v' ew' = ew' { ewKingOpen        = (ewKingOpen        ew') { mid = round v' }}
          setEndKingOpen        v' ew' = ew' { ewKingOpen        = (ewKingOpen        ew') { end = round v' }}
          setMidKingPlaceCent   v' ew' = ew' { ewKingPlaceCent   = (ewKingPlaceCent   ew') { mid = round v' }}
          setEndKingPlaceCent   v' ew' = ew' { ewKingPlaceCent   = (ewKingPlaceCent   ew') { end = round v' }}
          setMidKingPlacePwns   v' ew' = ew' { ewKingPlacePwns   = (ewKingPlacePwns   ew') { mid = round v' }}
          setEndKingPlacePwns   v' ew' = ew' { ewKingPlacePwns   = (ewKingPlacePwns   ew') { end = round v' }}
          setMidRookHOpen       v' ew' = ew' { ewRookHOpen       = (ewRookHOpen       ew') { mid = round v' }}
          setEndRookHOpen       v' ew' = ew' { ewRookHOpen       = (ewRookHOpen       ew') { end = round v' }}
          setMidRookOpen        v' ew' = ew' { ewRookOpen        = (ewRookOpen        ew') { mid = round v' }}
          setEndRookOpen        v' ew' = ew' { ewRookOpen        = (ewRookOpen        ew') { end = round v' }}
          setMidRookConn        v' ew' = ew' { ewRookConn        = (ewRookConn        ew') { mid = round v' }}
          setEndRookConn        v' ew' = ew' { ewRookConn        = (ewRookConn        ew') { end = round v' }}
          setMidMobilityKnight  v' ew' = ew' { ewMobilityKnight  = (ewMobilityKnight  ew') { mid = round v' }}
          setEndMobilityKnight  v' ew' = ew' { ewMobilityKnight  = (ewMobilityKnight  ew') { end = round v' }}
          setMidMobilityBishop  v' ew' = ew' { ewMobilityBishop  = (ewMobilityBishop  ew') { mid = round v' }}
          setEndMobilityBishop  v' ew' = ew' { ewMobilityBishop  = (ewMobilityBishop  ew') { end = round v' }}
          setMidMobilityRook    v' ew' = ew' { ewMobilityRook    = (ewMobilityRook    ew') { mid = round v' }}
          setEndMobilityRook    v' ew' = ew' { ewMobilityRook    = (ewMobilityRook    ew') { end = round v' }}
          setMidMobilityQueen   v' ew' = ew' { ewMobilityQueen   = (ewMobilityQueen   ew') { mid = round v' }}
          setEndMobilityQueen   v' ew' = ew' { ewMobilityQueen   = (ewMobilityQueen   ew') { end = round v' }}
          setMidCenterPAtts     v' ew' = ew' { ewCenterPAtts     = (ewCenterPAtts     ew') { mid = round v' }}
          setEndCenterPAtts     v' ew' = ew' { ewCenterPAtts     = (ewCenterPAtts     ew') { end = round v' }}
          setMidCenterNAtts     v' ew' = ew' { ewCenterNAtts     = (ewCenterNAtts     ew') { mid = round v' }}
          setEndCenterNAtts     v' ew' = ew' { ewCenterNAtts     = (ewCenterNAtts     ew') { end = round v' }}
          setMidCenterBAtts     v' ew' = ew' { ewCenterBAtts     = (ewCenterBAtts     ew') { mid = round v' }}
          setEndCenterBAtts     v' ew' = ew' { ewCenterBAtts     = (ewCenterBAtts     ew') { end = round v' }}
          setMidCenterRAtts     v' ew' = ew' { ewCenterRAtts     = (ewCenterRAtts     ew') { mid = round v' }}
          setEndCenterRAtts     v' ew' = ew' { ewCenterRAtts     = (ewCenterRAtts     ew') { end = round v' }}
          setMidCenterQAtts     v' ew' = ew' { ewCenterQAtts     = (ewCenterQAtts     ew') { mid = round v' }}
          setEndCenterQAtts     v' ew' = ew' { ewCenterQAtts     = (ewCenterQAtts     ew') { end = round v' }}
          setMidCenterKAtts     v' ew' = ew' { ewCenterKAtts     = (ewCenterKAtts     ew') { mid = round v' }}
          setEndCenterKAtts     v' ew' = ew' { ewCenterKAtts     = (ewCenterKAtts     ew') { end = round v' }}
          setMidSpace           v' ew' = ew' { ewSpace           = (ewSpace           ew') { mid = round v' }}
          setEndSpace           v' ew' = ew' { ewSpace           = (ewSpace           ew') { end = round v' }}
          setMidAdvAtts         v' ew' = ew' { ewAdvAtts         = (ewAdvAtts         ew') { mid = round v' }}
          setEndAdvAtts         v' ew' = ew' { ewAdvAtts         = (ewAdvAtts         ew') { end = round v' }}
          setMidIsolPawns       v' ew' = ew' { ewIsolPawns       = (ewIsolPawns       ew') { mid = round v' }}
          setEndIsolPawns       v' ew' = ew' { ewIsolPawns       = (ewIsolPawns       ew') { end = round v' }}
          setMidIsolPassed      v' ew' = ew' { ewIsolPassed      = (ewIsolPassed      ew') { mid = round v' }}
          setEndIsolPassed      v' ew' = ew' { ewIsolPassed      = (ewIsolPassed      ew') { end = round v' }}
          setMidBackPawns       v' ew' = ew' { ewBackPawns       = (ewBackPawns       ew') { mid = round v' }}
          setEndBackPawns       v' ew' = ew' { ewBackPawns       = (ewBackPawns       ew') { end = round v' }}
          setMidBackPOpen       v' ew' = ew' { ewBackPOpen       = (ewBackPOpen       ew') { mid = round v' }}
          setEndBackPOpen       v' ew' = ew' { ewBackPOpen       = (ewBackPOpen       ew') { end = round v' }}
          setMidEnpHanging      v' ew' = ew' { ewEnpHanging      = (ewEnpHanging      ew') { mid = round v' }}
          setEndEnpHanging      v' ew' = ew' { ewEnpHanging      = (ewEnpHanging      ew') { end = round v' }}
          setMidEnpEnPrise      v' ew' = ew' { ewEnpEnPrise      = (ewEnpEnPrise      ew') { mid = round v' }}
          setEndEnpEnPrise      v' ew' = ew' { ewEnpEnPrise      = (ewEnpEnPrise      ew') { end = round v' }}
          setMidEnpAttacked     v' ew' = ew' { ewEnpAttacked     = (ewEnpAttacked     ew') { mid = round v' }}
          setEndEnpAttacked     v' ew' = ew' { ewEnpAttacked     = (ewEnpAttacked     ew') { end = round v' }}
          setMidLastLinePenalty v' ew' = ew' { ewLastLinePenalty = (ewLastLinePenalty ew') { mid = round v' }}
          setEndLastLinePenalty v' ew' = ew' { ewLastLinePenalty = (ewLastLinePenalty ew') { end = round v' }}
          setMidBishopPair      v' ew' = ew' { ewBishopPair      = (ewBishopPair      ew') { mid = round v' }}
          setEndBishopPair      v' ew' = ew' { ewBishopPair      = (ewBishopPair      ew') { end = round v' }}
          setMidRedundanceRook  v' ew' = ew' { ewRedundanceRook  = (ewRedundanceRook  ew') { mid = round v' }}
          setEndRedundanceRook  v' ew' = ew' { ewRedundanceRook  = (ewRedundanceRook  ew') { end = round v' }}
          setMidRookPawn        v' ew' = ew' { ewRookPawn        = (ewRookPawn        ew') { mid = round v' }}
          setEndRookPawn        v' ew' = ew' { ewRookPawn        = (ewRookPawn        ew') { end = round v' }}
          setMidAdvPawn6        v' ew' = ew' { ewAdvPawn6        = (ewAdvPawn6        ew') { mid = round v' }}
          setEndAdvPawn6        v' ew' = ew' { ewAdvPawn6        = (ewAdvPawn6        ew') { end = round v' }}
          setMidAdvPawn5        v' ew' = ew' { ewAdvPawn5        = (ewAdvPawn5        ew') { mid = round v' }}
          setEndAdvPawn5        v' ew' = ew' { ewAdvPawn5        = (ewAdvPawn5        ew') { end = round v' }}
          setMidPawnBlockP      v' ew' = ew' { ewPawnBlockP      = (ewPawnBlockP      ew') { mid = round v' }}
          setEndPawnBlockP      v' ew' = ew' { ewPawnBlockP      = (ewPawnBlockP      ew') { end = round v' }}
          setMidPawnBlockO      v' ew' = ew' { ewPawnBlockO      = (ewPawnBlockO      ew') { mid = round v' }}
          setEndPawnBlockO      v' ew' = ew' { ewPawnBlockO      = (ewPawnBlockO      ew') { end = round v' }}
          setMidPawnBlockA      v' ew' = ew' { ewPawnBlockA      = (ewPawnBlockA      ew') { mid = round v' }}
          setEndPawnBlockA      v' ew' = ew' { ewPawnBlockA      = (ewPawnBlockA      ew') { end = round v' }}
          setMidPassPawnLev     v' ew' = ew' { ewPassPawnLev     = (ewPassPawnLev     ew') { mid = round v' }}
          setEndPassPawnLev     v' ew' = ew' { ewPassPawnLev     = (ewPassPawnLev     ew') { end = round v' }}
