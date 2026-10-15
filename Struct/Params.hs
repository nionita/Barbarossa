module Struct.Params (
    params,
) where

import Struct.ParamsPre

-- The configurable parameters we use
-- Every parameter has only one value (i.e. it does not depend on game phase)
-- The format is: (paramName, defaultValue)
-- All parameters have type Int
params :: [EvalParamSpec]
params = [
        ("epMovingMid",       156),
        ("epMovingEnd",       156),
        ("epMaterMinor",        1),
        ("epMaterRook",         4),
        ("epMaterQueen",       13),
        ("epMaterScale",        1),
        ("epMaterBonusScale",   5),
        ("epPawnBonusScale",    1),
        ("epPassA",            12),
        ("epPassB",           -48),
        ("epPassC",            98),
        ("epPassKingProx",     12),
        ("epPassBlockO",       11),
        ("epPassBlockA",       17),
        ("epPassMin",          30),
        ("epPassMyCtrl",        6),
        ("epPassYoCtrl",        7),
        -- futility margins (cp) of the remaining depths 1 .. 6 (Search.Core, maxFutilDepth)
        ("epFutil1",           24),
        ("epFutil2",           48),
        ("epFutil3",           96),
        ("epFutil4",          192),
        ("epFutil5",          384),
        ("epFutil6",          768),
        -- drawishness scale of the whole eval (Eval.Core.normalParts), instead of the heuristic rules of
        -- scaleFactor and the linear 50 moves scale when epScaleNew = 1; factors in 1/64 (64 = 1),
        -- fitted by hv_scale.py (BarbarossaTuning) on the round 3 self-play data; the pawn factors are
        -- normalized (x 148 / 134, the fitted sigmoid scales old / new) so that the eval keeps its
        -- calibration (the same k)
        ("epScaleNew",          0),
        ("epScP0",             36),	-- stronger side (by the sign of the eval) has 0 .. 7, 8+ pawns
        ("epScP1",             51),
        ("epScP2",             69),
        ("epScP3",             76),
        ("epScP4",             81),
        ("epScP5",             82),
        ("epScP6",             82),
        ("epScP7",             77),
        ("epScP8",             71),
        ("epScOcbOnly",        26),	-- opposite coloured bishops, no other pieces
        ("epScOcbPcs",         53),	-- opposite coloured bishops and other pieces
        ("epScOneWing",        57),	-- pawns of the stronger side on one wing only (a-d or e-h)
        ("epScRookEnd",        59),	-- one rook each, no other pieces
        ("epScQueenEnd",       52),	-- one queen each, no other pieces
        ("epSc50Pow",         220)	-- 50 moves scale (1 - m50 / 100) ^ (epSc50Pow / 100), < 0: old linear
    ]
