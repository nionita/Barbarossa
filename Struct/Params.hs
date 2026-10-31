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
        -- history init of a new search (Moves.History.initHist), bits: 1 = from the compiled PST in the phase
        -- of the root position, shifted by epHistPstSh (left, or right when negative), 2 = piece order of
        -- the move generation; 0 = zero
        ("epHistPst",           3),
        ("epHistPstSh",        -4),
        -- history bonus of a cut: 0 = by the absolute depth (1 << (31 - depth)), s > 0 = by the remaining depth d
        -- (1 << (s + d), at most 1 << 30), s < 0 = half slope, per ply of the same side (1 << (-s + (d + 2) / 2))
        ("epHRem",              0),
        -- sort of the quiet moves (Moves.Internal.Base.quietSort), bits: 1 = moves of a piece to a square
        -- attacked by a cheaper enemy piece last, 2 = moves of a piece away from such a square first;
        -- 0 = by history only
        ("epQThr",              2),
        -- reverse futility pruning (Search.Core.isRevFutil): zero window nodes of a remaining depth up to
        -- epRFutD (0 = off) fail high when the static score is over beta by epRFutA + epRFutM * depth (cp),
        -- plus epRFutT (cp, 0 = off) when own pieces hang (Search.Core.ownHanging)
        ("epRFutD",             6),
        ("epRFutA",             0),
        ("epRFutM",            50),
        ("epRFutT",             0),
        -- drawishness scale of the whole eval (Eval.Core.normalParts), instead of the heuristic rules of
        -- scaleFactor when epScaleNew = 1 (default: compiled in, scale-c-lin, TUNING-HISTORY finding 37;
        -- epScaleNew = 0 is the old scaling); factors in 1/64 (64 = 1), fitted by hv_scale.py --constrained
        -- (BarbarossaTuning) on the round 3 self-play data with the old linear 50 moves scale: all <= 1,
        -- pawn factors monotone, so normal middle game positions keep factor 1
        ("epScaleNew",          1),
        ("epScP0",             22),	-- stronger side (by the sign of the eval) has 0 .. 7, 8+ pawns
        ("epScP1",             36),
        ("epScP2",             52),
        ("epScP3",             59),
        ("epScP4",             63),
        ("epScP5",             64),
        ("epScP6",             64),
        ("epScP7",             64),
        ("epScP8",             64),
        ("epScOcbOnly",        23),	-- opposite coloured bishops, no other pieces
        ("epScOcbPcs",         53),	-- opposite coloured bishops and other pieces
        ("epScOneWing",        55),	-- pawns of the stronger side on one wing only (a-d or e-h)
        ("epScRookEnd",        59),	-- one rook each, no other pieces
        ("epScQueenEnd",       50),	-- one queen each, no other pieces
        ("epSc50Pow",          -1),	-- 50 moves scale (1 - m50 / 100) ^ (epSc50Pow / 100), < 0: old linear
        ("epSc50Knee",          0),	-- > 0: 50 moves scale of two linear segments, knee at this m50
        ("epSc50KneeH",      1024)	-- ... and its height there (1/1024): (0, 1) -> (knee, h) -> (100, 0)
    ]
