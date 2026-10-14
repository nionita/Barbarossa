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
        ("epFutil6",          768)
    ]
