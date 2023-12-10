{-# LANGUAGE TemplateHaskell #-}

-- Disclaimer: it's a nightmare to work with Template Haskell!!
-- It feels like programming in machine code in the 50's
-- But in this case it must be, otherwise the changes of the parameter records
-- when trying different parameters and weights is too time consuming

module Struct.Params (
    genEvalParams,
    genEvalWeights
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

type EvalParamSpec  = (String, Integer)
type EvalWeightSpec = (String, (Integer, Integer))

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
        ("epPassYoCtrl",        7)
    ]

-- The configurable weights we use
-- Every weight has two values, one for mid game, one for end game
-- The format is: (paramName, (defaultValueMid, defaultValueEnd))
-- All weights have type MidEnd
weights :: [EvalWeightSpec]
weights = [
        ("ewMaterialDiff",    (   8,    8)),
        ("ewKingSafe",        (   1,    0)),
        ("ewKingOpen",        (   2,    4)),
        ("ewKingPlaceCent",   (   8,    1)),
        ("ewKingPlacePwns",   (   0,    4)),
        ("ewKingPawn",        (   3,   50)),
        ("ewKingThreat",      (   0,  300)),
        ("ewRookHOpen",       ( 162,  182)),	-- DSPSA with Adadelta
        ("ewRookOpen",        ( 205,  178)),	-- 20k steps, depth 4,
        ("ewRookConn",        ( 110,   50)),
        ("ewRook7th",         ( 201,  161)),	-- niu=0.99, eps=1E-6
        ("ewMobilityKnight",  (  70,   54)),	-- texel2 / o: 62, 47
        ("ewMobilityBishop",  (  67,   32)),	-- texel2 / o: 61, 29
        ("ewMobilityRook",    (  32,   40)),	-- texel2 / o: 32, 39
        ("ewMobilityQueen",   (   4,   22)),	-- texel2 / o:  4, 22
        ("ewCenterPAtts",     (  73,   57)),
        ("ewCenterNAtts",     (  25,   20)),
        ("ewCenterBAtts",     (  25,   20)),
        ("ewCenterRAtts",     (  14,   22)),	-- DSPSA ...
        ("ewCenterQAtts",     (  13,   53)),
        ("ewCenterKAtts",     (   0,   32)),
        ("ewSpace",           (   1,    0)),
        ("ewAdvAtts",         (   0,   12)),
        ("ewWeakSq",          ( -32,    0)),
        ("ewWeakSqA",         ( 100,    0)),
        ("ewIsolPawns",       ( -36, -113)),
        ("ewIsolPassed",      ( -63, -143)),
        ("ewBackPawns",       (-108, -141)),
        ("ewBackPOpen",       ( -21,  -27)),
        ("ewEnpHanging",      ( -19,  -27)),
        ("ewEnpEnPrise",      ( -29,  -26)),
        ("ewEnpAttacked",     (  -2,  -14)),
        ("ewWepTotal",        ( 150,    0)),
        ("ewWepAttacked",     (   0,  200)),
        ("ewBishopPair",      ( 386,  323)),
        ("ewBishopPawns",     ( -25,  -54)),
        ("ewRookPawn",        ( -44,  -32)),
        ("ewAdvPawn5",        (  14,  106)),
        ("ewAdvPawn6",        ( 352,  333)),
        ("ewPawnBlockP",      (-112,  -92)),
        ("ewPawnBlockO",      ( -23,  -26)),
        ("ewPawnBlockA",      ( -19,  -69)),
        ("ewPassPawnLev",     (   2,    8))
    ]

-- This part is for generating the data structures and the CollectParams instances
data Phase = Mid | End

-- The type class, for which we want to generate an instance for any of our parameter data types
classCollectParams :: Type
classCollectParams = ConT $ mkName "CollectParams"

-- We need to derive the Show instance
derivingShow :: DerivClause
derivingShow = DerivClause Nothing [ConT ''Show]

typeFunCollectFor :: Type -> Type
typeFunCollectFor = AppT (ConT (mkName "CollectFor"))

-- This will generate a function to assign a value to a filed of the parameter data type
-- For parameters (one value), this will be: \v rec -> rec { field = round v }
-- For weights (two values), this will be:
-- for mid values: \v rec -> rec { field = field rec { mid = round v } }
-- for end values: \v rec -> rec { field = field rec { end = round v } }
genSetParamExp :: Maybe Phase -> String -> Q Exp
genSetParamExp mphase field = do
    val <- newName "v"
    rec <- newName "rec"
    let upd = case mphase of
                  Nothing  -> rval
                  Just Mid -> (RecUpdE (base rec) [(mkName "mid", rval)])
                  Just End -> (RecUpdE (base rec) [(mkName "end", rval)])
        rval = roundVal val
    return $ LamE [VarP val, VarP rec]
                  (RecUpdE (VarE rec) [(fldName, upd)])
    where fldName = mkName field
          roundVal v = AppE (VarE 'round) (VarE v)
          base r = AppE (VarE fldName) (VarE r)

-- This will generate a function to collect all parameters or weights, in form of:
-- \ (s, v) rec -> lookApply s v rec [ ("field", \v rec -> ... ) ... ]
-- The list contains one pair field name / assign function per data filed for params,
-- and 2 two pairs per data field for weights, one with the assignment for the mid
-- and one for the end value, while in this case the string will have the corresponding
-- prefix, "mid." or "end."
-- Example for the field ewKingSafe:
-- [ ...
--   ("mid.ewKingSafe", \ v rec -> rec {ewKingSafe = (ewKingSafe rec) {mid = round v}}),
--   ("end.ewKingSafe", \ v rec -> rec {ewKingSafe = (ewKingSafe rec) {end = round v}}),
--   ...
-- ]
genCollectEvalParamsExp :: [String] -> Bool -> Q Exp
genCollectEvalParamsExp names withPhase = do
    (newNames, setFs) <- if withPhase
                            then do
                                (qnsm, sfsm) <- getMidSet names
                                (qnse, sfse) <- getEndSet names
                                return (qnsm ++ qnse, sfsm ++ sfse)
                            else do
                                sfs <- mapM (genSetParamExp Nothing) names
                                return (names, sfs)
    let nfTups = zipWith (\p f -> TupE [LitE (StringL p), f]) newNames setFs
    return $ LamE [TupP [VarP strName, VarP valName], VarP recName]
                  (AppE (AppE (AppE (AppE (VarE lookApp) (VarE strName))
                                    (VarE valName))
                              (VarE recName))
                        (ListE nfTups))
    where strName = mkName "s"
          valName = mkName "v"
          recName = mkName "rec"
          lookApp = mkName "lookApply"

getMidSet :: [String] -> Q ([String], [Exp])
getMidSet names = do
    setFs <- mapM (genSetParamExp (Just Mid)) names
    let qualNames = map ("mid." ++) names
    return (qualNames, setFs)

getEndSet :: [String] -> Q ([String], [Exp])
getEndSet names = do
    setFs <- mapM (genSetParamExp (Just End)) names
    let qualNames = map ("end." ++) names
    return (qualNames, setFs)

-- Generate the parts for EvalParams
nameEvalParams :: Name
nameEvalParams = mkName "EvalParams"

genRecFieldDecP :: String -> VarStrictType
genRecFieldDecP fld = (fldName, Bang NoSourceUnpackedness SourceStrict, ConT ''Int)
    where fldName = mkName fld

genRecFieldIniP :: EvalParamSpec -> FieldExp
genRecFieldIniP (fld, val) = (fldName, fldVal)
    where fldName = mkName fld
          fldVal = LitE (IntegerL val)

-- Generate the declarations for EvalParams, something like:
-- data EvalParams = EvalParams {
--                       epKingDist :: !Int,
--                       ...
--                   }
-- instance CollectParams EvalParams where
--     type CollectFor EvalParams = EvalParams
--     npColInit = EvalParams {
--                     epKingDist = 12,
--                     ...
--                 }
--     npColParm = \ (s, v) rec -> lookApply s v rec [ ("field", \v rec -> ... ) ... ]
--     npSetParm = id
--
genEvalParams :: Q [Dec]
genEvalParams = do
    lambdaExp <- genCollectEvalParamsExp (map fst params) False
    let typeEvalParams = ConT nameEvalParams
        typeDec = TySynInstD (TySynEqn Nothing (typeFunCollectFor typeEvalParams) typeEvalParams)
        colInit = ValD (VarP $ mkName "npColInit")
                       (NormalB (RecConE nameEvalParams $ map genRecFieldIniP params)) []
        colParm = ValD (VarP $ mkName "npColParm") (NormalB lambdaExp) []
        setParm = ValD (VarP $ mkName "npSetParm") (NormalB (VarE 'id)) []
        -- Data daclaration
        d = DataD [] nameEvalParams [] Nothing
                  [RecC nameEvalParams (map (genRecFieldDecP . fst) params)]
                  [derivingShow]
        -- Instance declaration
        i = InstanceD Nothing []
                      (AppT classCollectParams typeEvalParams)
                      [typeDec, colInit, colParm, setParm]
    return [d, i]

-- Generate the parts for EvalWeights
nameEvalWeights :: Name
nameEvalWeights = mkName "EvalWeights"

genRecFieldDecW :: String -> VarStrictType
genRecFieldDecW fld = (fldName, Bang NoSourceUnpackedness SourceStrict, ConT midEndType)
    where fldName = mkName fld
          midEndType = mkName "MidEnd"

genRecFieldIniW :: EvalWeightSpec -> FieldExp
genRecFieldIniW (fld, (valM, valE)) = (fldName, fldVal)
    where fldName = mkName fld
          tme = mkName "tme"
          fldVal = AppE (AppE (VarE tme) (LitE (IntegerL valM))) (LitE (IntegerL valE))

genEvalWeights :: Q [Dec]
genEvalWeights = do
    lambdaExp <- genCollectEvalParamsExp (map fst weights) True
    let typeEvalWeights = ConT nameEvalWeights
        typeDec = TySynInstD (TySynEqn Nothing (typeFunCollectFor typeEvalWeights) typeEvalWeights)
        colInit = ValD (VarP $ mkName "npColInit")
                       (NormalB (RecConE nameEvalWeights $ map genRecFieldIniW weights)) []
        colParm = ValD (VarP $ mkName "npColParm") (NormalB lambdaExp) []
        setParm = ValD (VarP $ mkName "npSetParm") (NormalB (VarE 'id)) []
        -- Data declaration
        d = DataD [] nameEvalWeights [] Nothing
                  [RecC nameEvalWeights (map (genRecFieldDecW . fst) weights)]
                  [derivingShow]
        -- Instance declaration
        i = InstanceD Nothing []
                      (AppT classCollectParams typeEvalWeights)
                      [typeDec, colInit, colParm, setParm]
    return [d, i]
