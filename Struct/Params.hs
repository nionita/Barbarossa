{-# LANGUAGE TemplateHaskell #-}

module Struct.Params (
    genEvalParams,
    genEvalWeights
) where

-- import Data.Maybe (fromJust)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- import Struct.MidEnd

type EvalParamSpec = (String, Integer)
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
        ("ewKingPawn1",       (   4,   53)),
        ("ewKingPawn2",       (   2,   68)),
        ("ewRookHOpen",       ( 162,  182)),	-- DSPSA with Adadelta
        ("ewRookOpen",        ( 205,  178)),	-- 20k steps, depth 4,
        ("ewRookConn",        (  89,   59)),	-- 2 games, beta=0.95, gamma=0.8,
        ("ewRook7th",         ( 201,  161)),	-- niu=0.99, eps=1E-6
        ("ewMobilityKnight",  (  50,   56)),
        ("ewMobilityBishop",  (  53,   33)),
        ("ewMobilityRook",    (  16,   34)),	-- DSPSA ...
        ("ewMobilityQueen",   (   2,   11)),
        ("ewCenterPAtts",     (  73,   57)),
        ("ewCenterNAtts",     (  48,   37)),
        ("ewCenterBAtts",     (  52,   35)),
        ("ewCenterRAtts",     (  14,   22)),	-- DSPSA ...
        ("ewCenterQAtts",     (  13,   53)),
        ("ewCenterKAtts",     (   2,   62)),
        ("ewSpace",           (   1,    0)),
        ("ewAdvAtts",         (   1,   17)),
        ("ewIsolPawns",       ( -36, -113)),
        ("ewIsolPassed",      ( -63, -143)),
        ("ewBackPawns",       (-108, -141)),
        ("ewBackPOpen",       ( -21,  -27)),
        ("ewEnpHanging",      ( -19,  -27)),
        ("ewEnpEnPrise",      ( -29,  -26)),
        ("ewEnpAttacked",     (  -2,  -14)),
        ("ewWepAttacked",     (  35,   73)),
        ("ewLastLinePenalty", ( 100,    0)),
        ("ewBishopPair",      ( 386,  323)),
        ("ewBishopPawns",     ( -25,  -54)),
        ("ewRedundanceRook",  ( -27,  -51)),	-- DSPSA ...
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
evalParams, collectParams :: Name
evalParams    = mkName "EvalParams"
collectParams = mkName "CollectParams"

genRecFieldDecP :: String -> VarStrictType
genRecFieldDecP fld = (fldName, IsStrict, ConT ''Int)
    where fldName = mkName fld

genRecFieldIniP :: EvalParamSpec -> FieldExp
genRecFieldIniP (fld, val) = (fldName, fldVal)
    where fldName = mkName fld
          fldVal = LitE (IntegerL val)

genEvalParams :: Q [Dec]
genEvalParams = do
    bodyExp <- genCollectEvalParamsExp (map fst params) False
    let colParm = ValD (VarP $ mkName "npColParm") (NormalB bodyExp) []
        i = InstanceD [] (AppT theClass theInst) [ typeDec, colInit, colParm, setParm ]
    return [d, i]
    where d = DataD [] evalParams [] [RecC evalParams (map (genRecFieldDecP . fst) params)] [''Show]
          theClass = ConT collectParams
          theInst = ConT evalParams
          typeDec = TySynInstD (mkName "CollectFor") (TySynEqn [theInst] theInst)
          colInit = ValD (VarP $ mkName "npColInit")
                         (NormalB (RecConE evalParams $ map genRecFieldIniP params)) []
          setParm = ValD (VarP $ mkName "npSetParm") (NormalB (VarE 'id)) []

-- Generate the parts for EvalWeights
evalWeights :: Name
evalWeights = mkName "EvalWeights"

genRecFieldDecW :: String -> VarStrictType
genRecFieldDecW fld = (fldName, IsStrict, ConT midEndType)
    where fldName = mkName fld
          midEndType = mkName "MidEnd"

genRecFieldIniW :: EvalWeightSpec -> FieldExp
genRecFieldIniW (fld, (valM, valE)) = (fldName, fldVal)
    where fldName = mkName fld
          tme = mkName "tme"
          fldVal = AppE (AppE (VarE tme) (LitE (IntegerL valM))) (LitE (IntegerL valE))

genEvalWeights :: Q [Dec]
genEvalWeights = do
    bodyExp <- genCollectEvalParamsExp (map fst weights) True
    let colParm = ValD (VarP $ mkName "npColParm") (NormalB bodyExp) []
        inis = map genRecFieldIniW weights
        rfds = map (genRecFieldDecW . fst) weights
        i = InstanceD [] (AppT theClass theInst) [ typeDec, colInit, colParm, setParm ]
        d = DataD [] evalWeights [] [RecC evalWeights rfds] [''Show]
        colInit = ValD (VarP $ mkName "npColInit") (NormalB (RecConE evalWeights inis)) []
    return [d, i]
    where theClass = ConT collectParams
          theInst = ConT evalWeights
          typeDec = TySynInstD (mkName "CollectFor") (TySynEqn [theInst] theInst)
          setParm = ValD (VarP $ mkName "npSetParm") (NormalB (VarE 'id)) []
