-- Defines the types needed for parameter & weight generation

module Struct.ParamsPre (
    EvalParamSpec,
    EvalWeightSpec,
) where

type EvalParamSpec  = (String, Integer)
type EvalWeightSpec = (String, (Integer, Integer))
