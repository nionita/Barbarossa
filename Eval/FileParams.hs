module Eval.FileParams (
    makeEvalState
  ) where

-- import Data.Char (isSpace)
import Data.List (tails, intersperse)
import System.Directory

import Struct.Status(EvalState(..))
import Struct.Config
import Eval.NNUE (modelLoad, modelSave)
import Eval.Model (model)

-- Opens a model file for eval, read it and create an eval state
makeEvalState :: Maybe FilePath -> IO (EvalState, String)
makeEvalState argfile
    | Just filename <- argfile = do
        emodel <- modelLoad filename
        case emodel of
            Left mes -> return (EvalState model, "Load from " ++ show filename ++ ": " ++ mes)
            Right mo -> return (EvalState mo, "Model from " ++ show filename ++ " loaded")
    | otherwise = do
        modelSave model "model.bin"
        return (EvalState model, "No model path given, use default model")
