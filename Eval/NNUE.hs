{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Eval.NNUE (
    Matrix, Accum, Layer, FinalLayer, NNUE,
    makeAccum, makeMatrix, makeLayer, makeFinalLayer, makeNNUE,
    addIndex, subIndex, applyNNUE, accumFromList,
    modelSave, modelLoad,
    zeroAccum, accumVal
) where

import Data.Bits (unsafeShiftR)
import qualified Data.ByteString as B
import Data.Int
import Data.Serialize
import Data.Vector.Unboxed         (Vector)
import Data.Vector                 ((!))
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector                 as T
import Data.Vector.Serialize()
import GHC.Generics

-- Accumulator is a vector of floats
type Accum = Vector Int32

-- Matrix is a generic vector of matrix rows (row == unboxed vector)
type Matrix = T.Vector Accum

type Nonlin = Accum -> Accum

-- Sum 2 accumulators
acadd :: Accum -> Accum -> Accum
acadd = U.zipWith (+)

-- Subtract 2 accumulators - means: subtract first from second
acsub :: Accum -> Accum -> Accum
acsub = U.zipWith subtract

-- Scalar product
scalar :: Accum -> Accum -> Int32
scalar a b = U.sum $ U.zipWith (*) a b

multip :: Matrix -> Accum -> Accum
multip m a = U.fromList $ T.toList $ T.map (scalar a) m

-- We cannot serialize functions so we must find another reprezentation for nonlinearities
nonlinExec :: String -> Accum -> Accum
nonlinExec nl x
    | nl == "ReLU"        = relu x
    -- | nl == "Hardsigmoid" = hardsigmoid x
    | otherwise           = error $ "nonlin Executor: unknown Nonlin: " ++ nl

relu :: Nonlin
relu = U.map (max 0)

-- In the quantized world this is not correct and not so simple!
-- hardsigmoid :: Nonlin
-- hardsigmoid = U.map (max 0 . min 1 . (\x -> (x + 3) / 6))

makeAccum :: [Int32] -> Accum
makeAccum = U.fromList

-- check dimensions: all vectors must be same length
makeMatrix :: [Accum] -> Matrix
makeMatrix = T.fromList

-- Every layer (including the final one) and also the NNUE itself have a qunatization
-- constant (2 ^ q) - after the layer application the rezults are right shifted by q
data Layer = Layer {
        laWeights :: Matrix,	-- the weights matrix (vector of rows)
        laBias    :: Accum,	-- bias
        laQuant   :: Int,	-- quantization bits
        laNonlin  :: String	-- the non linearity
    } deriving Generic

instance Serialize Layer

data FinalLayer = FinalLayer {
        flWeights :: Accum,	-- just an accumulator
        flBias    :: Int32,	-- bias - an int
        flQuant   :: Int	-- quantization bits
    } deriving Generic

instance Serialize FinalLayer

data NNUE = NNUE {
        nnueAccums :: Matrix,	-- the accumulators - length must be 768
        nnueBias   :: Accum,	-- the initial accumulator
        nnueQuant  :: Int,	-- quantization bits
        nnueNonlin :: String,	-- non linearity of the first layer
        nnueLayers :: [Layer],	-- list of further intermediate layers
        nnueFinal  :: FinalLayer	-- the final layer
    } deriving Generic

instance Serialize NNUE

-- check dimensions!
makeLayer :: Matrix -> Accum -> Int -> String -> Layer
makeLayer m a q n = Layer { laWeights = m, laBias = a, laQuant = q, laNonlin = n }

-- check dimensions!
makeFinalLayer :: Accum -> Int32 -> Int -> FinalLayer
makeFinalLayer a f q = FinalLayer { flWeights = a, flBias = f, flQuant = q }

-- check dimensions!
makeNNUE :: Matrix -> Accum -> Int -> String -> [Layer] -> FinalLayer -> NNUE
makeNNUE m ai q n ls fl
    = NNUE {
        nnueAccums = m, nnueBias = ai, nnueQuant = q,
        nnueNonlin = n, nnueLayers = ls, nnueFinal = fl
    }

addIndex :: NNUE -> Int -> Accum -> Accum
addIndex nnue i = acadd (nnueAccums nnue ! i)

subIndex :: NNUE -> Int -> Accum -> Accum
subIndex nnue i = acsub (nnueAccums nnue ! i)

accumVal :: NNUE -> Int -> Accum
accumVal nnue i = nnueAccums nnue ! i

applyLayer :: Layer -> Accum -> Accum
applyLayer layer a = U.map (flip unsafeShiftR (laQuant layer))
                         $ nonlinExec (laNonlin layer)
                         $ acadd (multip (laWeights layer) a) (laBias layer)

applyFinalLayer :: FinalLayer -> Accum -> Int32
applyFinalLayer layer a = flip unsafeShiftR (flQuant layer)
                              $ scalar (flWeights layer) a + flBias layer

applyNNUE :: NNUE -> Accum -> Int32
applyNNUE nnue a = applyFinalLayer (nnueFinal nnue) $
    foldr applyLayer
          (U.map (flip unsafeShiftR (nnueQuant nnue)) $ nonlinExec (nnueNonlin nnue) a)
          (reverse $ nnueLayers nnue)

-- Calculate an accumulator from scratch from a list of indices
accumFromList :: NNUE -> [Int] -> Accum
accumFromList nnue = foldr (addIndex nnue) (nnueBias nnue)

modelSave :: NNUE -> FilePath -> IO ()
modelSave nnue filepath = B.writeFile filepath $ encode nnue

modelLoad :: FilePath -> IO (Either String NNUE)
modelLoad filepath = do
    bs <- B.readFile filepath
    return $ decode bs

zeroAccum :: Accum
zeroAccum = U.empty
