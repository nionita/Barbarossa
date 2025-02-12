{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Eval.NNUE (
    teste, Matrix, Accum, Layer, FinalLayer, NNUE,
    makeAccum, makeMatrix, makeLayer, makeFinalLayer, makeNNUE,
    addIndex, subIndex, applyNNUE, accumFromList
) where

-- import Data.Array.Base (unsafeAt)
-- import Data.Bits
-- import Data.List (minimumBy)
-- import Data.Array.Unboxed
-- import Data.Ord (comparing)
-- import Data.Int

-- import Data.Vector.Unboxed.Mutable (MVector, PrimMonad, PrimState)
import Data.Vector.Unboxed         (Vector)
import Data.Vector                 ((!))
-- import qualified Data.Vector.Unboxed.Mutable as V
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector                 as T

-- import Control.Monad (foldM_)
-- import Control.Monad.ST

-- import Struct.Struct
-- import Struct.Status
-- import Struct.Config
-- import Struct.MidEnd
-- import Moves.Moves
-- import Moves.BitBoard
-- import Moves.Pattern

-- Accumulator is a vector of floats
type Accum = Vector Float

-- Matrix is a generic vector of unboxed vectors, each of them beeing a matrix row
type Matrix = T.Vector Accum

-- Sum 2 accumulators
acadd :: Accum -> Accum -> Accum
acadd = U.zipWith (+)

-- Subtract 2 accumulators - means: subtract first from second
acsub :: Accum -> Accum -> Accum
acsub = U.zipWith subtract

-- Scalar product
scalar :: Accum -> Accum -> Float
scalar a b = U.sum $ U.zipWith (*) a b

multip :: Matrix -> Accum -> Accum
multip m a = U.fromList $ T.toList $ T.map (scalar a) m

type Nonlin = Accum -> Accum

relu :: Nonlin
relu = U.map (max 0)

hardsigmoid :: Nonlin
hardsigmoid = U.map (max 0 . min 1 . (\x -> (x + 3) / 6))

makeAccum :: [Float] -> Accum
makeAccum = U.fromList

-- check dimensions: all vectors must be same length
makeMatrix :: [Accum] -> Matrix
makeMatrix = T.fromList

teste :: Bool
teste = round (U.sum b0) == (60 :: Int)
    where r1 = makeAccum [1.0, 2.0, 3.0, 4.0, 5.0]
          r2 = makeAccum [1.0, 2.0, 1.0, 2.0, 1.0]
          r3 = makeAccum [0.0, 1.0, 2.0, 3.0, 4.0]
          m1 = makeMatrix [r1, r2, r3]
          a1 = makeAccum [2.0, 1.0, 3.0, 1.0, 4.0]
          b1 = multip m1 a1	-- [37, 13, 26]
          b2 = makeAccum [7.0, 3.0, 6.0]
          b0 = acsub b2 b1

data Layer = Layer {
        laWeights :: Matrix,	-- the weights matrix (vector of rows)
        laBias    :: Accum,	-- bias
        laNonlin  :: Nonlin	-- the non linearity
    }

data FinalLayer = FinalLayer {
        flWeights :: Accum,	-- just an accumulator
        flBias    :: Float	-- bias - a float
    }

data NNUE = NNUE {
        nnueAccums :: Matrix,	-- the accumulators - length must be 768
        nnueBias   :: Accum,	-- the initial accumulator
        nnueNonlin :: Nonlin,	-- non linearity of the first layer
        nnueLayers :: [Layer],	-- list of further intermediate layers
        nnueFinal  :: FinalLayer	-- the final layer
    }

-- check dimensions!
makeLayer :: Matrix -> Accum -> Layer
makeLayer m a = Layer { laWeights = m, laBias = a, laNonlin = relu }

-- check dimensions!
makeFinalLayer :: Accum -> Float -> FinalLayer
makeFinalLayer a f = FinalLayer { flWeights = a, flBias = f }

-- check dimensions!
makeNNUE :: Matrix -> Accum -> [Layer] -> FinalLayer -> NNUE
makeNNUE m ai ls fl
    = NNUE { nnueAccums = m, nnueBias = ai, nnueNonlin = relu, nnueLayers = ls, nnueFinal = fl }

addIndex :: NNUE -> Int -> Accum -> Accum
addIndex nnue i = acadd (nnueAccums nnue ! i)

subIndex :: NNUE -> Int -> Accum -> Accum
subIndex nnue i = acsub (nnueAccums nnue ! i)

applyLayer :: Layer -> Accum -> Accum
applyLayer layer a = laNonlin layer $ acadd (multip (laWeights layer) a) (laBias layer)

applyFinalLayer :: FinalLayer -> Accum -> Float
applyFinalLayer layer a = scalar (flWeights layer) a + flBias layer

applyNNUE :: NNUE -> Accum -> Float
applyNNUE nnue a = applyFinalLayer (nnueFinal nnue)
                       $ foldr applyLayer (nnueNonlin nnue a) (reverse $ nnueLayers nnue)

-- Calculate an accumulator from scratch from a list of indices
accumFromList :: NNUE -> [Int] -> Accum
accumFromList nnue = foldr (addIndex nnue) (nnueBias nnue)
