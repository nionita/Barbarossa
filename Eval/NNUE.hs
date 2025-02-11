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
relu = U.map f
    where f x | x > 0     = x
              | otherwise = 0

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
        weights :: Matrix,	-- the weights matrix (vector of rows)
        bias    :: Accum,	-- bias
        nonlin  :: Nonlin	-- the non linearity
    }

data FinalLayer = FinalLayer {
        fweights :: Accum,	-- just an accumulator
        fbias    :: Float	-- bias - a float
    }

data NNUE = NNUE {
        accums :: Matrix,	-- the accumulators - length must be 768
        accumi :: Accum,	-- the initial accumulator
        layers :: [Layer],	-- the list of intermediate layers
        final  :: FinalLayer	-- the final layer
    }

-- check dimensions!
makeLayer :: Matrix -> Accum -> Layer
makeLayer m a = Layer { weights = m, bias = a, nonlin = relu }

-- check dimensions!
makeFinalLayer :: Accum -> Float -> FinalLayer
makeFinalLayer a f = FinalLayer { fweights = a, fbias = f }

-- check dimensions!
makeNNUE :: Matrix -> Accum -> [Layer] -> FinalLayer -> NNUE
makeNNUE m ai ls fl = NNUE { accums = m, accumi = ai, layers = ls, final = fl }

addIndex :: NNUE -> Int -> Accum -> Accum
addIndex nnue i = acadd (accums nnue ! i)

subIndex :: NNUE -> Int -> Accum -> Accum
subIndex nnue i = acsub (accums nnue ! i)

applyLayer :: Layer -> Accum -> Accum
applyLayer layer a = nonlin layer $ acadd (multip (weights layer) a) (bias layer)

applyFinalLayer :: FinalLayer -> Accum -> Float
applyFinalLayer layer a = scalar (fweights layer) a + fbias layer

applyNNUE :: NNUE -> Accum -> Float
applyNNUE nnue a = applyFinalLayer (final nnue) $ foldr applyLayer a (reverse $ layers nnue)

-- Calculate an accumulator from scratch from a list of indices
accumFromList :: NNUE -> [Int] -> Accum
accumFromList nnue = foldr (addIndex nnue) (accumi nnue)
