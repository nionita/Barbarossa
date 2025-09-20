{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

module Tune.EvalAccum (
    EvalAccum(..),
    EASimple(..),
    EAStep(..),
    EATrip(..),
    reportEAStep,
    reportEATrip,
) where

import Control.Monad (when, forM_)
import Control.Monad.ST
-- import Control.Monad.ST.Unsafe (unsafeIOToST)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as V

import Struct.Struct
import Struct.ParamsPost (optSpaceNames, optSpaceInit)
import Tune.Utils

-- When we evaluate a dataset we can calculate in one pass a few things, like losses for
-- more than one parameter vector, some samples of positions which exhibit higher losses
-- and so on - but the general framework is always the same - with evalPos at the bottom
-- of the functionality
-- In order to be flexible about what we calculate and accumulate, we define a class here
-- for a status that can contain different things and so we can generalize the framework
-- The accum method gets the position, the target score, the target result, the list of
-- scores and the list of losses (calculated for the list of the evaluation states)
-- and transforms the state (here "a")
class EvalAccum a where
    eaInit :: a
    eaAccum :: MyPos -> Double -> Double -> [Int] -> [Double] -> a -> a

-- A simple accumulation with total loss & number of records
data EASimple = EASimple !Int !Double

accumEASimple :: MyPos -> Double -> Double -> [Int] -> [Double] -> EASimple -> EASimple
accumEASimple _ _ _ _ ls (EASimple r acc) = EASimple (r + 1) (acc + head ls)

instance EvalAccum EASimple where
    eaInit  = EASimple 0 0
    eaAccum = accumEASimple

type Accumulator = U.Vector Double

-- A data structure to calculate the next step in optimizing over the whole dataset
-- by moving the current best towards better scores depending on the error, but less for
-- bigger errors - which may come from not understanding the position
data EAStep = EAStep {
        easDims  :: !Int,
        easCount :: !Int,
        easStep  :: !Accumulator
    }

-- We get 1 + 2 * n scores, the first beeing for the current point, the next n
-- for the +1 vectors and the last n for the -1 vectors needed to calculate the partial
-- derivatives of the individual position errors, like
-- (x1 + 1, x2, ..., xn), (x1, x2 + 1, x3, ..., xn) etc. and
-- (x1 - 1, x2, ..., xn), (x1, x2 - 1, x3, ..., xn) etc.
-- where n is the dimensionality of the weight space
-- The losses are ignored (not needed for this method!)
accumEAStep :: MyPos -> Double -> Double -> [Int] -> [Double] -> EAStep -> EAStep
accumEAStep _ tgsc _ scores _ eastep = eastep { easCount = easCount eastep + 1, easStep = nstep }
    where sc0 = fromIntegral $ head scores
          fac = exp (negate $ abs (sc0 - tgsc) * tau)
          (scoresPlus, scoresMinus) = splitAt (easDims eastep) (tail scores)
          -- for the "real" partials we would have to divide by 2, but here the scale does not matter
          -- because at the end we want the highest component, and all components are scaled equal
          partials = map fromIntegral $ zipWith (-) scoresPlus scoresMinus
          nstep = U.zipWith (+) (easStep eastep) (U.fromList $ map ((*) fac) partials)
          tau = 0.005

reportEAStep :: EAStep -> IO ()
reportEAStep eastep = do
    putStrLn $ "--> EAStep ended"
    putStrLn $ "Positions: " ++ show (easCount eastep)
    putStrLn $ "Dims: " ++ show (easDims eastep)
    putStrLn "Step:"
    let vals = U.toList (easStep eastep)
        mx = maximum $ map abs vals
        valn = map (/ mx) vals
    forM_ (zip optSpaceNames valn) $ \(n, v) -> do
        putStrLn $ justifyLeft 25 ' ' n ++ " -> " ++ show v

instance EvalAccum EAStep where
    eaInit  = EAStep { easDims = n, easCount = 0, easStep = U.fromList (take n $ repeat 0) }
        where n = length optSpaceInit
    eaAccum = accumEAStep

-- Similar to EAStep but for -1, 0 and 1 on every dimension we count how much we should
-- move towards them (or stay, for 0) by comparing the 0 to 1 and -1 to 0
data EATrip = EATrip {
        eatDims  :: !Int,
        eatCount :: !Int,
        eatLeft  :: !Accumulator,
        eatMid   :: !Accumulator,
        eatRight :: !Accumulator
    }

-- We get 1 + 2 * n losses, the first beeing for the current point, the next n
-- for the +1 vectors and the last n for the -1 vectors
-- (x1 + 1, x2, ..., xn), (x1, x2 + 1, x3, ..., xn) etc. and
-- (x1 - 1, x2, ..., xn), (x1, x2 - 1, x3, ..., xn) etc.
-- where n is the dimensionality of the weights space
-- The scores are ignored (not needed for this method!)
-- The logic for one dimension is:
-- Compare the loss for -1, current and +1 - the minimum wins - but it could be 1, 2 or 3 winners!
-- The winners share the reward (i.e. for 2 winner, every one gets the half)
-- This procedure is done for every dimension
-- The loss is the absolute value of the difference
-- The reward is the inverse exponential in the current loss
accumEATrip :: MyPos -> Double -> Double -> [Int] -> [Double] -> EATrip -> EATrip
accumEATrip _ _ _ _ losses eatrip
    = eatrip { eatCount = eatCount eatrip + 1, eatLeft = ealeft, eatMid = eamid, eatRight = earight }
    where -- Current loss
          closs = head losses
          -- factor: smaller for further scores
          tau = 0.005
          rew = exp (negate $ closs * tau)
          -- losses for the +1 & -1 vectors
          (rlosses, llosses) = splitAt (eatDims eatrip) (tail losses)
          (ealeft, eamid, earight) = runST $ do
              vl <- U.thaw (eatLeft  eatrip)
              vm <- U.thaw (eatMid   eatrip)
              vr <- U.thaw (eatRight eatrip)
              -- unsafeIOToST $ putStrLn $ " vl: " ++ show (eatLeft  eatrip)
              -- unsafeIOToST $ putStrLn $ " vm: " ++ show (eatMid   eatrip)
              -- unsafeIOToST $ putStrLn $ " vr: " ++ show (eatRight eatrip)
              forM_ (zip [0..] $ zip3 llosses (repeat closs) rlosses) $ \(i, (l, m, r)) -> do
                  let (mi, ms) = foldr (\e (v, k) -> if e < v
                                                     then (e, 1)
                                                     else if e == v
                                                          then (v, k+1)
                                                          else (v, k))
                                       (l, 1::Int) [m, r]
                      fa = rew / fromIntegral ms
                  -- unsafeIOToST $ putStrLn $ " --> dim " ++ show i ++ ": "
                  --     ++ show l ++ ", " ++ show m ++ ", " ++ show r
                  --     ++ " -> mi = " ++ show mi ++ " ms = " ++ show ms
                  when (ms /= 3) $ do
                      when (l == mi) $ do
                          ol <- V.unsafeRead vl i
                          V.unsafeWrite vl i (ol + fa)
                          -- unsafeIOToST $ putStrLn $ " <-> l: "
                          --                    ++ show ol ++ " + " ++ show fa ++ " = " ++ show (ol + fa)
                      when (m == mi) $ do
                          om <- V.unsafeRead vm i
                          V.unsafeWrite vm i (om + fa)
                          -- unsafeIOToST $ putStrLn $ " <-> m: "
                          --                    ++ show om ++ " + " ++ show fa ++ " = " ++ show (om + fa)
                      when (r == mi) $ do
                          or <- V.unsafeRead vr i
                          V.unsafeWrite vr i (or + fa)
                          -- unsafeIOToST $ putStrLn $ " <-> r: "
                          --                    ++ show or ++ " + " ++ show fa ++ " = " ++ show (or + fa)
              ul <- U.unsafeFreeze vl
              um <- U.unsafeFreeze vm
              ur <- U.unsafeFreeze vr
              return (ul, um, ur)

reportEATrip :: EATrip -> IO ()
reportEATrip eatrip = do
    putStrLn $ "--> EATrip ended"
    putStrLn $ "Positions: " ++ show (eatCount eatrip)
    putStrLn $ "Dims: " ++ show (eatDims eatrip)
    putStrLn "Trips:"
    let lefts  = U.toList (eatLeft  eatrip)
        rights = U.toList (eatRight eatrip)
        mids   = U.toList (eatMid   eatrip)
    forM_ (zip optSpaceNames $ zip3 lefts mids rights) $ \(n, (l, m, r)) -> do
        let s = l + m + r
            rl = l / s
            rm = m / s
            rr = r / s
        putStrLn $ justifyLeft 20 ' ' n ++ ": "
            -- ++ justifyLeft 18 ' ' (show l) ++ " / "
            -- ++ justifyLeft 18 ' ' (show m) ++ " / "
            -- ++ justifyLeft 18 ' ' (show r) ++ " -> "
            ++ justifyLeft 21 ' ' (show rl) ++ " / "
            ++ justifyLeft 21 ' ' (show rm) ++ " / "
            ++ justifyLeft 21 ' ' (show rr)

instance EvalAccum EATrip where
    eaInit  = EATrip { eatDims = n, eatCount = 0,
                       eatLeft = U.fromList z1, eatMid = U.fromList z2, eatRight = U.fromList z3 }
        where n = length optSpaceInit
              z1 = take n $ repeat 0
              z2 = take n $ repeat 0
              z3 = take n $ repeat 0
    eaAccum = accumEATrip
