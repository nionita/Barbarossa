{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-x-partial #-}

module Tune.EvalAccum (
    EvalAccum(..),
    EASimple(..),
    reportEAStat,
    bestLoss,
    firstLoss,
) where

import Control.Monad (when, forM_)
import Control.Monad.ST
-- import Control.Monad.ST.Unsafe (unsafeIOToST)

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as V
-- import Data.Vector.Generic

import Struct.Struct
import Struct.ParamsPost (optSpaceNames, optSpaceInit)
import Moves.Notation
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
    eaStop :: a -> Bool

type Accumulator = U.Vector Double
type Counters    = U.Vector Int

-- A simple accumulation with total loss & number of records
data EASimple = EASimple !Int !Double

accumEASimple :: MyPos -> Double -> Double -> [Int] -> [Double] -> EASimple -> EASimple
accumEASimple _ _ _ _ ls (EASimple r acc) = EASimple (r + 1) (acc + head ls)

instance EvalAccum EASimple where
    eaInit  = EASimple 0 0
    eaAccum = accumEASimple
    eaStop  = const False

-- Accumulation with number of records & total loss per parameter vector
data EAMulti = EAMulti !Int !Accumulator

accumEAMulti :: MyPos -> Double -> Double -> [Int] -> [Double] -> EAMulti -> EAMulti
accumEAMulti _ _ _ _ ls (EAMulti r acc) = EAMulti (r + 1) (U.zipWith (+) acc (U.fromList ls))

instance EvalAccum EAMulti where
    eaInit  = EAMulti 0 (U.fromList (take (length optSpaceInit) $ repeat 0))
    eaAccum = accumEAMulti
    eaStop  = const False

bestLoss :: EAMulti -> (Int, Double)
bestLoss (EAMulti _ lss) = (i, U.unsafeIndex lss i)
    where i = U.minIndex lss

firstLoss :: EAMulti -> Double
firstLoss (EAMulti _ lss) = U.unsafeIndex lss 0

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
    eaStop  = const False

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
          addVal vr i fa = do
              old <- V.unsafeRead vr i
              V.unsafeWrite vr i (old + fa)
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
                          addVal vl i fa
                          -- unsafeIOToST $ putStrLn $ " <-> l: "
                          --                    ++ show ol ++ " + " ++ show fa ++ " = " ++ show (ol + fa)
                      when (m == mi) $ do
                          addVal vm i fa
                          -- unsafeIOToST $ putStrLn $ " <-> m: "
                          --                    ++ show om ++ " + " ++ show fa ++ " = " ++ show (om + fa)
                      when (r == mi) $ do
                          addVal vr i fa
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
    eaStop  = const False

-- Collect different statistics over dimensions along the train set:
-- - number of positions
-- - max score change using +1/-1 of all weights - summed per train set
-- - counters for a histogram of the absolute errors, for up to 24, 48, 96, 192, 384, 768, 1536, and over
-- - counters for a histogram of the absolute target scores, same boudaries as for errors
-- - max score change using +1/-1 of one weight summed per dimension and train set - to measure
-- how sensitive the score is in regard to every dimension
-- Also collect positions with high error and low max score change - which should be the ones
-- hard to "understand"
data EAStat = EAStat {
        eacColl  :: Bool,			-- if it should collect - or count
        eacDims  :: !Int,
        eacCount :: !Int,
        eacMaxSu :: !Int,
        eacHistE :: !Counters,
        eacHistS :: !Counters,
        eacMaxCh :: !Counters,
        eacPosC  :: !Int,			-- number of collected positions
        eacPos   :: [(MyPos, Int, Int, Int)]	-- the collected positions with target, score and max change
    }

accumEAStat :: MyPos -> Double -> Double -> [Int] -> [Double] -> EAStat -> EAStat
accumEAStat pos tgsc _ scores _ eastat
    | eacColl eastat = accumEAStatCollect pos tgsc scores eastat
    | otherwise      = accumEAStatCount   pos tgsc scores eastat

-- We get 1 + 2 * n losses, the first beeing for the current point, the next n
-- for the +1 vectors and the last n for the -1 vectors
-- (x1 + 1, x2, ..., xn), (x1, x2 + 1, x3, ..., xn) etc. and
-- (x1 - 1, x2, ..., xn), (x1, x2 - 1, x3, ..., xn) etc.
-- where n is the dimensionality of the weights space
-- Here we count the statistics
accumEAStatCount :: MyPos -> Double -> [Int] -> EAStat -> EAStat
accumEAStatCount _ tgsc scores eastat
    = eastat { eacCount = eacCount eastat + 1,
               eacMaxSu = eacMaxSu eastat + maxsu,
               eacHistE = histe,
               eacHistS = hists,
               eacMaxCh = maxch
             }
    where powers2 = 1 : map (* 2) powers2
          boundaries = take 7 $ map (* 24) powers2
          histIndex h = foldr (\a b -> if h <= a then b else b + 1) 0 boundaries
          itgsc = round tgsc
          -- A ST function to add to a histogram
          addHisto i v = do
              old <- V.read v i
              V.write v i (old + 1)
          -- Current loss (or error)
          cscor = head scores
          iloss = histIndex (abs $ itgsc - cscor)
          -- Add to the error histogram
          histe = U.modify (addHisto iloss) $ eacHistE eastat
          -- Target score histogram
          iscor = histIndex (abs itgsc)
          -- Add to the score histogram
          hists = U.modify (addHisto iscor) $ eacHistS eastat
          -- scores for the +1 & -1 vectors, the max absolute changes per dimension
          (rscores, lscores) = splitAt (eacDims eastat) (tail scores)
          rchanges = map abs $ zipWith (-) rscores $ repeat cscor
          lchanges = map abs $ zipWith (-) lscores $ repeat cscor
          changes  = zipWith max rchanges lchanges
          maxch    = U.zipWith (+) (eacMaxCh eastat) $ U.fromList changes
          maxsu    = maximum changes

-- We get 1 + 2 * n losses, the first beeing for the current point, the next n
-- for the +1 vectors and the last n for the -1 vectors
-- (x1 + 1, x2, ..., xn), (x1, x2 + 1, x3, ..., xn) etc. and
-- (x1 - 1, x2, ..., xn), (x1, x2 - 1, x3, ..., xn) etc.
-- where n is the dimensionality of the weights space
-- Here we collect positions - parameters are hard coded for now
accumEAStatCollect :: MyPos -> Double -> [Int] -> EAStat -> EAStat
accumEAStatCollect pos tgsc scores eastat
    | closs > 200 && maxsu < 10
        = eastat {
                   eacCount = eacCount eastat + 1,
                   eacPosC  = eacPosC eastat + 1,
                   eacPos   = (pos, itgsc, cscor, maxsu) : eacPos eastat
                 }
    | otherwise = eastat
    where itgsc = round tgsc
          -- Current loss (or error)
          cscor = head scores
          closs = abs $ itgsc - cscor
          -- scores for the +1 & -1 vectors, the max absolute changes per dimension
          (rscores, lscores) = splitAt (eacDims eastat) (tail scores)
          rchanges = map abs $ zipWith (-) rscores $ repeat cscor
          lchanges = map abs $ zipWith (-) lscores $ repeat cscor
          changes  = zipWith max rchanges lchanges
          maxsu    = maximum changes

reportEAStat :: EAStat -> IO ()
reportEAStat eastat = do
    putStrLn $ "--> EAStat ended " ++ if eacColl eastat then "collection" else "counting"
    putStrLn $ "Dims: " ++ show (eacDims eastat)
    putStrLn $ "Positions: " ++ show (eacCount eastat)
    if eacColl eastat
       then do
           forM_ (eacPos eastat) $ \(pos, itgsc, score, maxsu) -> do
               putStrLn $ justifyRight 5 ' ' (show itgsc) ++ " "
                   ++ justifyRight 5 ' ' (show score) ++ " "
                   ++ justifyRight 3 ' ' (show maxsu) ++ ": "
                   ++ posToFen pos
       else do
           let incount = (1 :: Double) / fromIntegral (eacCount eastat)
               maxsu = fromIntegral (eacMaxSu eastat) * incount
           putStrLn $ "Scores Histogram abs: " ++ show (eacHistS eastat)
           putStrLn $ "Scores Histogram rel: " ++ show (U.map ((* incount) . fromIntegral) $ eacHistS eastat)
           putStrLn $ "Errors Histogram abs: " ++ show (eacHistE eastat)
           putStrLn $ "Errors Histogram rel: " ++ show (U.map ((* incount) . fromIntegral) $ eacHistE eastat)
           putStrLn $ "Max Change:" ++ show (eacMaxSu eastat) ++ " -> " ++ show maxsu
           putStrLn "Max Change per dimension:"
           forM_ (zip optSpaceNames (U.toList (eacMaxCh eastat))) $ \(n, chg) -> do
               putStrLn $ justifyLeft 20 ' ' n ++ ": "
                   ++ justifyLeft 10 ' ' (show chg) ++ " -> "
                   ++ justifyLeft 21 ' ' (show (fromIntegral chg * incount))

stopEAStat :: EAStat -> Bool
stopEAStat eastat
    | eacColl eastat = eacPosC eastat >= 10
    | otherwise      = False

instance EvalAccum EAStat where
    -- Hardcoded for the operation for now
    eaInit  = EAStat {
                       eacColl = False, eacDims = n, eacCount = 0, eacMaxSu = 0,
                       eacHistE = U.fromList z1, eacHistS = U.fromList z1, eacMaxCh = U.fromList z2,
                       eacPosC = 0, eacPos = []
                     }
        where n = length optSpaceInit
              z1 = take 8 $ repeat 0
              z2 = take n $ repeat 0
    eaAccum = accumEAStat
    eaStop  = stopEAStat
