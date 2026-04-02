module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

import SelfPlay.SPRT
    ( PairWdl(..)
    , PentaScore(..)
    , SprtConfig(..)
    , classifyPair
    , mkSprtState
    , sprtPentaLLR
    )

main :: IO ()
main = do
    summary <- runTestTT tests
    if errors summary + failures summary == 0
       then exitSuccess
       else exitFailure

tests :: Test
tests = TestList
    [ TestLabel "classifyPair buckets" testClassifyPair
    , TestLabel "balanced counts stay neutral with symmetric bounds" testSymmetricCounts
    , TestLabel "mirrored counts negate llr with symmetric bounds" testMirroredCounts
    , TestLabel "scaling counts scales llr" testScaleInvariance
    , TestLabel "more favorable counts increase llr" testMonotonicity
    , TestLabel "golden approximation near balanced vector" testGoldenBalanced
    , TestLabel "golden approximation near slight plus vector" testGoldenSlightPlus
    ]

testClassifyPair :: Test
testClassifyPair = TestList
    [ classifyCase (PairWin, PairWin) (mkCombinedPenta 1 0 0 0 0)
    , classifyCase (PairWin, PairDraw) (mkCombinedPenta 0 1 0 0 0)
    , classifyCase (PairWin, PairLoss) (mkCombinedPenta 0 0 1 0 0)
    , classifyCase (PairDraw, PairDraw) (PentaScore 0 0 0 1 0 0)
    , classifyCase (PairDraw, PairLoss) (mkCombinedPenta 0 0 0 1 0)
    , classifyCase (PairLoss, PairLoss) (mkCombinedPenta 0 0 0 0 1)
    ]
    where
      classifyCase (r1, r2) expected =
          TestCase $ assertEqual ("classifyPair " ++ show (r1, r2)) expected (classifyPair r1 r2)

testSymmetricCounts :: Test
testSymmetricCounts = TestCase $
    assertApprox 1e-12 0.0 (llr (-2.5) 2.5 balancedPenta)

testMirroredCounts :: Test
testMirroredCounts = TestCase $
    assertApprox 1e-9 (negate $ llr (-2.5) 2.5 strongPlusPenta) (llr (-2.5) 2.5 $ mirrorPenta strongPlusPenta)

testScaleInvariance :: Test
testScaleInvariance = TestCase $
    assertApprox 1e-9 (3.0 * llr 0.0 5.0 slightPlusPenta) (llr 0.0 5.0 $ scalePenta 3 slightPlusPenta)

testMonotonicity :: Test
testMonotonicity = TestCase $
    assertBool "shifting mass from LD to WD should increase llr" $
        llr 0.0 5.0 moreFavorablePenta > llr 0.0 5.0 slightPlusPenta

testGoldenBalanced :: Test
testGoldenBalanced = TestCase $ do
    let approx = approxNormalizedLLR 0.0 5.0 balancedPenta
    assertApprox 1e-12 (-2.0704180788522013) approx
    assertApprox 5e-4 approx (llr 0.0 5.0 balancedPenta)

testGoldenSlightPlus :: Test
testGoldenSlightPlus = TestCase $ do
    let approx = approxNormalizedLLR 0.0 5.0 slightPlusPenta
    assertApprox 1e-12 3.967113955768269 approx
    assertApprox 2e-3 approx (llr 0.0 5.0 slightPlusPenta)

balancedPenta :: PentaScore
balancedPenta = mkCombinedPenta 159 2189 5303 2189 159

slightPlusPenta :: PentaScore
slightPlusPenta = mkCombinedPenta 130 2350 5250 2150 120

strongPlusPenta :: PentaScore
strongPlusPenta = mkCombinedPenta 320 2800 5000 1800 80

moreFavorablePenta :: PentaScore
moreFavorablePenta = mkCombinedPenta 130 2360 5250 2140 120

mkCombinedPenta :: Int -> Int -> Int -> Int -> Int -> PentaScore
mkCombinedPenta ww wd wldd ld ll = PentaScore ww wd wldd 0 ld ll

mirrorPenta :: PentaScore -> PentaScore
mirrorPenta (PentaScore ww wd wl dd ld ll) = PentaScore ll ld wl dd wd ww

scalePenta :: Int -> PentaScore -> PentaScore
scalePenta k (PentaScore ww wd wl dd ld ll) = PentaScore (k * ww) (k * wd) (k * wl) (k * dd) (k * ld) (k * ll)

llr :: Double -> Double -> PentaScore -> Double
llr elo0 elo1 = sprtPentaLLR state
    where
      state = mkSprtState $ SprtConfig 0.05 0.05 elo0 elo1

approxNormalizedLLR :: Double -> Double -> PentaScore -> Double
approxNormalizedLLR elo0 elo1 penta = total * 0.5 * log ((1.0 + (tHat - t0) * (tHat - t0)) / (1.0 + (tHat - t1) * (tHat - t1)))
    where
      (total, scores, probs) = combinedOutcomeStats penta
      mu = meanVals scores probs
      sigma = sqrt $ varianceVals scores probs
      tHat = (mu - 0.5) / sigma
      t0 = normalizedT elo0
      t1 = normalizedT elo1

combinedOutcomeStats :: PentaScore -> (Double, [Double], [Double])
combinedOutcomeStats (PentaScore ww wd wl dd ld ll) = (total, scores, probs)
    where
      outcomeCounts =
          [ fromIntegral ll
          , fromIntegral ld
          , fromIntegral (wl + dd)
          , fromIntegral wd
          , fromIntegral ww
          ]
      total = sum outcomeCounts
      scores = [0.0, 0.25, 0.5, 0.75, 1.0]
      probs = map (/ total) outcomeCounts

normalizedT :: Double -> Double
normalizedT elo = sqrt 2.0 * elo / (800.0 / log 10.0)

meanVals :: [Double] -> [Double] -> Double
meanVals xs ps = sum $ zipWith (*) xs ps

varianceVals :: [Double] -> [Double] -> Double
varianceVals xs ps = sum $ zipWith (\x p -> p * (x - mu) * (x - mu)) xs ps
    where
      mu = meanVals xs ps

assertApprox :: Double -> Double -> Double -> Assertion
assertApprox eps expected actual =
    assertBool
        ("expected " ++ show expected ++ ", got " ++ show actual ++ ", diff " ++ show (abs (expected - actual)))
        (abs (expected - actual) <= eps)
