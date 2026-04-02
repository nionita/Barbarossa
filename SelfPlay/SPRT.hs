module SelfPlay.SPRT
    ( PairWdl(..)
    , PentaScore(..)
    , SprtConfig(..)
    , SprtResult(..)
    , SprtState
    , sprtLowerBound
    , sprtUpperBound
    , emptyPenta
    , addPentaScore
    , classifyPair
    , mkSprtState
    , sprtResult
    , sprtPentaLLR
    ) where

data PairWdl = PairWin | PairDraw | PairLoss
    deriving (Eq, Show, Read)

data PentaScore = PentaScore
    { pWW :: !Int
    , pWD :: !Int
    , pWL :: !Int
    , pDD :: !Int
    , pLD :: !Int
    , pLL :: !Int
    }
    deriving (Eq, Show, Read)

data SprtConfig = SprtConfig
    { sprtAlpha :: !Double
    , sprtBeta  :: !Double
    , sprtElo0  :: !Double
    , sprtElo1  :: !Double
    }
    deriving (Eq, Show, Read)

data SprtResult = SprtH0 | SprtH1 | SprtContinue
    deriving (Eq, Show, Read)

data SprtState = SprtState
    { sprtCfgState   :: !SprtConfig
    , sprtLowerBound :: !Double
    , sprtUpperBound :: !Double
    }

emptyPenta :: PentaScore
emptyPenta = PentaScore 0 0 0 0 0 0

addPentaScore :: PentaScore -> PentaScore -> PentaScore
addPentaScore (PentaScore ww1 wd1 wl1 dd1 ld1 ll1) (PentaScore ww2 wd2 wl2 dd2 ld2 ll2) =
    PentaScore (ww1 + ww2) (wd1 + wd2) (wl1 + wl2) (dd1 + dd2) (ld1 + ld2) (ll1 + ll2)

classifyPair :: PairWdl -> PairWdl -> PentaScore
classifyPair r1 r2 = case (w, d, l) of
    (2, 0, 0) -> PentaScore 1 0 0 0 0 0
    (1, 1, 0) -> PentaScore 0 1 0 0 0 0
    (1, 0, 1) -> PentaScore 0 0 1 0 0 0
    (0, 2, 0) -> PentaScore 0 0 0 1 0 0
    (0, 1, 1) -> PentaScore 0 0 0 0 1 0
    (0, 0, 2) -> PentaScore 0 0 0 0 0 1
    _         -> emptyPenta
    where
      w = fromEnum (r1 == PairWin) + fromEnum (r2 == PairWin)
      d = fromEnum (r1 == PairDraw) + fromEnum (r2 == PairDraw)
      l = fromEnum (r1 == PairLoss) + fromEnum (r2 == PairLoss)

mkSprtState :: SprtConfig -> SprtState
mkSprtState cfg = SprtState
    { sprtCfgState = cfg
    , sprtLowerBound = log (sprtBeta cfg / (1 - sprtAlpha cfg))
    , sprtUpperBound = log ((1 - sprtBeta cfg) / sprtAlpha cfg)
    }

sprtResult :: SprtState -> Double -> SprtResult
sprtResult ss llr
    | llr >= sprtUpperBound ss = SprtH1
    | llr <= sprtLowerBound ss = SprtH0
    | otherwise                = SprtContinue

sprtPentaLLR :: SprtState -> PentaScore -> Double
sprtPentaLLR ss (PentaScore ww wd wl dd ld ll) =
    getLLRNormalized total scores probs t0 t1
    where
      reg x = if x == 0 then 1e-3 else fromIntegral x
      ll' = reg ll
      ld' = reg ld
      wldd' = reg (wl + dd)
      wd' = reg wd
      ww' = reg ww
      total = ll' + ld' + wldd' + wd' + ww'
      probs = map (/ total) [ll', ld', wldd', wd', ww']
      scores = [0.0, 0.25, 0.5, 0.75, 1.0]
      cfg = sprtCfgState ss
      t0 = sqrt 2.0 * sprtElo0 cfg / (800.0 / log 10.0)
      t1 = sqrt 2.0 * sprtElo1 cfg / (800.0 / log 10.0)

meanVals :: [Double] -> [Double] -> Double
meanVals xs ps = sum $ zipWith (*) xs ps

meanVarVals :: [Double] -> [Double] -> (Double, Double)
meanVarVals xs ps = (mu, var)
    where
      mu = meanVals xs ps
      var = sum $ zipWith (\x p -> p * (x - mu) * (x - mu)) xs ps

itpRoot
    :: (Double -> Double)
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
    -> Double
itpRoot f a0 b0 fa0 fb0 k1 k2 n0 eps = go (0 :: Int) a1 b1 fa1 fb1
    where
      (a1, b1, fa1, fb1)
          | fa0 > 0 = (b0, a0, fb0, fa0)
          | otherwise = (a0, b0, fa0, fb0)
      nHalf = ceiling (logBase 2 (abs (b1 - a1) / (2.0 * eps))) :: Int
      nMax = fromIntegral nHalf + n0
      signOrOne x
          | x < 0 = -1
          | x > 0 = 1
          | otherwise = 1
      go i a b fa fb
          | abs (b - a) <= 2.0 * eps = (a + b) / 2.0
          | otherwise =
              let xHalf = (a + b) / 2.0
                  r = eps * (2.0 ** (nMax - fromIntegral i)) - (b - a) / 2.0
                  delta = k1 * ((b - a) ** k2)
                  xF = (fb * a - fa * b) / (fb - fa)
                  sigma = signOrOne (xHalf - xF)
                  xT = if delta <= abs (xHalf - xF) then xF + sigma * delta else xHalf
                  xITP = if abs (xT - xHalf) <= r then xT else xHalf - sigma * r
                  fITP = f xITP
              in if abs fITP < 1e-15
                    then xITP
                    else if fITP < 0
                            then go (i + 1) xITP b fITP fb
                            else go (i + 1) a xITP fa fITP

mleNormalized :: [Double] -> [Double] -> Double -> Double -> [Double]
mleNormalized scores probs muRef tStar = go 0 (replicate (length scores) (1.0 / fromIntegral (length scores)))
    where
      thetaEps = 1e-7
      mleEps = 1e-4
      go :: Int -> [Double] -> [Double]
      go i p
          | i >= 10 = p
          | otherwise =
              let (mu, var) = meanVarVals scores p
                  sigma = sqrt var
                  phi = map (\a ->
                          let z = (a - mu) / sigma
                          in a - muRef - 0.5 * tStar * sigma * (1.0 + z * z)
                        ) scores
                  u = minimum phi
                  v = maximum phi
                  minTheta = -1.0 / v
                  maxTheta = -1.0 / u
                  fTheta x = sum $ zipWith (\ph phat -> phat * ph / (1.0 + x * ph)) phi probs
                  theta = itpRoot fTheta minTheta maxTheta (1 / 0) (-1 / 0) 0.1 2.0 0.99 thetaEps
                  pNew = zipWith (\ph phat -> phat / (1.0 + theta * ph)) phi probs
                  maxDiff = maximum $ zipWith (\a b -> abs (a - b)) pNew p
              in if maxDiff < mleEps then pNew else go (i + 1) pNew

getLLRNormalized :: Double -> [Double] -> [Double] -> Double -> Double -> Double
getLLRNormalized total scores probs t0 t1 = total * meanVals lpr probs
    where
      p0 = mleNormalized scores probs 0.5 t0
      p1 = mleNormalized scores probs 0.5 t1
      lpr = zipWith (\a b -> log a - log b) p1 p0
