-- | Rejection Sampling

{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Rejection where

import Sampler
import Distributions

import Data.List

import Control.Monad

import qualified System.Random.MWC as MWC

class RSKernel k a | k -> a where
  propose :: k -> IO a
  accepts :: k -> a -> IO Bool

rs :: RSKernel k a => Int -> k -> IO [a]
rs 0 _ = return []
rs n k = do
  x <- propose k
  a <- k `accepts` x
  if a
    then (x:) <$> rs (n-1) k
    else rs (n-1) k

data RSMC ω = RSMC
  { prior :: Sampler ω
  , priorDensity :: ω -> Double -- ^ scaled by M
  , targetDensity :: ω -> Double
  , gen :: Gen
  }

instance RSKernel (RSMC ω) ω where
  propose :: RSMC ω -> IO ω
  propose RSMC{..} = sample prior gen

  accepts :: RSMC ω -> ω -> IO Bool
  accepts RSMC{..} x = let
    α = targetDensity x / priorDensity x
    in sample (bernoulli $ min 1 α) gen

-- approximate the Weibull distribution by rejection sampling
-- for convenience, show the result in R
-- TODO use matplotlib... so that I can have a larger input
approxWeibull ::IO [Double]
approxWeibull = let
  λ = 1
  k = 2.5
  kernel gen = RSMC
    { prior = uniform 0 5
    , priorDensity = const 2
    , targetDensity = \x ->
        (k / λ) * (x / λ)**(k-1) * exp (-(x / λ)**k)
    , gen = gen }
  in do
    gen <- MWC.createSystemRandom
    rs 2000 $ kernel gen

data RSABC θ ω = RSABC
  { observations :: ω
  , model :: θ -> Sampler ω
  , prior :: Sampler θ
  , distance :: ω -> ω -> Double
  , tolerance :: Double
  , gen :: Gen
  }

instance RSKernel (RSABC θ ω) θ where
  propose :: RSABC θ ω -> IO θ
  propose RSABC{..} = sample prior gen

  accepts :: RSABC θ ω -> θ -> IO Bool
  accepts RSABC{..} θ = do
    x <- sample (model θ) gen
    return $ distance x observations <= tolerance

-- approximate the parameters of a uniform distribution
-- by rejection sampling
abcUniform :: IO [(Double, Double)]
abcUniform = let
  -- y ~ (uniform -3 7) # 50
  y = [-2.6560252170793195,-2.5878251619649477,1.9747883832592752,-1.9884247162422193,0.830853291299471,5.343962887212475,1.5405201696401267,-8.165448398970465e-2,4.855397594491139,6.5954039376673865,5.671175574610675,6.656464183429462,-2.8643056179569646,2.682716666365285,0.6279783883511421,4.918005713716293,-0.9712474791324714,1.6379633834120817,-0.2338346482886493,3.206460400811925,-0.7545130658471741,3.3250927601238125,2.7925295308547984,6.612664664164832,5.965851220145867,4.911359813646687,-0.28028505529102166,3.25678322010754e-2,-0.28517959753686295,6.882572533291393,3.520013966325563,0.5817037856869502,1.0161522933175373,5.705922716113841,3.834147859607386,2.519524818230593,4.343903080642181,6.905942464527911,6.8512644428267535,6.096600563575185,1.759373520735771,-2.895503047070596,-1.3498747530535067,4.779124221475421,4.803442975829808,2.4336564852732563,3.640630982958603,2.9769925380066704,3.2352176986384658,-2.1659478473211244]
  summarise x = let x' = sort x in (head x, x !! 25, last x)
  kernel gen = RSABC
    { observations = y
    , model = replicateM 50 . uncurry uniform
    , prior = (,)
        <$> ((\x -> x + minimum y) <$> uniform (-5) 0)
        <*> ((\x -> x + maximum y) <$> uniform 0 5)
    , distance = \x y -> let
        (x0, x1, x2) = summarise x
        (y0, y1, y2) = summarise y
        in sqrt $ (x0 - y0)**2 + (x1 - y1)**2 + (x2 - y2)**2
    , tolerance = 1
    , gen = gen}
  in do
  gen <- MWC.createSystemRandom
  rs 5000 $ kernel gen
