{-# LANGUAGE DuplicateRecordFields #-}

module Examples where

import Distributions
import Rejection
import Sampler

import qualified System.Random.MWC as MWC

import Control.Monad

import Data.List

-- call the examples for an external script to graph the results
main :: IO ()
main = do
  model <- getLine
  case model of
    x -> print x

{-
import matplotlib.pyplot as plt
x = [...] # copied from Haskell
plt.hist(x, bins=25, range=(0, 5))
plt.show()
-}

-- approximate the Weibull distribution by rejection sampling
-- for convenience, show the result in R
approxWeibull :: Sampler [Double]
approxWeibull = let
  λ = 1
  k = 2.5
  kernel = RSMC
    { prior = uniform 0 5
    , priorDensity = const 2
    , targetDensity = \x ->
        (k / λ) * (x / λ)**(k-1) * exp (-(x / λ)**k) }
  in rs 10000 kernel

-- approximate the parameters of a uniform distribution
-- by rejection sampling
abcUniform :: Sampler [(Double, Double)]
abcUniform = let
  -- y ~ (uniform -3 7) # 50
  y = [-2.6560252170793195,-2.5878251619649477,1.9747883832592752,-1.9884247162422193,0.830853291299471,5.343962887212475,1.5405201696401267,-8.165448398970465e-2,4.855397594491139,6.5954039376673865,5.671175574610675,6.656464183429462,-2.8643056179569646,2.682716666365285,0.6279783883511421,4.918005713716293,-0.9712474791324714,1.6379633834120817,-0.2338346482886493,3.206460400811925,-0.7545130658471741,3.3250927601238125,2.7925295308547984,6.612664664164832,5.965851220145867,4.911359813646687,-0.28028505529102166,3.25678322010754e-2,-0.28517959753686295,6.882572533291393,3.520013966325563,0.5817037856869502,1.0161522933175373,5.705922716113841,3.834147859607386,2.519524818230593,4.343903080642181,6.905942464527911,6.8512644428267535,6.096600563575185,1.759373520735771,-2.895503047070596,-1.3498747530535067,4.779124221475421,4.803442975829808,2.4336564852732563,3.640630982958603,2.9769925380066704,3.2352176986384658,-2.1659478473211244]
  summarise x = let x' = sort x in (head x, x !! 25, last x)
  kernel = RSABC
    { observations = y
    , model = replicateM 50 . uncurry uniform
    , prior = (,)
        <$> ((\x -> x + minimum y) <$> uniform (-5) 0)
        <*> ((\x -> x + maximum y) <$> uniform 0 5)
    , distance = \x y -> let
        (x0, x1, x2) = summarise x
        (y0, y1, y2) = summarise y
        in sqrt $ (x0 - y0)**2 + (x1 - y1)**2 + (x2 - y2)**2
    , tolerance = 1 }
  in rs 100000 kernel
