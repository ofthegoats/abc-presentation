-- | Examples of using the ABC module

module ABCExamples where

import PPL
import ABC
import Distributions

import qualified System.Random.MWC as MWC

import Control.Monad

-- example of using rejection sampling for a [Binomial 10 0.4] distribution
binomialExample :: IO ()
binomialExample = let
  summary ω = (fromIntegral . sum) ω / (fromIntegral . length) ω
  obs = [2,1,2,4,5,4,3,2,6,2,3,4,8,4,4,6,5,4,3,5,3,4,1,2,5,5,6,4,4,3] -- <<< replicateM 30 $ (bin 10 .4)
  sObs = summary obs
  dist x y = (x-y)**2
  tolerance = 1.5
  prior = uniform01
  model p = summary <$> replicateM 30 (binomial' 10 p)
  n = 1000
  estimate = abcRejection n sObs dist tolerance prior model
  in do
  gen <- MWC.createSystemRandom
  θs' <- runDist estimate gen
  print $ sum θs' / (fromIntegral . length) θs'

-- ditto, using MCMC sampling
binomialExample' :: IO ()
binomialExample' = let
  summary ω = (fromIntegral . sum) ω / (fromIntegral . length) ω
  obs = [2,1,2,4,5,4,3,2,6,2,3,4,8,4,4,6,5,4,3,5,3,4,1,2,5,5,6,4,4,3] -- <<< replicateM 30 $ (bin 10 .4)
  sObs = summary obs
  dist x y = (x-y)**2
  tols = repeat 1.5
  pri = normal' 0.4 0.4
  priD x = let μ = 0.4; ss = 0.4 in (1/sqrt(2 * pi * ss)) * exp ((-1/(2*ss)) * (x - μ)**2)
  pro p = normal' p 0.2
  proD last x = let ss = 0.2 in (1/sqrt(2 * pi * ss)) * exp ((-1/(2*ss)) * (x - last)**2)
  model p = summary <$> replicateM 30 (binomial' 10 p)
  n = 1000
  estimate = abcMCMC n sObs dist tols pri priD pro proD model Nothing
  in do
  gen <- MWC.createSystemRandom
  θs' <- runDist estimate gen
  let θs'' = drop (n `div` 2) θs'
  print $ sum θs'' / (fromIntegral . length) θs''
