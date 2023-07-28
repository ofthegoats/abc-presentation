-- | Examples of using the ABC module

{-# LANGUAGE DuplicateRecordFields #-}

module ABCExamples where

import PPL
import ABC
import RS
import Distributions

import qualified System.Random.MWC as MWC

import Control.Monad

-- Example of ABC rejection sampling for [Binomial 10 0.4]
binomialExampleRS :: IO ()
binomialExampleRS = let
  summary x = (fromIntegral . sum) x / (fromIntegral . length) x
  kernel gen = RSABC
    { observations = summary [4,4,6,6,5,3,3,3,3,6,4,6,5,2,3,5,3,5,4,1,4,4,4,5,2,4,6,6,6,3]
    , prior = uniform01
    , model = \p -> summary <$> replicateM 30 (binomial' 10 p)
    , distance = \x y -> (x - y)**2
    , tolerance = 1.0
    , gen = gen
    }
  in do
  gen <- MWC.createSystemRandom
  x <- rs 1000 (kernel gen)
  print $ sum x / (fromIntegral . length) x

-- ditto, using MCMC sampling
binomialExampleMH :: IO ()
binomialExampleMH = let
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
