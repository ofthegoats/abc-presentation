-- | Examples of using the ABC module

module ABCExamples where

import PPL
import ABC
import Distributions

import qualified System.Random.MWC as MWC

import Control.Monad

-- example of using rejection sampling for a [Binomial 10 0.4] distribution
binomialExample :: IO ()
binomialExample =
  let obs = [2,1,2,4,5,4,3,2,6,2,3,4,8,4,4,6,5,4,3,5,3,4,1,2,5,5,6,4,4,3] -- <<< replicateM 30 $ (bin 10 .4)
      summary ω = (fromIntegral . sum) ω / (fromIntegral . length) ω
      prior = uniform01
      priorDensity = const 1
      model p = replicateM 30 $ binomial' 10 p
      distance s s' = (s - s')**2
      tolerances = repeat 1.0
      pars = abcRejectionPar obs summary prior priorDensity model distance tolerances
      estimate = abcMarjoram 1000 pars
  in do
    gen <- MWC.createSystemRandom
    θs <- runDist estimate gen
    print $ sum θs / (fromIntegral . length) θs

-- ditto, using MCMC sampling
binomialExample' :: IO ()
binomialExample' =
  let obs = [2,1,2,4,5,4,3,2,6,2,3,4,8,4,4,6,5,4,3,5,3,4,1,2,5,5,6,4,4,3] -- <<< replicateM 30 $ (bin 10 .4)
      summary ω = (fromIntegral . sum) ω / (fromIntegral . length) ω
      prior = uniform01
      priorDensity = const 1
      proposal θ = normal' θ 1
      proposalDensity x μ = let σ² = 1 in exp $ (-1/(2*σ²)) * (x - μ)**2
      model p = replicateM 30 $ binomial' 10 p
      distance s s' = (s - s')**2
      tolerances = replicate 500 1.0 <> replicate 100 0.9 <> replicate 100 0.8 <> replicate 100 0.7 <> replicate 100 0.6 <> replicate 100 0.5
      pars = abcMCMCPar obs summary prior priorDensity proposal proposalDensity model distance tolerances
      estimate = abcMarjoram 1000 pars
  in do
    gen <- MWC.createSystemRandom
    θs <- runDist estimate gen
    let θs' = drop 970 θs
    putStrLn $ (show . head) θs ++ " ... " ++ show θs'
    print $ sum θs' / (fromIntegral . length) θs'
