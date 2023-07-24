-- | Approximate Bayesian Computation as described in Marjoram et al. 2003

{-# LANGUAGE RecordWildCards #-}

module ABC where

import PPL
import Distributions

data AbcPar θ ω s =
  ABC { yObs :: ω
      , summary :: ω -> s
      , prior :: Dist θ
      , priorDensity :: θ -> Double
      , proposal :: θ -> Dist θ
      , proposalDensity :: θ -> θ -> Double
      , model :: θ -> Dist ω
      , distance :: s -> s -> Double
      , tolerances :: [Double]
      , lastPar :: Maybe θ
      }

abcMarjoram :: Int -> AbcPar θ ω s -> Dist [θ]
abcMarjoram 0 _ = return []
abcMarjoram n par@ABC{ tolerances=(tolerance : tolerances'), .. } = let
  sObs = summary yObs
  in case lastPar of
  Nothing -> do
    θ <- prior
    yGen <- model θ
    let sGen = summary yGen
    if sObs `distance` sGen <= tolerance
      then (θ:) <$> abcMarjoram (n-1) par{ lastPar = Just θ }
      else abcMarjoram n par
  Just last -> do
    θ' <- proposal last
    yGen <- model θ'
    let sGen = summary yGen
    accept <- bernoulli' $ min 1 $ (priorDensity θ' * proposalDensity last θ') / (priorDensity last * proposalDensity θ' last)
    if sObs `distance` sGen <= tolerance && accept
      then (last:) <$> abcMarjoram (n-1) par{ lastPar = Just θ', tolerances = tolerances' }
      else (last:) <$> abcMarjoram (n-1) par

-- convenience constructor for rejection sampling
abcRejectionPar :: ω -> (ω -> s) -> Dist θ -> (θ -> Double) -> (θ -> Dist ω) -> (s -> s -> Double) -> [Double] -> AbcPar θ ω s
abcRejectionPar yObs summary prior priorDensity model distance tolerances =
  ABC { yObs = yObs
      , summary = summary
      , prior = prior
      , priorDensity = priorDensity
      , proposal = const prior
      , proposalDensity = const priorDensity
      , model = model
      , distance = distance
      , tolerances = tolerances
      , lastPar = Nothing
      }

-- convenience constructor for mcmc-based sampling
abcMCMCPar :: ω -> (ω -> s) -> Dist θ -> (θ -> Double) -> (θ -> Dist θ) -> (θ -> θ -> Double) -> (θ -> Dist ω) -> (s -> s -> Double) -> [Double] -> AbcPar θ ω s
abcMCMCPar yObs summary prior priorDensity proposal proposalDensity model distance tolerances =
  ABC { yObs = yObs
      , summary = summary
      , prior = prior
      , priorDensity = priorDensity
      , proposal = proposal
      , proposalDensity = proposalDensity
      , model = model
      , distance = distance
      , tolerances = tolerances
      , lastPar = Nothing
      }
