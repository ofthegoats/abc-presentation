-- | Implementations of standard ABC kernels for different inference methods

{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ABC where

import PPL
import RS
import MH
import Distributions

import qualified System.Random.MWC as MWC

import GHC.Prim

-- | Approximate Bayesian Computation via Monte Carlo Rejection Sampling
-- | à la Marjoram et al. 2003

type RSABC :: * -> * -> *
data RSABC θ ω = RSABC
  { observations :: ω
  , prior :: Dist θ
  , model :: θ -> Dist ω
  , distance :: ω -> ω -> Double
  , tolerance :: Double
  , gen :: MWC.Gen RealWorld
  }

instance RSKernel (RSABC θ ω) IO θ where
  sample :: RSABC θ ω -> IO θ
  sample RSABC{..} = runDist prior gen

  accept :: RSABC θ ω -> θ -> IO Bool
  RSABC{..} `accept` θ = do
    x <- runDist (model θ) gen
    return $ x `distance` observations <= tolerance

-- | Approximate Bayesian Computation via Gaussian Metropolis sampling
-- | à la Marjoram et al. 2003
-- | (That is, Metropolis-Hastings where the transition kernel is the Gaussian
-- | distribution)

type GMABC :: * -> * -> *
data GMABC θ ω = GMABC
  { observations :: ω
  , model :: θ -> Dist ω
  , priorDensity :: θ -> Double
  , σ² :: Double -- ^ transition variance
  , distance :: ω -> ω -> Double
  , tolerance :: Double
  , gen :: MWC.Gen RealWorld
  }

instance MHKernel (GMABC Double ω) IO Double where
  accept :: GMABC Double ω -> Double -> Double -> IO Bool
  accept GMABC{..} θ_0 θ_1 = let
    α = min 1 $ priorDensity θ_1 / priorDensity θ_0
    in do
    x <- runDist (model θ_1) gen
    if x `distance` observations <= tolerance
      then runDist (bernoulli' α) gen
      else return False

  propose :: GMABC Double ω -> Double -> IO Double
  propose GMABC{..} μ = runDist (normal' μ σ²) gen
