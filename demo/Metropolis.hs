-- | Metropolis Sampling

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Metropolis where

import Sampler
import Distributions

mh :: MHKernel k a => Int -> k -> a -> Sampler [a]
mh 0 _ _ = return []
mh n k x_0 = do
  x_1 <- k `perturb` x_0
  a <- accepts k x_0 x_1
  if a
    then (x_1:) <$> mh (n-1) k x_1
    else (x_0:) <$> mh (n-1) k x_0

class MHKernel k a | k -> a where
  perturb :: k -> a -> Sampler a
  accepts :: k -> a -> a -> Sampler Bool

data MABC θ ω = MABC
  { observations :: ω
  , model :: θ -> Sampler ω
  , prior :: θ -> Double -- ^ density
  , transition :: θ -> Sampler θ -- ^ assumed symmetrical
  , distance :: ω -> ω -> Double
  , tolerance :: Double
  }

instance MHKernel (MABC θ ω) θ where
  perturb :: MABC θ ω -> θ -> Sampler θ
  perturb MABC{..} = transition

  accepts :: MABC θ ω -> θ -> θ -> Sampler Bool
  accepts MABC{..} θ θ' = do
    x <- model θ'
    if distance x observations <= tolerance
      then bernoulli $ min 1 (prior θ' / prior θ)
      else return False
