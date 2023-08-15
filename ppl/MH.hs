-- | "Higher Order" Metropolis Hastings

-- | A generalisation of Metropolis Hastings, where the algorithm
-- | is provided with a "kernel" which will
-- | (a) provide samples
-- | (b) decide whether or not to accept the sample

{-
NOTE
The "kernel" system is just a "strategy pattern"
For now I am avoiding a tuning strategy... TODO learn how MWC.Gen updates per usage

When I do implement it, the kernel should be capable of updating itself with
some kind of monad, similar to how MWC.Gen works.
-}


{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module MH where

import GHC.Types

import PPL
import Distributions

import qualified System.Random.MWC as MWC

mh :: (MHKernel k m a) => Int -> a -> k -> m [a]
mh 0 _ _ = return []
mh n x_0 kernel = do
  x_1 <- kernel `propose` x_0
  α <- accept kernel x_0 x_1
  if α
    then (x_1:) <$> mh (n-1) x_1 kernel
    else (x_0:) <$> mh (n-1) x_0 kernel

type MHKernel :: * -> (* -> *) -> * -> Constraint
class Monad m => MHKernel k m a | k -> a where
  accept :: k -> a -> a -> m Bool
  propose :: k -> a -> m a

-- | Classical application of Metropolis-Hastings for sampling from a distribution
-- * Move this to another file onMHGaussce it makes sensMHGausse to MHMCype MHGauss :: * -> *
data MHMC ω = MHMC
  { prior :: Dist ω
  , priorDensity :: ω -> Double
  , targetDensity :: ω -> Double
  , transition :: ω -> Dist ω
  , transitionDensity :: ω -> ω -> Double -- ^ first argument is condition
  , gen :: MWC.GenIO
  }

instance MHKernel (MHMC ω) IO ω where
  accept :: MHMC ω -> ω -> ω -> IO Bool
  accept MHMC{..} last prop = let
    α = (priorDensity prop / priorDensity last)
      * (transitionDensity prop last / transitionDensity last prop)
    in runDist (bernoulli' $ min 1 α) gen

  propose :: MHMC ω -> ω -> IO ω
  MHMC{..} `propose` x = runDist (transition x) gen
