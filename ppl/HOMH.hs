-- | "Higher Order" Metropolis Hastings

-- | A generalisation of Metropolis Hastings, where the algorithm
-- | is provided with a "kernel" which will
-- | (a) provide samples
-- | (b) decide whether or not to accept the sample

-- * NOTE
-- the struggle to do this with MH is that we probably want access to our trace
-- for an "update" process to tweak the parameters of the chain.
-- notice though that across iterations of @rs@ we pass in the kernel...
-- so in theory we could make a new one via an "update" process Trace -> Kernel -> Kernel
--
-- what data will this function need to be given access to?
-- some algorithms may rely on knowing the acceptance rate, others on the quantiles of the trace
--
-- if we are to support an update procedure, then surely it would have to be given first class support?

{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HOMH where

import GHC.Types

import PPL
import Distributions

import qualified System.Random.MWC as MWC

mh :: MHKernel k m a => Int -> a -> k -> m [a]
mh 0 _ _ = return []
mh n last kernel = do
  x <- kernel `propose` last
  α <- accept kernel last x
  -- newKernel <- kernel `update` (x, α)
  let newKernel = kernel
  if α
    then (x:) <$> mh (n-1) x newKernel
    else (last:) <$> mh (n-1) last newKernel

-- * TODO How to allow the user to update hyperparameters during the burn-in?
-- being able to tune hyperparameters (e.g. the spread of the transition distribution,
-- particularly when it is Gaussian) is important to decrease the mixing time.
type MHKernel :: * -> (* -> *) -> * -> Constraint
class Monad m => MHKernel k m a | k -> a where
  sample :: k -> m a
  accept :: k -> a -> a -> m Bool
  propose :: k -> a -> m a

-- | Classical application of Metropolis-Hastings for sampling from a distribution
-- * Move this to another file once it makes sense to do so
type MHMC :: * -> *
data MHMC ω = MHMC
  { prior :: Dist ω
  , priorDensity :: ω -> Double
  , transition :: ω -> Dist ω
  , transitionDensity :: ω -> ω -> Double
  , targetDensity :: ω -> Double
  , gen :: MWC.GenIO
  }

instance MHKernel (MHMC ω) IO ω where
  sample :: MHMC ω -> IO ω
  sample MHMC{..} = runDist prior gen

  -- ^ @x@ is the /last/ parameter, @x'@ is the /proposed/ parameter.
  accept :: MHMC ω -> ω -> ω -> IO Bool
  accept MHMC{..} x x' = let
    α = (targetDensity x' / targetDensity x) * (transitionDensity x' x / transitionDensity x x')
    in runDist (bernoulli' $ min 1 α) gen

  propose :: MHMC ω -> ω -> IO ω
  propose MHMC{..} x = runDist (transition x) gen

-- Metropolis-Hastings sampling with target [Normal 0.5 0.1]
classical :: IO ()
classical = let
  transσ² = 0.05
  kernel gen = MHMC
    { prior = uniform01
    , priorDensity = const 1
    , transition = \μ -> normal' μ transσ²
    , transitionDensity = \μ x' ->
        (1 / sqrt (2 * pi * transσ²)) * exp ((-1/(2*transσ²)) * (x' - μ)**2)
    , targetDensity = \x -> let μ = 0.5 ; σ² = 0.1
        in (1 / sqrt (2 * pi * σ²)) * exp ((-1/(2*σ²)) * (x - μ)**2)
    , gen = gen
    }
  in do
  gen <- MWC.createSystemRandom
  xs <- mh 200 0.5 (kernel gen)
  print xs
