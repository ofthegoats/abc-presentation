-- | "Higher Order" Rejection Sampling

-- | A generalisation of rejection sampling, where the algorithm
-- | is provided with a "kernel" which will
-- | (a) provide samples
-- | (b) decide whether or not to accept the sample

-- * TODO
-- * This would also be fairly easy to generalise to importance sampling, which just
-- * introduces a Bernoulli decision on whether or not to accept a particular sample,
-- * increasing the acceptance rate.

{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module RS where

import GHC.Types

import PPL
import Distributions

import qualified System.Random.MWC as MWC

-- if @m@ implies determinism --- e.g. @Identity@ --- then this is actually
-- a deterministic process. The generality of @m@ may be useful for testing,
-- since testing stochastic processes is... unreliable; but in practice it
-- will usually probably be @IO@ to use an IO generator, though a seeded gen
-- would also be possible.

rs :: RSKernel k m a => Int -> k -> m [a]
rs 0 _ = return []
rs n kernel = do
  x <- sample kernel
  α <- kernel `accept` x
  if α
    then (x:) <$> rs (n-1) kernel
    else rs (n-1) kernel

type RSKernel :: * -> (* -> *) -> * -> Constraint
class Monad m => RSKernel k m a | k -> a where
  sample :: k -> m a
  accept :: k -> a -> m Bool

-- | Classical Monte Carlo Rejection Sampling kernel
-- ^ Assume that the range of the prior subsumes the domain of the density
-- ^ Assume that the prior and target are normalised
-- * Move this to another file once it makes sense to do so
type RSMC :: * -> *
data RSMC ω = RSMC
  { prior :: Dist ω
  , priorDensity :: ω -> Double
  , targetDensity :: ω -> Double
  , gen :: MWC.GenIO
  }

instance RSKernel (RSMC ω) IO ω where
  sample :: RSMC ω -> IO ω
  sample RSMC{..} = runDist prior gen

  accept :: RSMC ω -> ω -> IO Bool
  RSMC{..} `accept` x = runDist (bernoulli' $ min 1 (targetDensity x / priorDensity x)) gen

-- rejection sampling with target [Normal 0.5 0.1]
classical :: IO ()
classical = let
  kernel gen = RSMC
    { prior = uniform01
    , priorDensity = const 1
    , targetDensity = \x ->
        let μ = 0.5 ; σ² = 0.1
        in (1 / sqrt(2 * pi * σ²)) * exp ((-1/(2*σ²)) * (x - μ)**2)
    , gen = gen
    }
  in do
  gen <- MWC.createSystemRandom
  xs <- rs 200 (kernel gen)
  print xs

-- NOTE the FunDep does allow us to sample different types using the same kernel and struct

-- take a [Dirac True] out of a [Bernoulli 0.5]
classical' :: IO ()
classical' = let
  kernel gen = RSMC
    { prior = bernoulli' 0.5
    , priorDensity = const 0.5
    , targetDensity = \x -> if x then 1 else 0
    , gen = gen
    }
  in do
  gen <- MWC.createSystemRandom
  xs <- rs 200 (kernel gen)
  print xs
