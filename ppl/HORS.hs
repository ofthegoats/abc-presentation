-- | "Higher Order" Rejection Sampling

-- | A generalisation of rejection sampling, where the algorithm
-- | is provided with a "kernel" which will
-- | (a) provide samples
-- | (b) decide whether or not to accept the sample

-- | My idea is that there's more than one type of thing that can do this... there
-- | might be lots of useful ways to represent these kernels, dependent on the type
-- | of inference (regular sampling or Bayesian) benefiting from varying levels of
-- | user-defined abstraction. I will provide two examples.

-- | This would also be fairly easy to generalise to importance sampling, which just
-- | introduces a Bernoulli decision on whether or not to accept a particular sample,
-- | increasing the acceptance rate.

{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HORS where

import GHC.Types

import PPL
import Distributions

import qualified System.Random.MWC as MWC
import Control.Monad.Identity

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

-- dummy example kernel, which always produces @mempty@ and always accepts values
instance Monoid ω => RSKernel (RSMC ω) Identity ω where
  sample :: RSMC ω -> Identity ω
  sample = return . mempty

  accept :: RSMC ω -> ω -> Identity Bool
  accept _ _ = return True

identity :: Identity [String]
identity = let
  kernel gen = RSMC
    { prior = dirac' [] :: Dist [Char]
    , priorDensity = const 1
    , targetDensity = const 1
    , gen = gen
    }
  in rs 200 (kernel undefined)

type RSABC :: * -> * -> *
data RSABC θ ω = RSABC
  { observations :: ω
  , prior :: Dist θ
  , model :: θ -> Dist ω
  , distance :: ω -> ω -> Double
  , tolerance :: Double
  , gen :: MWC.GenIO
  }

instance RSKernel (RSABC θ ω) IO θ where
  sample :: RSABC θ ω -> IO θ
  sample RSABC{..} = runDist prior gen

  accept :: RSABC θ ω -> θ -> IO Bool
  RSABC{..} `accept` θ = do
    x <- runDist (model θ) gen
    return $ x `distance` observations <= tolerance

-- ABC example on [Binomial 10 0.4]
abc :: IO ()
abc = let
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
