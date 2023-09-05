-- | Rejection Sampling

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Rejection where

import Sampler
import Distributions

import Data.List

import Control.Monad

import qualified System.Random.MWC as MWC

class RSKernel k a | k -> a where
  propose :: k -> Sampler a
  accepts :: k -> a -> Sampler Bool

rs :: RSKernel k a => Int -> k -> Sampler [a]
rs 0 _ = return []
rs n k = do
  x <- propose k
  a <- k `accepts` x
  if a
    then (x:) <$> rs (n-1) k
    else rs (n-1) k

data RSMC ω = RSMC
  { prior :: Sampler ω
  , priorDensity :: ω -> Double -- ^ scaled by M
  , targetDensity :: ω -> Double
  }

instance RSKernel (RSMC ω) ω where
  propose :: RSMC ω -> Sampler ω
  propose RSMC{..} = prior

  accepts :: RSMC ω -> ω -> Sampler Bool
  accepts RSMC{..} x = let
    α = targetDensity x / priorDensity x
    in (bernoulli $ min 1 α)

data RSABC θ ω = RSABC
  { observations :: ω
  , model :: θ -> Sampler ω
  , prior :: Sampler θ
  , distance :: ω -> ω -> Double
  , tolerance :: Double
  }

instance Eq ω => RSKernel (RSABC θ ω) θ where
  propose :: RSABC θ ω -> Sampler θ
  propose RSABC{..} = prior

  accepts :: RSABC θ ω -> θ -> Sampler Bool
  accepts RSABC{..} θ = do
    x <- model θ
    return $ distance x observations <= tolerance
