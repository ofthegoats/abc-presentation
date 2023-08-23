module Distributions where

import Sampler

import qualified System.Random.MWC as MWC

import Control.Monad
import Control.Monad.Reader

-- | Prob in range [0,1]
type Prob = Double

-- | Uniform standard continuous
random :: Sampler Double
random = Sampler $ ask >>= MWC.uniform

uniform :: Double -> Double -> Sampler Double
uniform a b | a <= b = (\x -> (b - a) * x + a) <$> random

bernoulli :: Prob -> Sampler Bool
bernoulli p = (<=p) <$> random

binomial :: Int -> Prob -> Sampler Int
binomial n p = length . filter id <$> replicateM n (bernoulli p)

gaussian :: Double -> Double -> Sampler Double
gaussian μ σ² =
  (\u1 u2 -> μ + σ² * sqrt (-2 * log u1) * cos (2 * pi * u2))
  <$> random <*> random
