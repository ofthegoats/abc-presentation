-- | Common distributions implemented as generative models

module Distributions where

import PPL

import qualified System.Random.MWC as MWC

import Control.Monad
import Control.Monad.Reader

type Uniform = Dist Double
uniform01 :: Uniform
uniform01 = Dist $ ask >>= MWC.uniform

uniform' :: Double -> Double -> Uniform
uniform' a b = (\x -> a + (b-a)*x) <$> uniform01

type Bernoulli = Dist Bool
bernoulli' :: Prob -> Bernoulli
bernoulli' p = (<=p) <$> uniform01

type Binomial = Dist Int
binomial' :: Int -> Prob -> Binomial
binomial' n p = length . filter id <$> replicateM n (bernoulli' p)

type Dirac ω = Dist ω
dirac' :: ω -> Dirac ω
dirac' x = return x

type Normal = Dist Double
normal' :: Double -> Double -> Normal
normal' μ σ² =
  (\u1 u2 -> μ + σ² * sqrt (-2 * log u1) * cos (2 * pi * u2))
  <$> uniform01 <*> uniform01
