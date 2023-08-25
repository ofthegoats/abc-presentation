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
  propose :: k -> IO a
  accepts :: k -> a -> IO Bool

rs :: RSKernel k a => Int -> k -> IO [a]
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
  , gen :: Gen
  }

instance RSKernel (RSMC ω) ω where
  propose :: RSMC ω -> IO ω
  propose RSMC{..} = sample prior gen

  accepts :: RSMC ω -> ω -> IO Bool
  accepts RSMC{..} x = let
    α = targetDensity x / priorDensity x
    in sample (bernoulli $ min 1 α) gen

data RSABC θ ω = RSABC
  { observations :: ω
  , model :: θ -> Sampler ω
  , prior :: Sampler θ
  , distance :: ω -> ω -> Double
  , tolerance :: Double
  , gen :: Gen
  }

instance RSKernel (RSABC θ ω) θ where
  propose :: RSABC θ ω -> IO θ
  propose RSABC{..} = sample prior gen

  accepts :: RSABC θ ω -> θ -> IO Bool
  accepts RSABC{..} θ = do
    x <- sample (model θ) gen
    return $ distance x observations <= tolerance
