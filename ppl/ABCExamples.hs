-- | Examples of using the ABC module

{-# LANGUAGE DuplicateRecordFields #-}

module ABCExamples where

import PPL
import ABC
import RS
import MH
import Distributions

import qualified System.Random.MWC as MWC

import Control.Monad

-- Example of ABC rejection sampling for [Binomial 10 0.4]
binomialExampleRS :: IO [Double]
binomialExampleRS = let
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
  rs 1000 (kernel gen)

-- Example of Gaussian Metropolis sampling for [Binomial 10 0.4]
binomialExampleGM :: IO [Double]
binomialExampleGM = let
  summary x = (fromIntegral . sum) x / (fromIntegral . length) x
  kernel gen = GMABC
    { observations = summary [4,4,6,6,5,3,3,3,3,6,4,6,5,2,3,5,3,5,4,1,4,4,4,5,2,4,6,6,6,3]
    , model = \p -> summary <$> replicateM 30 (binomial' 10 p)
    , priorDensity = const 1
    , σ² = 0.08
    , distance = \x y -> (x - y)**2
    , tolerance = 1.0
    , gen = gen
    }
  in do
  gen <- MWC.createSystemRandom
  x <- mh 1000 0.5 (kernel gen)
  return $ drop 100 x

main :: IO ()
main = do
  binomialExampleRS >>= print
  binomialExampleGM >>= print
