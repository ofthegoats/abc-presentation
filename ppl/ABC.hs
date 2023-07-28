-- | Implementations of standard ABC kernels for different inference methods

{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module ABC where

import PPL
import RS
import MH

import qualified System.Random.MWC as MWC

-- | Approximate Bayesian Computation via Monte Carlo Rejection Sampling
-- | à la Marjoram et al. 2003

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

abcMCMC :: Int -> ω -> (ω -> ω -> Double) -> [Double] -> Dist θ -> (θ -> Double) -> (θ -> Dist θ) -> (θ -> θ -> Double) -> (θ -> Dist ω) -> Maybe θ -> Dist [θ]
abcMCMC n obs dist tols pri priD pro proD mod θ = mh n $ MHPars obs dist tols pri priD pro proD mod θ
