-- | Metropolis Hastings as described in Marjoram et al. 2003.

-- | Since Metropolis-Hastings is a fairly ubiquitous method, I provide a general
-- | Bayesian algorithm for it.

{-# LANGUAGE RecordWildCards #-}

module MH where

import PPL
import Distributions

data MHPars ω θ =
  MHPars { observed :: ω
         , distance :: ω -> ω -> Double
         , tolerances :: [Double]
         , prior :: Dist θ
         , priorDensity :: θ -> Double
         , proposal :: θ -> Dist θ
         , proposalDensity :: θ -> θ -> Double
         , model :: θ -> Dist ω
         , lastParameter :: Maybe θ
         }

mh :: Int -> MHPars ω θ -> Dist [θ]
mh 0 _ = return []
mh n par@MHPars{ lastParameter=Nothing, ..} =
  prior >>= \θ -> mh n par { lastParameter = Just θ }
mh n par@MHPars{ tolerances = ϵ:ts', lastParameter = Just last, ..} = do
  θ <- proposal last
  x <- model θ
  accept <- bernoulli' $ min 1 $ (priorDensity θ * proposalDensity θ last) / (priorDensity last * proposalDensity last θ)
  if x `distance` observed <= ϵ && accept
    then (θ:) <$> mh (n-1) par { lastParameter = Just θ, tolerances = ts' }
    else (last:) <$> mh (n-1) par
