-- | Rejection Sampling as described in Marjoram et al. 2003.

-- | Since rejection sampling is a fairly ubuquitous method, I provide a general
-- | Bayesian algorithm for it.

{-# LANGUAGE RecordWildCards #-}

module RS where

import PPL

-- | It is a good idea for ω to represent the summary statistic.
data RSPars ω θ =
  RSPars { observed :: ω
         , distance :: ω -> ω -> Double
         , tolerance :: Double
         , prior :: Dist θ
         , model :: θ -> Dist ω
         }

rs :: Int -> RSPars ω θ -> Dist [θ]
rs 0 _ = return []
rs n par@RSPars{ .. } = do
  θ <- prior
  x <- model θ
  if x `distance` observed <= tolerance
    then (θ:) <$> rs (n-1) par
    else rs (n-1) par
