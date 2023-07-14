-- | Approximate Bayesian Computation
-- |
-- | Presenting an example of ABC rejection sampling (at minimum) for some distribution (normal? or something more exotic...)
-- | Would like to present ABC with MCMC sampling (more efficient)
-- |
-- | Would like to then generalise it... but probably only after refactoring @Probability.hs@ or in a new project
-- |
-- | See Umberto Picchini's slides https://www.maths.lu.se/fileadmin/maths/forskning_research/InferPartObsProcess/abc_slides.pdf
-- | Then further reading could be used to make this more efficient
-- |
-- | After ABC, learn Bayesian Synthetic Likelihood methods and present this as well

module ABC where

import Probability ( RandVar(..), raise, random, uniform , uniform' )
import Control.Monad ( replicateM )

-- * definition ripped from @Probability.hs@
--
-- * see that there is no density function for the normal distribution (!that we can access!)
normal :: Double -> Double -> RandVar Double
normal μ σ² =
  let u = uniform
      v = uniform
      z = sqrt (-2 * log u) * cos (2 * pi * v)
  in raise $ \gen -> do
    z' <- random z gen
    return (sqrt σ² * z' + μ)

-- restrictive summary class... in general this bit should be left to the user
-- it just needs to compose with the distance function, which I have hardcoded
data Summary =
  S { μ :: Double
    , σ² :: Double
    }
  deriving Show

-- by COINCIDENCE parameters are the same as the summary
type Parameters = Summary

-- | approximate bayesian computation rejection sampling
-- | extremely simple example with constant ϵ, uniform prior, decided distance function
-- | but really these should be provided by the user
-- | potentially with a step function, to vary them with every iteration!
--
-- * this example also requires μ ∈ (0, 5), σ² ∈ (0, 10)
abc_rs :: Int -> [Double] -> Double -> RandVar [Summary]
abc_rs 0 _ _ = return []
abc_rs n y ϵ = raise $ \gen -> do
  μθ <- random (uniform' 0 5) gen
  σ²θ <- random (uniform' 0 10) gen
  y' <- replicateM (length y) $ random (normal μθ σ²θ) gen
  next <- random (abc_rs (n-1) y ϵ) gen
  if dist y' <= ϵ
    then return $ S μθ σ²θ : next
    else return next
  where
    s :: [Double] -> Summary
    s xs =
      let mean = sum xs / fromIntegral (length xs)
          variance = sum (map (\x -> (x - mean)**2) xs) / (fromIntegral (length xs) - 1)
      in S mean variance
    dist :: [Double] -> Double
    dist y' =
      let sy = s y
          sy' = s y'
      in (μ sy - μ sy')**2 + (σ² sy - σ² sy')**2

-- >>> import qualified System.Random.MWC as MWC
-- >>> gen <- MWC.createSystemRandom
-- >>> y <- replicateM 1000 $ random (normal 2 6) gen
-- >>> θ <- random (abc_rs 500 y 1) gen
-- >>> sum (map μ θ) / (fromIntegral $ length θ)
-- >>> sum (map σ² θ) / (fromIntegral $ length θ)

{-
EXTENSION
- [ ] generalise summary statistic, provided by user
- [ ] implement MCMC variant
  - [ ] or ask user for sampling and acceptance function?
  - all this sort of continuation can be done LATER
  - FOR NOW I just want to generalise rejection-based sampling, implement MCMC-based sampling then do some reading on summary statistics

the user WILL provide functions to work with summary statistics...
but I will provide some example ones

then I would like to make an example with a more complicated distribution
-}
