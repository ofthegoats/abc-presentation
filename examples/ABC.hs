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

import Probability ( RandVar(..) )
import Control.Monad ( replicateM, liftM2 )

-- | Approximate Bayesian Computation via Rejection Sampling
-- | Generates a list of "acceptable" parameters for the given generative model
-- | Restrictions: ϵ is constant between iterations; complete resampling straight from π
abcRejection ::
  Int -- ^ @n@ number of iterations
  -> [y] -- ^ @y@ real-world sample
  -> Double -- ^ @ϵ@ maximum distance before rejection
  -> ([y] -> s) -- ^ @s@ summary statistic function
  -> (s -> s -> Double) -- ^ @d@ distance function
  -> (p -> RandVar y) -- ^ @x@ generative model assumed
  -> RandVar p -- ^ @π@ prior
  -> RandVar [p]
abcRejection 0 _ _ _ _ _ _ = return []
abcRejection n y ϵ s dist m π = do
  θ <- π
  y' <- replicateM (length y) $ m θ
  liftM2
    (++)
    (if s y `dist` s y' <= ϵ
     then return [θ]
     else return [])
    (abcRejection (n-1) y ϵ s dist m π)
