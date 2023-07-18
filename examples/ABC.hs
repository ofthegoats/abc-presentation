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

{-# LANGUAGE ScopedTypeVariables #-}

module ABC where

import Probability
import Control.Monad ( replicateM, liftM2 )
import Data.List
import qualified System.Random.MWC as MWC

-- TODO replace with a fold-based implementation?

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

abcMH ::
  forall p y s .
  Int -- ^ @n@ number of iterations
  -> [y] -- ^ @y@ real-world sample
  -> Double -- ^ @ϵ@ maximum distance before rejection
  -> ([y] -> s) -- ^ @s@ summary statistic function
  -> (s -> s -> Double) -- ^ distance function
  -> (p -> RandVar y) -- ^ @x@ generative model assumed
  -> RandVar p -- ^ @π@ prior
  -> (p -> Double) -- ^ prior density function
  -> (p -> RandVar p) -- ^ transition kernel
  -> (p -> p -> Double) -- ^ transition kernel density function
  -> RandVar [p]
abcMH 0 _ _ _ _ _ _ _ _ _ = return []
abcMH n y ϵ s dist gen π πd q qd = do
  θ <- π
  x <- replicateM (length y) $ gen θ
  if s y `dist` s x <= ϵ
    then abcMCMC' (n-1) θ
    else abcMH (n-1) y ϵ s dist gen π πd q qd
  where abcMCMC' :: Int -> p -> RandVar [p]
        abcMCMC' 0 _ = return []
        abcMCMC' n θ = do
          θ' <- q θ
          x <- replicateM (length y) $ gen θ'
          if s y `dist` s x <= ϵ
            then liftM2 (++)
                 (let h = min 1 (πd θ' / πd θ) * (qd θ' θ / qd θ θ')
                  in (\x -> if x then return θ' else return θ) <$> bernoulli h)
                 (abcMCMC' (n-1) θ')
            else liftM2 (++) (return [θ]) (abcMCMC' (n-1) θ)

summary :: Ord y => [y] -> (y,y,y,y,y)
summary y =
  let y' = sort y
  in (head y', y' !! (length y `div` 4), y' !! (length y `div` 2), y' !! (3 * length y `div` 4), last y')

distance :: (Double,Double,Double,Double,Double) -> (Double,Double,Double,Double,Double) -> Double
distance (min₁, q1₁, q2₁, q3₁, max₁) (min₂,q1₂,q2₂,q3₂,max₂) =
  (min₁ - min₂)**2 + (q1₁ - q1₂)**2 + (q2₁ - q2₂)**2 + (q3₁ - q3₂)**2 + (max₁ - max₂)**2

model :: (Double,Double) -> RandVar Double
model (μ,ss) = gaussian μ ss

prior :: RandVar (Double,Double)
prior = (,) <$> uniform' 0 10 <*> uniform' 0 10

priorDensity :: (Double,Double) -> Double
priorDensity (μ,ss) = 1 / 100

transition :: (Double,Double) -> RandVar (Double,Double)
transition (μ,ss) = (,) <$> gaussian μ 1 <*> gaussian ss 1

transitionDensity :: (Double,Double) -> (Double,Double) -> Double
transitionDensity (μ',ss') (μ,ss) =
  let f x = (1 / sqrt (2 * pi * ss)) * exp ((-1/(2*ss)) * (x - μ)**2)
  in f μ' * f ss'

example = do
  gen <- MWC.createSystemRandom
  y <- replicateM 1000 $ random (gaussian 3 6) gen
  θ <- random (abcMH 1000 y 0.3 summary distance model prior priorDensity transition transitionDensity) gen
  print $ last θ
  print $ ((sum . map fst $ θ)/ (fromIntegral . length $ θ), (sum . map snd $ θ)/ (fromIntegral . length $ θ))
