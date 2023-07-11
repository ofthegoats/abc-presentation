module Probability where

import qualified System.Random.MWC as MWC
-- NOTE this provides most of these distributions, probably /much/ faster. Below purely for reference!

import Control.Monad ( replicateM )
import Control.Monad.Trans.Reader ( ReaderT(runReaderT), ask )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Data.Foldable ( minimumBy )

newtype RandVar a = RandVar { sample :: ReaderT MWC.GenIO IO a }

instance Functor RandVar where
  fmap f x = raise $ \gen -> do { x' <- random x gen ; return $ f x' }

instance Applicative RandVar where
  pure x = deterministic x
  f <*> x = raise $ \gen -> do { f' <- random f gen ; x' <- random x gen ; return $ f' x' }

instance Monad RandVar where
  x >>= f = raise $ \gen -> do { x' <- random x gen ; random (f x') gen }

-- | auxiliary function to write new distributions idiomatically
raise :: (MWC.GenIO -> IO a) -> RandVar a
raise f = RandVar $ ask >>= lift . f

-- | sample from a random variable, also given a generator
random :: RandVar a -> MWC.GenIO -> IO a
random x g = runReaderT (sample x) g

-- | standard uniform distribution
uniform :: RandVar Double
uniform = raise $ \gen -> MWC.uniform gen

-- | generalised uniform distribution
uniform' :: Double -> Double -> RandVar Double
uniform' a b = raise $ \gen -> do
  x <- random uniform gen
  return $ a + x * (b - a)

-- | bernoulli distribution
-- * example of making a distribution with an inverse CDF
bernoulli :: Double -> RandVar Bool
bernoulli p = raise $ \gen -> do
  x <- random uniform gen
  return $ x <= p

-- | categorical distribution --- generalises bernoulli
-- ^ @v@ does not need to be normalised, this function will normalise it
categorical :: [(a, Double)] -> RandVar a
categorical v = raise $ \gen -> do
  x <- random uniform gen
  let w' = filter (\i -> snd i >= x) w
  return . fst . minimumBy (\x y -> snd x `compare` snd y) $ w'
  where
    s = sum . map snd $ v
    w = fst $ foldr (\(x, p) (aca, acp) -> let p' = p/s in ((x, acp + p') : aca, p' + acp)) ([], 0) v
    -- [(val, UB)]

deterministic :: a -> RandVar a
deterministic x = raise $ const $ return x

-- | binomial distribution
-- * example of making a distribution using another distribution
-- * example of using @fmap@ on a random variable
binomial :: Int -> Double -> RandVar Int
binomial n p = raise $ \gen -> do
  xs <- replicateM n $ random (bernoulli') gen
  return $ sum xs
  where
    bernoulli' :: RandVar Int
    bernoulli' = (\x -> if x then 1 else 0) <$> bernoulli p

-- | geometric distribution
-- * example of a distribution defined recursively
geometric :: Double -> RandVar Int
geometric p = raise $ \gen -> do
  x <- random (bernoulli p) gen
  if x
    then return 0
    else do
      z <- random (geometric p) gen
      return $ 1 + z
-- | gaussian distribution
-- * more complicated example of making another distribution (Box-Muller)
-- * TODO replace with polar method, to show rejection sampling
gaussian :: Double -> Double -> RandVar Double
gaussian μ σ² =
  let u = uniform
      v = uniform
      z = sqrt (-2 * log u) * cos (2 * pi * v)
  in raise $ \gen -> do
    z' <- random z gen
    return (sqrt σ² * z' + μ)

-- * toy example of a higher-order random variable
equal :: Eq a => RandVar a -> RandVar a -> RandVar Bool
equal x y = raise $ \gen -> do
  x₀ <- random x gen
  y₀ <- random y gen
  return $ x₀ == y₀

-- * toy example of a discrete distribution
d6 :: RandVar Int
d6 = categorical [(d, 1) | d <- [1 .. 6]]

-- use random variables like numbers
instance Num a => Num (RandVar a) where
  x + y = raise $ \gen -> do { x' <- random x gen ; y' <- random y gen ; return $ x' + y' }
  x * y = raise $ \gen -> do { x' <- random x gen ; y' <- random y gen ; return $ x' * y' }
  abs x = raise $ \gen -> do { x' <- random x gen ; return $ abs x' }
  signum x = raise $ \gen -> do { x' <- random x gen ; return $ signum x'}
  negate x = raise $ \gen -> do { x' <- random x gen ; return $ negate x'}
  fromInteger x = deterministic $ fromInteger x

-- the pierogi example I used earlier, to demonstrate how having random variables rather than just distributions should be useful
pierogi :: RandVar Int
pierogi = abs $ d6 - 3

-- continue the trend with particularly useful mathematical functions
instance Fractional a => Fractional (RandVar a) where
  recip x = raise $ \gen -> do { x' <- random x gen ; return $ recip x' }
  fromRational x = deterministic $ fromRational x

instance Floating a => Floating (RandVar a) where
  exp x = raise $ \gen -> do { x' <- random x gen ; return $ exp x'}
  log x = raise $ \gen -> do { x' <- random x gen ; return $ log x'}
  sin x = raise $ \gen -> do { x' <- random x gen ; return $ sin x'}
  cos x = raise $ \gen -> do { x' <- random x gen ; return $ cos x'}
  asin x = raise $ \gen -> do { x' <- random x gen ; return $ asin x'}
  acos x = raise $ \gen -> do { x' <- random x gen ; return $ acos x'}
  atan x = raise $ \gen -> do { x' <- random x gen ; return $ atan x'}
  sinh x = raise $ \gen -> do { x' <- random x gen ; return $ sinh x'}
  cosh x = raise $ \gen -> do { x' <- random x gen ; return $ cosh x'}
  asinh x = raise $ \gen -> do { x' <- random x gen ; return $ asinh x'}
  acosh x = raise $ \gen -> do { x' <- random x gen ; return $ acosh x'}
  atanh x = raise $ \gen -> do { x' <- random x gen ; return $ atanh x' }
  pi = deterministic pi


-- * a linear model and some inference over it
lm :: Double -> RandVar Double
lm x =
  let
    α = 2
    β = 1
    e = gaussian 0 3
  in raise $ \gen -> do
    e' <- random e gen
    return $ α + x * β + e'

infer_lm_lse :: [(Double, Double)] -> (Double, Double)
infer_lm_lse v =
  let
    n = fromIntegral $ length v
    μx = ((sum . map fst) v) / n
    μy = ((sum . map snd) v) / n
    ssxx = sum . map (\(x, _) -> (x - μx)^2) $ v
    ssxy = sum . map (\(x, y) -> (x - μx) * (y - μy)) $ v
    β = ssxy / ssxx
    α = μy - β * μx
  in (α, β)

-- expecting something around (2, 1)
--
-- >>> g <- MWC.createSystemRandom
-- >>> ys <- mapM (\x -> random (lm x) g) [1 .. 100]
-- >>> let d = zip [1 .. 100] ys
-- >>> infer_lm_lse d
-- (2.308739972208116,0.9967936772561005)

-- TODO some simple inference methods, generalised
