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

-- | TODO gaussian distribution
-- * TODO more complicated example of making another distribution (Box-Muller)
gaussian :: Double -> Double -> RandVar Double
gaussian μ σ² = error "TODO implement gaussian"

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

-- TODO some simple inference methods
