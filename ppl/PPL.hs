{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PPL where

import qualified System.Random.MWC as MWC

import Control.Monad
import Control.Monad.Reader

sample :: MonadIO m => Dist ω -> MWC.GenIO -> m ω
sample μ g = liftIO $ runReaderT (runDist μ) g

type Prob = Double

newtype Dist ω = Dist { runDist :: ReaderT MWC.GenIO IO ω }
  deriving (Functor, Applicative, Monad)

type Uniform = Dist Double
uniform01 :: Uniform
uniform01 = Dist $ do
  gen <- ask
  MWC.uniform gen

uniform' :: Double -> Double -> Uniform
uniform' a b = Dist $ do
  gen <- ask
  x <- sample uniform01 gen
  return $ (b-a) * x + a

type Bernoulli = Dist Bool
bernoulli' :: Prob -> Bernoulli
bernoulli' p = (<=p) <$> uniform01

type Binomial = Dist Int
binomial' :: Int -> Prob -> Binomial
binomial' n p = length . filter id <$> replicateM n (bernoulli' p)

type Dirac ω = Dist ω
dirac' :: ω -> Dirac ω
dirac' x = return x
