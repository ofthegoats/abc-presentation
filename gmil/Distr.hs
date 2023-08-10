-- | Basic distributions

{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}

module Distr where

import Model

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

import Control.Monad.Reader

import GHC.OverloadedLabels
import GHC.TypeLits
import GHC.Types

type Random :: * -> *
type Random a = ReaderT MWC.GenIO IO a

type Prob :: *
type Prob = Double

type Dist :: * -> * -> Constraint
class Dist d a | d -> a where
  callDist :: d -> Random a

  mkModel :: d -> Model env (Random a)
  mkModel d = Model $ \env -> callDist d

type Uniform :: *
data Uniform = Uniform Double Double deriving Show
instance Dist Uniform Double where
  callDist :: Uniform -> ReaderT MWC.GenIO IO Double
  callDist (Uniform a b) | b >= a = ask >>= fmap (\x -> (b - a) * x + a) . MWC.uniform

type Bernoulli :: *
newtype Bernoulli = Bernoulli Prob deriving Show
instance Dist Bernoulli Bool where
  callDist :: Bernoulli -> ReaderT MWC.GenIO IO Bool
  callDist (Bernoulli p) | 0 <= p && p <= 1 = ask >>= MWC.bernoulli p

type Binomial :: *
data Binomial = Binomial Int Prob deriving Show
instance Dist Binomial Int where
  callDist :: Binomial -> ReaderT MWC.GenIO IO Int
  callDist (Binomial n p) | n >= 0 && 0 <= p && p <= 1 = length . filter id <$> replicateM n (callDist $ Bernoulli p)

type Beta :: *
data Beta = Beta Double Double deriving Show
instance Dist Beta Double where
  callDist :: Beta -> ReaderT MWC.GenIO IO Double
  callDist (Beta α β) | α > 0 && β > 0 = ask >>= MWC.beta α β
