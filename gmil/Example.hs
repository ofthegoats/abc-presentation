-- | Copying the example used in the Anglican documentation

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}

module Example where

import Env
import Model
import Distr

import Data.Maybe

import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

import GHC.OverloadedLabels
import GHC.TypeLits
import GHC.Types

-- | Suppose that you are thinking about purchasing a factory that makes
-- | pencils. Your accountants have determined that you can make a profit (i.e.
-- | you should transact the purchase) if the percentage of defective pencils
-- | manufactured by the factory is less than 30%. In your prior experience, you
-- | learned that, on average, pencil factories produce defective pencils at a
-- | rate of 50%. To make your judgement about the efficiency of this factory
-- | you test pencils one at a time in sequence as they emerge from the factory
-- | to see if they are defective.

type FactoryModel =
  '[ "p" := Double -- ^ probability that a pencil is adequate
   , "k" := Int    -- ^ number of adequate pencils
   ]

(??) :: Model env (Maybe a) -> Model env (Random a) -> Model env (Random a)
mdl ?? dst = mdl >>= \case
  { Just x -> Model . const $ return x
  ; Nothing -> dst
  }

-- * NOTE since I don't have access to any densities etc. I need to be able to
-- * describe something like Approximate Bayesian Computation in @pencilFactory@
-- * then be able to stick that into a driver, e.g. a rejection-sampling
-- * inference algorithm.

-- need to recap how those algorithms work and then discern how that can fit
-- into a system like this.

pencilFactory :: forall env.
  ( Observable env "p" Prob
  )
  => Int
  -> Model env (Random Double)
pencilFactory n = do
  p <- #p ?? mkModel (Uniform 0 1)
  return p


-- >>> import Control.Monad.Reader
-- >>> gen <- MWC.createSystemRandom
-- >>> runReaderT (runModel (pencilFactory 10) egenv) gen
-- 0.6320499159978757

egenv :: Env '["p" := Prob]
egenv = #p := Nothing <:> enil
