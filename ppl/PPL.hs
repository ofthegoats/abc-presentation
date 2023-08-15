-- | Modelling implementation for a simple likelihood-free-oriented PPL

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PPL where

import qualified System.Random.MWC as MWC

import Control.Monad
import Control.Monad.Reader

import GHC.Prim

runDist :: MonadIO m => Dist ω -> MWC.GenIO -> m ω
runDist μ g = liftIO $ runReaderT (dist μ) g

type Prob = Double

newtype Dist ω = Dist { dist :: ReaderT (MWC.Gen RealWorld) IO ω }
  deriving (Functor, Applicative, Monad, MonadIO)
