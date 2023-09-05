{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sampler where

import qualified System.Random.MWC as MWC

import Control.Monad.Reader

type Gen = MWC.GenIO

newtype Sampler a = Sampler { runSampler :: ReaderT Gen IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

sample :: Sampler a -> Gen -> IO a
sample = runReaderT . runSampler
