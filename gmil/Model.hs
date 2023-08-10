-- | Model
-- | Describes how to access observed variables within a model and how to
-- | generate those which are not provided.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}

module Model where

import Env

import Data.Maybe

import GHC.OverloadedLabels
import GHC.TypeLits
import GHC.Types

-- | A model is a program which takes some environment and computes some value
-- | on that environment

type Model :: [Symbol `As` Type] -> * -> *
newtype Model env a = Model { runModel :: Env env -> a }
  deriving Functor via Model env
-- NOTE derived Applicative (and hence Monad) instance is not the instance I want

instance Applicative (Model env) where
  pure :: a -> Model env a
  pure x = Model $ const x

  (<*>) :: Model env (a -> b) -> Model env a -> Model env b
  Model f <*> Model m = Model $ \env -> f env $ m env

instance Monad (Model env) where
  (>>=) :: Model env a -> (a -> Model env b) -> Model env b
  Model m >>= f = Model $ \env -> runModel (f (m env)) env

instance (KnownSymbol x, Observable env x a) => IsLabel x (Model env (Maybe a)) where
  fromLabel :: Model env (Maybe a)
  fromLabel = Model $ \env -> observe (Var :: Var x) env

egmod :: Observable env "y" Double => Model env Double
egmod = Model $ \env -> let
  y = runModel #y env
  in fromMaybe 1 y

-- | Syntactic sugar to provide a default value/distribution
(??) :: Model env (Maybe a) -> Model env a -> Model env a
mdl ?? def = mdl >>= \case { Just x -> return x ; Nothing -> def }

egmod' :: Observable env "y" Double => Model env Double
egmod' = do
  y <- #y ?? do return 1
  return y
