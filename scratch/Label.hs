-- | Learning/scratch how to use OverloadedLabels GHC extension to access fields
-- | in a record (model)

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module Label where

import GHC.OverloadedLabels
import GHC.TypeLits
import GHC.Types

import Data.Maybe

type Label :: Symbol -> *
data Label l = Get

type Point :: *
data Point = Point Int Int
  deriving Show

-- * In my problem, we do not necessarily always know all the parameters of the
-- * "record", which is a model-environment.

-- * NB the actual model is just a function @Env -> Env@, which effectively
-- * acts as an interpreter "completing" the structure. Not sure yet how I can
-- * encode that in the type system...

type Obs :: * -> Symbol -> * -> Constraint
class Obs env l a | env l -> a where
  observe :: env -> Label l -> Maybe a

instance Obs env l a => IsLabel l (env -> Maybe a) where
  fromLabel :: env -> Maybe a
  fromLabel x = observe x (Get :: Label l)

data SimpleEnv = SE
  { x :: Maybe Int
  , y :: Maybe Int
  }

instance Obs SimpleEnv "x" Int where observe SE{..} _ = x
instance Obs SimpleEnv "y" Int where observe SE{..} _ = y

model :: (Obs env "x" Int, Obs env "y" Int) => env -> Double
model env = let
  x :: Int = fromMaybe 4 $ #x env -- chosen by a fair dice roll
  y :: Int = fromMaybe (x + 1) $ #y env -- guaranteed to be random
  in fromIntegral (x + y) / 2

data CuriousEnv = CE
  { x :: Maybe Int
  , y :: Maybe Int
  , z :: Double
  }

instance Obs CuriousEnv "x" Int where observe CE{..} _ = x
instance Obs CuriousEnv "y" Int where observe CE{..} _ = y
instance Obs CuriousEnv "z" Double where observe CE{..} _ = Just z
-- see that we already have all our "Maybe"s from the types in the environment...
-- so arguably we do not need it in the class

curiousmodel :: CuriousEnv -> Double
curiousmodel env = let
  z :: Double = fromMaybe (error "") $ #z env
  x :: Double = fromMaybe (4 * z) $ fromIntegral <$> #x env
  y :: Double = fromMaybe (4 * z) $ fromIntegral <$> #y env
  in (x + y) / 2
