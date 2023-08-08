-- | Reading/writing through Env.hs by Min
-- | Trying to understand approach used

-- What I want to do slightly differently, is that I have just @a@ as opposed to
-- @[a]@ inside the @Env@.
-- Perhaps the easiest way to do this would just be to abuse the @Maybe@ functor?

-- Before I do this, myself, I should get a firm understanding of how Min's
-- implementation works and why all the parts are necessary

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE OverloadedLabels #-}

module Env where

import GHC.OverloadedLabels
import GHC.TypeLits
import GHC.Types

-- * Observable variable

type Var :: Symbol -> *
data Var x where
  Var :: KnownSymbol x => Var x

instance (KnownSymbol x, x ~ x') => IsLabel x (Var x') where
  fromLabel :: Var x'
  fromLabel = Var

type Vars :: [Symbol] -> *
data  Vars xs where
  VNil :: Vars '[]
  VCons :: Vars xs -> Vars (x : xs)

vnil :: Vars '[]
vnil = VNil

infixr 5 <#>
(<#>) :: Var x -> Vars xs -> Vars (x : xs)
x <#> xs = VCons xs

-- * Environment

type Assign :: * -> * -> *
data Assign x a = x := a

type Env :: [Assign Symbol *] -> *
data Env env where
  ENil :: Env '[]
  ECons :: [a] -> Env env -> Env ( x := a : env )

enil :: Env '[]
enil = ENil

infixr 5 <:>
(<:>) :: UniqueVar x env ~ 'True => Assign (Var x) [a] -> Env env -> Env ((x ':= a) ': env)
(_ := a) <:> env = ECons a env

-- NB @UniqueVar@ says if @x@ is NEW to the @env@
type UniqueVar :: Symbol -> [Assign Symbol Type] -> Bool
type family UniqueVar x env where
  UniqueVar x ( x := a : env) = False
  UniqueVar x ( x' := a : env ) = UniqueVar x env
  UniqueVar x '[] = True

-- * Observables
-- basically a wrapper around those above
-- then something similar to dependent typing?

type Observable :: [Assign Symbol *] -> Symbol -> * -> Constraint
class Observable env x a where
  get :: Var x -> Env env -> [a]
  set :: Var x -> [a] -> Env env -> Env env

-- define a way to access the variable @x@ --- NB this won't work when it's in the tail
instance {-# OVERLAPS #-} Observable ( x := a : env ) x a where
  get :: Var x -> Env (x := a : env) -> [a]
  get _ (ECons a env) = a

  set :: Var x -> [a] -> Env ( x := a : env ) -> Env ( x := a : env )
  set _ a' (ECons a env) = ECons a' env

-- so let's define a way to get at it when it's in the @env@ i.e. tail
instance {-# OVERLAPPABLE #-} Observable env x a => Observable ( y := b : env ) x a where
  get :: Var x -> Env ( y := b : env ) -> [a]
  get x (ECons a env) = get x env

  set :: Var x -> [a] -> Env ( y := b : env ) -> Env ( y := b : env )
  set x a' (ECons a env) = ECons a (set x a' env)

-- NB we haven't specified here that y /= x
-- we could use the OVERLAPS pragma, though this isn't very good Haskell
-- perhaps there is a condition similar to @UniqueVar@ we could use instead?

-- builds up a constraint, that all the variables in xs need to have @Observable@
-- so this is just a "sugar"
type Observables :: [Assign Symbol *] -> [Symbol] -> * -> Constraint
type family Observables env xs a where
  Observables env (x : xs) a = (Observable env x a, Observables env xs a)
  Observables env '[] a = ()

-- * attempt at usage

type Model :: [Assign Symbol *] -> * -> *
newtype Model env a = Model { runModel :: Env env -> a }
  deriving (Functor, Applicative, Monad)

-- HACK? since we're pulling the value out based on the type level... we don't
-- actually use the @undefined@ anywhere, so this is perfectly fine?
instance Observable env x a => IsLabel x (Model env a) where
  fromLabel :: Model env a
  fromLabel = Model $ \env -> head $ get (undefined :: Var x) env

-- * I would like to make a specification that says "these are variables which
--   need to be provided and these are those which do not..." this seems perfectly
--   doable if we just put a @Maybe@ functor in the right place.

-- μ ~ Beta a b ;; model for Normal μ σ²
type HModel =
  '[ "a" := Double
   , "b" := Double
   , "ss" := Double
   ]

-- NB in ProbFX the distributions have an additional parameter which checks to see if there's any
-- value in the tag. If there is, then it runs deterministically.
-- Whilst this is a neat approach, I don't like it since it means for any user-defined
-- distribution there must be additional boilerplate --- I would prefer to create som syntactic sugar.
-- For now, I will for some variables assume that they always exist and for others that they do not.

hierarchy :: Observables env '["a", "b", "ss"] Double => Model env Double
hierarchy = do
  a <- #a
  b <- #b
  ss <- return 3
  let mu = a + b
  return $ mu * ss

env :: Env HModel
env = (#a := [1]) <:> (#b := [2]) <:> (#ss := [3]) <:> enil

-- * it works :D
