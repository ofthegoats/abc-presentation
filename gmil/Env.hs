-- | Model environment
-- | Describes which random variables are observed and whether they have a
-- | value.

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}

module Env where

import Data.Maybe

import GHC.OverloadedLabels
import GHC.TypeLits
import GHC.Types

-- | Label for observable variables
type Var :: Symbol -> *
data Var x where
  Var :: KnownSymbol x => Var x

-- | Turn @#x@ into @Var "x"@
instance (KnownSymbol x, x ~ x') => IsLabel x (Var x') where
  fromLabel :: Var x'
  fromLabel = Var

-- | List of observable variables
-- | We don't want a variable to appear in this list more than once --- so it's
-- | like an ordered set
type Vars :: [Symbol] -> *
data Vars xs where
  VNil :: Vars '[]
  VCons :: x `NotIn` xs ~ 'True => Var x -> Vars xs -> Vars (x ': xs)

vnil :: Vars '[]
vnil = VNil

infixr 6 <#>
(<#>) :: x `NotIn` xs ~ 'True => Var x -> Vars xs -> Vars (x ': xs)
(<#>) = VCons

-- | Assign a symbol to a type
-- | Later we will see that its type *might* be known
type As :: * -> Type -> *
data x `As` a = x := a

type Env :: [Symbol `As` Type] -> *
data Env env where
  ENil :: Env '[]
  ECons :: x `NotIn` GetVars env ~ 'True => Maybe a -> Env env -> Env (x := a ': env)

enil :: Env '[]
enil = ENil

infixr 6 <:>
(<:>) :: x `NotIn` GetVars env ~ 'True => Var x `As` Maybe a -> Env env -> Env (x := a ': env)
(_ := a) <:> env = ECons a env

class Observable env x a | env x -> a where
  observe :: Var x -> Env env -> Maybe a

instance {-# OVERLAPPABLE #-} Observable env x a
  => Observable (y := b ': env) x a where
  observe :: Var x -> Env (y := b ': env) -> Maybe a
  observe x (ECons _ env) = observe x env

instance {-# OVERLAPS #-} Observable (x := a ': env) x a where
  observe :: Var x -> Env (x := a ': env) -> Maybe a
  observe _ (ECons a _) = a

eg2 :: Env '["x" := Int, "y" := Double]
eg2 = #x := Just 3 <:> #y := Nothing <:> enil

-- >>> observe (Var :: Var "x") eg2
-- >>> observe (Var :: Var "y") eg2
-- Just 3
-- Nothing

type NotIn :: Symbol -> [Symbol] -> Bool
type family NotIn x xs where
  x `NotIn` '[] = True
  x `NotIn` x ': xs = False
  x `NotIn` (x' ': xs) = x `NotIn` xs

type GetVars :: [Symbol `As` Type] -> [Symbol]
type family GetVars env where
  GetVars '[] = '[]
  GetVars (x := _ ': env') = x ': GetVars env'
