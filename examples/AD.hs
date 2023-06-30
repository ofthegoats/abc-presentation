-- | Automatic Differentiation module

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module AD where

import qualified Data.Map as M

f₁ :: Floating x => x -> x
f₁ x = x ** 2 + 2 * x - 2

f₂ :: Floating x => x -> x
f₂ x = x / (2 * (x ** 2) + 1)

f₃ :: Floating x => x -> x
f₃ x = exp (sin x)

class Monoid e => AbelianGroup e where
  zero :: e
  zero = mempty
  invert :: e -> e

{- NB we (should be) guaranteed equality defined for d.
   This is because d admits a field.
   Fields have e.g. commutativity, which is defined with equality (as are many other properties)
   So there must be some notion of equality on d.
-}

class (Eq d, Num d, AbelianGroup e) => VectorSpace d e | e -> d where
  (#) :: d -> e -> e

class VectorSpace d e => Kronecker v d e where
  delta :: v -> e

raiseᴺ :: (Kronecker v d e) => d -> v -> Nagata d e
raiseᴺ d v = N d $ delta v

data Nagata d e = N { primalᴺ :: d , tangentᴺ :: e }
  deriving stock (Show)

instance (Eq d, Num d, VectorSpace d e) => Num (Nagata d e) where
  (N f df) + (N g dg) = N (f + g) (df <> dg)
  (N f df) * (N g dg) = N (f * g) (f # dg <> g # df)
  negate (N f df) = N (negate f) (invert df)
  fromInteger z = N (fromInteger z) mempty
  abs (N f df)
    | signum f == -1 = N (abs f) (invert df)
    | signum f == 0  = N f (error "derivative of abs at 0 is undefined!")
    | signum f == 1  = N f df
    | otherwise = error "unexpected return from signum"

instance (Fractional d, VectorSpace d e) => Fractional (Nagata d e) where
  recip (N f df) = N (recip f) ((- recip (f ^^ 2)) # df)
  fromRational q = N (fromRational q) zero

instance (Floating d, VectorSpace d e) => Floating (Nagata d e) where
  pi = N pi zero
  exp (N f df) = N (exp f) (exp f # df)
  log (N f df) = N (log f) (recip f # df)
  sin (N f df) = N (sin f) (cos f # df)
  cos (N f df) = N (cos f) ((- sin f) # df)
  asin (N f df) = N (asin f) ((recip . sqrt) (1 - f ** 2) # df)
  acos (N f df) = N (acos f) ((negate . recip . sqrt) (1 - f ** 2) # df)
  atan (N f df) = N (atan f) (recip (1 + f ** 2) # df)
  sinh (N f df) = N (sinh f) (cosh f # df)
  cosh (N f df) = N (cosh f) (sinh f # df)
  asinh (N f df) = N (asinh f) ((recip . sqrt) (f ** 2 + 1) # df)
  acosh (N f df) = N (acosh f) ((recip . sqrt) (f ** 2 - 1) # df)
  atanh (N f df) = N (atanh f) (recip (1 - f ** 2) # df)

g₁ :: Floating x => x -> x -> x
g₁ x y = sinh x ** exp y + 2

{- NOTE on Dense representation
   A sensible function would be something like (String -> d) where String = "d/dx" or something...
   But this does just seem less useful than the Sparse representation, at least for interactive use.
-}

newtype Dense x d = Dense (x -> d)

instance (Num d) => Semigroup (Dense x d) where
  (Dense f) <> (Dense g) = Dense $ \x -> f x + g x

instance (Num d) => Monoid (Dense x d) where
  mempty = Dense $ const 0

instance (Num d) => AbelianGroup (Dense x d) where
  invert (Dense f) = Dense $ \x -> (negate . f) x

instance (Eq d, Num d) => VectorSpace d (Dense x d) where
  μ # (Dense f) = Dense $ \x -> μ * f x

instance (Eq x, Eq d, Num d) => Kronecker x d (Dense x d) where
  delta v = Dense $ \w -> if v == w then 1 else 0


-- >>> let (N pri (Dense tan)) = g₁ (N pi $ Dense $ \case { 'x' -> 1.0 ; _ -> 0.0 }) (N 1 $ Dense $ \case {'y' -> 1.0 ; _ -> 0.0})
-- >>> pri
-- >>> (tan 'x', tan 'y')
-- 775.1583323182499
-- (2109.5263988877678,5141.877007189466)

newtype Sparse x d = Sparse (M.Map x d)
  deriving stock (Show)
  deriving (Monoid) via (M.Map x d)
  deriving (Functor) via (M.Map x)

instance (Ord x, Num d) => Semigroup (Sparse x d) where
  (Sparse m₁) <> (Sparse m₂) = Sparse $ M.unionWith (+) m₁ m₂

instance (Ord x, Num d) => AbelianGroup (Sparse x d) where
  invert m = negate <$> m

instance (Eq d, Ord x, Num d) => VectorSpace d (Sparse x d) where
  μ # m = (*μ) <$> m

instance (Ord x, Eq d, Num d) => Kronecker x d (Sparse x d) where
  delta v = Sparse $ M.singleton v 1

-- >>> g₁ (N pi $ Sparse $ M.singleton "d/dx" 1.0) (N 1 $ Sparse $ M.singleton "d/dy" 1.0)
-- N {primalᴺ = 775.1583323182499, tangentᴺ = Sparse (fromList [("d/dx",2109.5263988877678),("d/dy",5141.877007189466)])}

g₂ :: Num x => x -> x
g₂ x = abs x

newtype Reverse d e = Reverse (d -> e)
  deriving (Semigroup, Monoid) via (d -> e)

instance (AbelianGroup e) => AbelianGroup (Reverse d e) where
  invert (Reverse f) = Reverse $ \d -> invert $ f d

instance VectorSpace d e => VectorSpace d (Reverse d e) where
  μ # (Reverse f) = Reverse $ \d -> μ # f d

instance Kronecker x d e => Kronecker x d (Reverse d e) where
  delta v = Reverse $ \d -> d # delta v
  -- NOTE there should be lots of optimisations available here so long as we know the type, using {-# OVERLAPS #-}

-- >>> let (N pri (Reverse tan)) = g₁ (N pi (Reverse $ \d -> Sparse $ M.singleton "d/dx" d)) (N 1.0 (Reverse $ \d -> Sparse $ M.singleton "d/dy" d))
-- >>> pri
-- >>> tan 1.0
-- 775.1583323182499
-- Sparse (fromList [("d/dx",2109.5263988877678),("d/dy",5141.877007189466)])

newtype LReverse d e = LReverse (d -> e)
  deriving (Semigroup, Monoid) via (d -> e)
  deriving (AbelianGroup, Kronecker x d) via (Reverse d e)
-- * ASSUMES: Linear f => x # f y == f $ x * y

instance VectorSpace d e => VectorSpace d (LReverse d e) where
  μ # (LReverse f) = LReverse $ \d -> f (μ * d) -- use assumption for optimisation

-- >>> let (N pri (LReverse tan)) = g₁ (N pi (LReverse $ \d -> Sparse $ M.singleton "d/dx" d)) (N 1.0 (LReverse $ \d -> Sparse $ M.singleton "d/dy" d))
-- >>> pri
-- >>> tan 1.0
-- 775.1583323182499
-- Sparse (fromList [("d/dx",2109.5263988877673),("d/dy",5141.877007189466)])

newtype Cayley e = Cayley (e -> e)
-- * ASSUMES: Cayley f => x <> f y == f $ x <> y
-- ? guaranteed by x, y : e -> e

instance Semigroup (Cayley e) where
  (Cayley f) <> (Cayley g) = Cayley $ f . g

instance Monoid e => Monoid (Cayley e) where
  mempty = Cayley id

instance (AbelianGroup e) => AbelianGroup (Cayley e) where
  invert (Cayley f) = Cayley $ invert <> f

instance VectorSpace d e => VectorSpace d (Cayley e) where
  μ # (Cayley f) = Cayley $ \e -> e <> (μ # f mempty)
  {- TODO investigate if this can be optimized a la Cayley representations
     previous attempt led to bogus results, @Cayley $ (μ #) <> f@
     @min-nguyen had a lead earlier?
  -}

instance Kronecker x d e => Kronecker x d (Cayley e) where
  delta v = Cayley $ \e -> e <> delta v


-- | see if the `Let` binder used by Tom can be used here... but using the normal Haskell binder!
h₁ :: Num x => x -> x
h₁ x = (x + x) * (x + x)

h₂ :: Num x => x -> x
h₂ x = let !y = x + x
       in y * y
-- * NOTE check via Haskell debugger & `:step`
