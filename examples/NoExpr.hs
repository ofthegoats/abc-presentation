-- |

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}

module NoExpr where

import Data.Semigroup (Sum(..))
import qualified Data.Map as M

-- Duals are only any good for unary functions...

data Dual d = D { primalᴰ :: d , tangentᴰ :: d }
  deriving stock (Show)

instance Num d => Num (Dual d) where
  (D f df) + (D g dg) = D (f + g) (df + dg)
  (D f df) * (D g dg) = D (f * g) (f * dg + g * df)
  negate (D f df) = D (negate f) (negate df)
  fromInteger z = D (fromInteger z) 0

instance Fractional d => Fractional (Dual d) where
  recip (D f df) = D (recip f) (- df / (f ^ 2))
  fromRational q = D (fromRational q) 0

instance Floating d => Floating (Dual d) where
  pi = D pi 0
  exp (D f df) = D (exp f) (df * exp f)
  sin (D f df) = D (sin f) (df * cos f)
  cos (D f df) = D (cos f) (-df * sin f)

f₁ :: Num x => x -> x
f₁ x = x ^ 2 + 2 * x - 2

f₂ :: Fractional x => x -> x
f₂ x = x / (2 * (x^2) + 1)

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

instance (Fractional d, VectorSpace d e) => Fractional (Nagata d e) where
  recip (N f df) = N (recip f) ((- recip (f ^ 2)) # df)
  fromRational q = N (fromRational q) zero

instance (Floating d, VectorSpace d e) => Floating (Nagata d e) where
  pi = N pi zero
  exp (N f df) = N (exp f) (exp f # df)
  log (N f df) = N (log f) (recip f # df)
  sin (N f df) = N (sin f) (cos f # df)
  cos (N f df) = N (cos f) ((- sin f) # df)
  asin (N f df) = N (asin f) ((recip . sqrt) (1 - f^2) # df)
  acos (N f df) = N (acos f) ((negate . recip . sqrt) (1 - f^2) # df)
  atan (N f df) = N (atan f) (recip (1 + f^2) # df)
  sinh (N f df) = N (sinh f) (cosh f # df)
  cosh (N f df) = N (cosh f) (sinh f # df)
  asinh (N f df) = N (asinh f) ((recip . sqrt) (f^2 + 1) # df)
  acosh (N f df) = N (acosh f) ((recip . sqrt) (f^2 - 1) # df)
  atanh (N f df) = N (atanh f) (recip (1 - f^2) # df)

g₁ :: Floating x => x -> x -> x
g₁ x y = sinh x ** exp y + 2

-- >>> g₁ (N pi $ M.singleton "d/dx" 1.0) (N 1 $ M.singleton "d/dy" 1.0)
-- N {primalᴺ = 775.1583323182499, tangentᴺ = fromList [("d/dx",2109.5263988877678),("d/dy",5141.877007189466)]}

instance (Ord x, Num d) => AbelianGroup (M.Map x d) where
  invert m = negate <$> m

instance (Eq d, Ord x, Num d) => VectorSpace d (M.Map x d) where
  μ # m = (*μ) <$> m

g₂ :: Num x => x -> x
g₂ x = abs x

{- TODO reverse mode!
   Haven't fully understood reverse mode yet.
   Think the relevant gradient-representing abelian group must be a function?
   The function must _accumulate_ operations but not execute them.
   Then at the end the user will supply the seed 1.0 to get the full gradient.

   The problems are:
   - what function to supply so that evaluation is awaited for
   - how to prove this is reverse mode? I am _providing_ several arguments after all...

   After implementing reverse mode, it would be nice to have some wrappers around function execution.
   It would also be better for this to all be done at the type-level, and permit stacking properties.
-}
