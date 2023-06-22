{- Resources:
   - Forward or Reverse Mode Automatic Differentiation, Tom Shcrijvers et al
-}

{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module ADField where

import qualified Data.Map as M

{- We should prefer to admit a field.
   Most formulae are probably going to require division, negation, etc.
-}

class Field a where
  (#+) :: a -> a -> a -- ^ (#+) should be closed, associative, commutative
  (#*) :: a -> a -> a -- ^ (#*) should be closed, associative, commutative
  negation :: a -> a -- ^ additive inverse
  reciprocal :: a -> a -- ^ multiplicative inverse
  zero :: a -- ^ additive identity
  one :: a -- ^ multiplicative identity

instance Field Double where
  (#+) = (+)
  (#*) = (*)
  negation = negate
  reciprocal = recip
  zero = 0
  one = 1

{- We should use Nagata numbers to store derivatives (TODO solve higher order derivatives).
   The reason for this is that the value and gradient of a function may have different types,
   consider e.g. f : R^3 -> R ; x : R^3 |- f x : R && f' x = (∂f/∂x₁, ∂f/∂x₂, ∂f/∂x₃) : R^3.
-}

data Nagata d e = N { pri :: d , tan :: e }

{- A cofield is like a vector space but the vector-containing set has to admit a field
   This basically just adds multiplication between vectors. Here that will be component-wise.
-}
class (Field d, Field e) => CoField d e where
  (#.) :: d -> e -> e

{- Now we are ready to say that Nagata numbers form a field.
   So long as its components behave nicely, of course.
-}

{- NOTE
   d/dx 1/f(x) = -f'(x) / (f(x) * f(x))
-}

instance CoField d e => Field (Nagata d e) where
  (N f df) #+ (N g dg) = N (f #+ g) (df #+ dg)
  (N f df) #* (N g dg) = N (f #* g) ((f #. dg) #+ (g #. df))
  negation (N f df) = N (negation f) (negation df)
  reciprocal (N f df) = N (reciprocal f) (reciprocal (f #* f) #. negation df)
  zero = N zero zero
  one = N one zero

{- Now we can provide our language, which represents functions of type v -> d

   TODO how can we represent constants? Since we can't get the type of d involved...
-}
data Expr v where
  Var :: v -> Expr v
  Zero :: Expr v
  One :: Expr v
  Negation :: Expr v -> Expr v
  Reciprocal :: Expr v -> Expr v
  Plus :: Expr v -> Expr v -> Expr v
  Times :: Expr v -> Expr v -> Expr v
  deriving (Show)

data Var = X | Y | Z deriving (Show, Eq, Ord, Bounded, Enum)

-- example1 x = 1/x + 1
-- ∂/∂x example1 x = -1/xx
example1 :: Expr Var
example1 = Reciprocal (Var X) `Plus` One

-- example2 x y = x/y + xy
-- ∂/∂x example2 x y = 1/y + y
-- ∂/∂y example2 x y = -x/yy + x
example2 :: Expr Var
example2 = (Var X `Times` Reciprocal (Var Y)) `Plus` (Var X `Times` Var Y)

-- example3 x y = xy + x + 1
-- ∂/∂x example3 x y = y + 1
-- ∂/∂y example3 x y = x
example3 :: Expr Var
example3 = (Negation (Var X) `Times` Var Y) `Plus` Var X `Plus` One

instance Field (Expr v) where
  (#+) = Plus
  (#*) = Times
  negation = Negation
  reciprocal = Reciprocal
  zero = Zero
  one = One

{- And now we should provide some function to evaluate that language
-}

evaluate :: Field d => (v -> d) -> Expr v -> d
evaluate _ Zero = zero
evaluate _ One = one
evaluate f (Var x) = f x
evaluate f (Negation e) = negation $ evaluate f e
evaluate f (Reciprocal e) = reciprocal $ evaluate f e
evaluate f (Plus e1 e2) = (evaluate f e1) #+ (evaluate f e2)
evaluate f (Times e1 e2) = (evaluate f e1) #* (evaluate f e2)

{- One trick from Tom's paper is making a type class for the Kronecker delta.
   This comes in handy later so that we can make different AD implementations which may use different
   tangent types e, whilst defining them all with the following abstract definition.
-}
class CoField d e => Kronecker v d e | e -> d where
  delta :: v -> e

{- And now we can use @evaulate@ together with the Nagata field.
   We just need to provide a way to turn v (as above) into a Nagata number, which we do via an auxiliary function
-}

ad :: Kronecker v d e => (v -> d) -> Expr v -> Nagata d e
ad var expr =
  let gen x = N (var x) (delta x)
  in evaluate gen expr

-- *** EXAMPLE 1, FORWARD MODE with dense representation

newtype Dense v d = Dense (v -> d)

instance Field d => Field (Dense v d) where
  (Dense f) #+ (Dense g) = Dense $ \x -> f x #+ g x
  (Dense f) #* (Dense g) = Dense $ \x -> f x #* g x
  negation (Dense f) = Dense $ \x -> negation $ f x
  reciprocal (Dense f) = Dense $ \x -> reciprocal $ f x
  zero = Dense $ const zero
  one = Dense $ const one

instance Field d => CoField d (Dense v d) where
  μ #. (Dense f) = Dense $ \x -> μ #* f x

instance (Eq v, Field d) => Kronecker v d (Dense v d) where
  delta v = Dense $ \w -> if v == w then one else zero

forwardAD_Dense :: Kronecker v d (Dense v d) => (v -> d) -> Expr v -> Nagata d (Dense v d)
forwardAD_Dense = ad

instance Show d => Show (Nagata d (Dense Var d)) where
  show (N pri (Dense tan)) =
    "pri: " ++ show pri
    ++ " tan: {"
    ++ " d/dx -> " ++ show (tan X)
    ++ "; d/dy -> " ++ show (tan Y)
    ++ "; d/dz -> " ++ show (tan Z)
    ++ " }"

-- >>> forwardAD_Dense ( \case { X -> 3 ; Y -> 2 } ) example2
-- pri: 7.5 tan: { d/dx -> 2.5; d/dy -> 2.25; d/dz -> 0.0 }

{- NOTE
   Now that we have one valid form of AD, we can easily make new forms of AD which we _know_ will work
   via "Kronecker isomorphisms", more detail on which can be found in the paper.

   See that the paper _also_ provides a generic way to make those AD variants, just providing the pair
   of homomorphisms which compose the isomorphism. Although this generic way is not used very much, since
   we usually choose new forms of AD for speed improvements, which we need to implement internally.
-}

class Kronecker v d e => CorrectAD v d e where
  representation :: (Dense v d) -> e
  abstraction :: e -> (Dense v d)

-- *** EXAMPLE 2, FORWARD MODE with sparse representation i.e. Data.Map

newtype Sparse v d = Sparse (M.Map v d)

rep_sparse :: (Ord v, Bounded v, Enum v, Eq d, Field d) => (Dense v d) -> (Sparse v d)
rep_sparse (Dense f) = Sparse $ M.fromList [(x, f x) | x <- [minBound .. maxBound], f x /= zero]

abs_sparse :: (Ord v, Field d) => (Sparse v d) -> (Dense v d)
abs_sparse (Sparse f) = Dense $ \x -> M.findWithDefault zero x f

instance (Ord v, Bounded v, Enum v, Eq d, Field d) => Field (Sparse v d) where
  (Sparse f) #* (Sparse g) = Sparse $ M.unionWith (#*) f g
  (Sparse f) #+ (Sparse g) = Sparse $ M.unionWith (#+) f g
  negation (Sparse f) = Sparse $ M.map negation f
  reciprocal (Sparse f) = Sparse $ M.map reciprocal f
  zero = Sparse $ M.empty
  one = rep_sparse . Dense $ const one

instance (Ord v, Bounded v, Enum v, Eq d, Field d) => CoField d (Sparse v d) where
  μ #. (Sparse f) = Sparse $ M.map (μ #*) f

instance (Ord v, Bounded v, Enum v, Eq d, Field d) => Kronecker v d (Sparse v d) where
  delta x = Sparse $ M.singleton x one

instance (Ord v, Bounded v, Enum v, Eq d, Field d) => CorrectAD v d (Sparse v d) where
  representation = rep_sparse
  abstraction :: (Ord v, Bounded v, Enum v, Eq d, Field d) => Sparse v d -> Dense v d
  abstraction = abs_sparse

instance Show d => Show (Nagata d (Sparse Var d)) where
  show (N pri (Sparse tan)) = "pri: " ++ show pri ++ " tan: " ++ show tan

forwardAD_Sparse :: (Ord v, Bounded v, Enum v, Eq d, Field d) => (v -> d) -> Expr v -> Nagata d (Sparse v d)
forwardAD_Sparse = ad

-- >>> forwardAD_Sparse ( \case { X -> 3 ; Y -> 2 } ) example2
-- pri: 7.5 tan: fromList [(X,2.5),(Y,2.25)]

-- *** EXAMPLE 3, REVERSE MODE with sparse representation

{- Without getting into details, we can enforce reverse mode ad by giving the tangent type as
   some form of map d -> e.

   We evaluate forwards and basically just accumulate operations for the tangent type. Then we supply
   a seed (always the same, one) to collapse it and we get the tangent.
-}

newtype Reverse d e = Reverse (d -> e)

-- more generic reverse, for any tangent type e
rep_reverse :: CoField d e => e -> (Reverse d e)
rep_reverse e = Reverse $ \x -> x #. e

abs_reverse :: CoField d e => (Reverse d e) -> e
abs_reverse (Reverse f) = f one

instance Field e => Field (Reverse d e) where
  (Reverse f) #+ (Reverse g) = Reverse $ \x -> f x #+ g x
  (Reverse f) #* (Reverse g) = Reverse $ \x -> f x #* g x
  negation (Reverse f) = Reverse $ \x -> negation $ f x
  reciprocal (Reverse f) = Reverse $ \x -> reciprocal $ f x
  zero = Reverse $ const zero
  one = Reverse $ const one

instance CoField d e => CoField d (Reverse d e) where
  μ #. (Reverse f) = Reverse $ \x -> μ #. f x


instance Kronecker v d e => Kronecker v d (Reverse d e) where
  delta :: CoField d e => v -> Reverse d e
  delta v = Reverse $ \d -> d #. delta v

instance CorrectAD v d e => CorrectAD  v d (Reverse d e) where
  representation = rep_reverse . representation
  abstraction = abstraction . abs_reverse

reverseAD_Dense :: (Eq v, Field d) => (v -> d) -> Expr v -> Nagata d (Reverse d (Dense v d))
reverseAD_Dense = ad

instance (Field d, Show d) => Show (Nagata d (Reverse d (Dense Var d))) where
  show (N pri tan) =
    let (Dense tan') = abs_reverse tan
    in "pri: " ++ show pri ++
       " tan: " ++ "{ " ++
       "X -> " ++ show (tan' X) ++
       "; Y -> " ++ show (tan' Y) ++
       "; Z -> " ++ show (tan' Z) ++
       " }"

-- >>> reverseAD_Dense ( \case { X -> 3 ; Y -> 2 } ) example2
-- pri: 7.5 tan: { X -> 2.5; Y -> 2.25; Z -> 0.0 }

reverseAD_Sparse :: (Ord v, Bounded v, Eq v, Enum v, Eq d, Field d) => (v -> d) -> Expr v -> Nagata d (Reverse d (Sparse v d))
reverseAD_Sparse = ad

instance (Field d, Eq d, Show d) => Show (Nagata d (Reverse d (Sparse Var d))) where
  show (N pri tan) =
    let (Sparse tan') = abs_reverse tan
    in "pri: " ++ show pri ++ " tan: " ++ show tan'

-- >>> reverseAD_Sparse ( \case { X -> 3 ; Y -> 2 } ) example2
-- pri: 7.5 tan: fromList [(X,2.5),(Y,2.25)]

{- next steps:
   - improve reverse-mode runtime via methods identified in paper
     - modular homomorphisms (~linear functions) to improve runtime of (#.)
     - cayley functions to improve runtime of (#+)
     - use state-thread array
   - support higher order derivatives
     - by default?
   - an example of AD for gradient descent
-}
