-- | toy PPL

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Probability where

import AD
import qualified Data.Map as M

{-
if we use the probability spaces proposed on nlab we will not have to think about sigma algebras
this makes sense... we don't really use them besides to have an event space
but we don't need a event space, do we really?
-}

type a |-> b = a -> b
-- * restriction, representing specifically a MEASURABLE MAP
-- this should actually be fairly permissive?

instance Num b => Num (a |-> b) where
  x + y = \ω -> x ω + y ω
  x * y = \ω -> x ω * y ω
  negate x = negate . x
  abs x = abs . x
  fromInteger x = const $ fromInteger x

instance Fractional b => Fractional (a |-> b) where
  recip x = recip . x
  fromRational x = const $ fromRational x

instance Floating b => Floating (a |-> b) where
  exp x = exp . x
  log x = log . x

class PSpace x d where
  μ :: d -> x -> Double

data UniformD = UniformD Integer

instance PSpace Integer UniformD where
  μ (UniformD n) x
    | x <= 0 || x > n = 0
    | otherwise = 1 / (fromIntegral n)

data UniformC = UniformC Double Double

instance PSpace Double UniformC where
  μ (UniformC a b) x
    | a <= x && x <= b = 1 / (b - a)
    | otherwise = error ""

{-
let's make an example now
suppose that we roll a 6-sided die getting a result x
then we eat |3-x| pierogi
so we have a random variabe x : int -> int ; ω |-> abs (3 - ω)
we assume that the die is evenly weighted, ie ω <- U[1,6]
-}

d6 :: UniformD
d6 = UniformD 6

x :: Integer |-> Integer
x ω = abs (3 - ω)

squash :: (Ord a) => (b -> b -> b) -> [(a,b)] -> [(a,b)]
squash f = M.toList . M.fromListWith f

-- >>> squash (+) $ map (\ω -> (x ω, μ d6 ω)) [1 .. 6]
-- [(0,0.16666666666666666),(1,0.3333333333333333),(2,0.3333333333333333),(3,0.16666666666666666)]

{-
so we can see the distribution for how many pierogi we eat

what would be a comfortable way to wrap this, or to say that "the outcomes I am giving you are distributed so and so"
literally a probability space.
but also, what we have above is a probability space (we specify the outcomes by type and provide a distribution)

here let's just use a pair since it's a probability space that's a value...

(I may refactor the naming later)
-}

{-
TODO how to make joint distributions? (think I will need applicatives... at least in the blog example, perhaps I can make something better, but nonetheless it would be preferable)
-}
