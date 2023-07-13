module MLE where

import AD
import qualified Data.Map as M

import Probability

logpGeom :: Floating a => a -> a -> a
logpGeom p x = (x-1) * log (1-p) + log p

-- mlep := argmin [ 0 < p < 1 ] -( Σᵢ₌₁ⁿ logpGeom p x )
-- goal: use AD to get p

mleGeom :: [Nagata Double (Sparse Char Double)] -> Int -> Double -> Double
mleGeom xs n est_p = let
  (N pri (Sparse tan)) = sum [ - logpGeom (raiseᴺ est_p 'p') x | x <- xs ]
  next_est_p = est_p - 0.0001 * tan M.! 'p'
  in if n == 0 then next_est_p else mleGeom xs (n-1) next_est_p

-- >>> import qualified System.Random.MWC as MWC
-- >>> import Control.Monad
-- >>> g <- MWC.createSystemRandom
-- >>> xs <- replicateM 100 $ random (geometric 0.2) g
-- >>> fromIntegral (sum xs) / 100
-- >>> let p = mleGeom (map fromIntegral xs) 1000 0.5
-- >>> p
-- >>> 1/p
-- 4.08
-- 0.24509803921568635
-- 4.079999999999999

-- NOTE CRITICAL! the step (above @0.001@ might be too small, depending on what p is in the actual distribution)
-- perhaps γ should be provided also by the user of the function?
-- with the note that if the results are divergent, a smaller γ should be chosen
-- there should also be some algorithms to estimate what value of γ should be used... but these are hard to understand
