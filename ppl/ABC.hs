-- | wrapper functions around RS and MH

module ABC where

import PPL
import RS
import MH

abcRejection :: Int -> ω -> (ω -> ω -> Double) -> Double -> Dist θ -> (θ -> Dist ω) -> Dist [θ]
abcRejection n obs dist tol pri mod = rs n $ RSPars obs dist tol pri mod

abcMCMC :: Int -> ω -> (ω -> ω -> Double) -> [Double] -> Dist θ -> (θ -> Double) -> (θ -> Dist θ) -> (θ -> θ -> Double) -> (θ -> Dist ω) -> Maybe θ -> Dist [θ]
abcMCMC n obs dist tols pri priD pro proD mod θ = mh n $ MHPars obs dist tols pri priD pro proD mod θ
