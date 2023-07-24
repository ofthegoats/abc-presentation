{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module PPL where

import qualified System.Random.MWC as MWC

import Control.Monad
import Control.Monad.Reader

runDist :: MonadIO m => Dist ω -> MWC.GenIO -> m ω
runDist μ g = liftIO $ runReaderT (dist μ) g

type Prob = Double

newtype Dist ω = Dist { dist :: ReaderT MWC.GenIO IO ω }
  deriving (Functor, Applicative, Monad)

type Uniform = Dist Double
uniform01 :: Uniform
uniform01 = Dist $ ask >>= MWC.uniform

uniform' :: Double -> Double -> Uniform
uniform' a b = Dist $ do
  gen <- ask
  x <- runDist uniform01 gen
  return $ (b-a) * x + a

type Bernoulli = Dist Bool
bernoulli' :: Prob -> Bernoulli
bernoulli' p = (<=p) <$> uniform01

type Binomial = Dist Int
binomial' :: Int -> Prob -> Binomial
binomial' n p = length . filter id <$> replicateM n (bernoulli' p)

type Dirac ω = Dist ω
dirac' :: ω -> Dirac ω
dirac' x = return x

type Normal = Dist Double
normal' :: Double -> Double -> Normal
normal' μ σ² =
  (\u1 u2 -> μ + σ² * sqrt (-2 * log u1) * cos (2 * pi * u2))
  <$> uniform01 <*> uniform01

-- | approximate bayesian computation as described in Marjoram et al. 2003

data AbcPar θ ω s =
  ABC { yObs :: ω
      , summary :: ω -> s
      , prior :: Dist θ
      , priorDensity :: θ -> Double
      , proposal :: θ -> Dist θ
      , proposalDensity :: θ -> θ -> Double
      , model :: θ -> Dist ω
      , distance :: s -> s -> Double
      , tolerance :: Double
      , lastPar :: Maybe θ
      }

abcMarjoram :: Int -> AbcPar θ ω s -> Dist [θ]
abcMarjoram 0 _ = return []
abcMarjoram n par@ABC{..} = let sObs = summary yObs in case lastPar of
  Nothing -> do
    θ <- prior
    yGen <- model θ
    let sGen = summary yGen
    if sObs `distance` sGen <= tolerance
      then (θ:) <$> abcMarjoram (n-1) par{ lastPar = Just θ }
      else abcMarjoram n par
  Just last -> do
    θ' <- proposal last
    yGen <- model θ'
    let sGen = summary yGen
    accept <- bernoulli' $ min 1 $ (priorDensity θ' * proposalDensity last θ') / (priorDensity last * proposalDensity θ' last)
    if sObs `distance` sGen <= tolerance && accept
      then (last:) <$> abcMarjoram (n-1) par{ lastPar = Just θ' } -- HERE!
      else (last:) <$> abcMarjoram (n-1) par

-- convenience constructor for rejection sampling
abcRejectionPar :: ω -> (ω -> s) -> Dist θ -> (θ -> Double) -> (θ -> Dist ω) -> (s -> s -> Double) -> Double -> AbcPar θ ω s
abcRejectionPar yObs summary prior priorDensity model distance tolerance =
  ABC { yObs = yObs
      , summary = summary
      , prior = prior
      , priorDensity = priorDensity
      , proposal = const prior
      , proposalDensity = const priorDensity
      , model = model
      , distance = distance
      , tolerance = tolerance
      , lastPar = Nothing
      }

abcMCMCPar :: ω -> (ω -> s) -> Dist θ -> (θ -> Double) -> (θ -> Dist θ) -> (θ -> θ -> Double) -> (θ -> Dist ω) -> (s -> s -> Double) -> Double -> AbcPar θ ω s
abcMCMCPar yObs summary prior priorDensity proposal proposalDensity model distance tolerance =
  ABC { yObs = yObs
      , summary = summary
      , prior = prior
      , priorDensity = priorDensity
      , proposal = proposal
      , proposalDensity = proposalDensity
      , model = model
      , distance = distance
      , tolerance = tolerance
      , lastPar = Nothing
      }
