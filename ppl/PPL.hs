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
      , tolerances :: [Double]
      , lastPar :: Maybe θ
      }

abcMarjoram :: Int -> AbcPar θ ω s -> Dist [θ]
abcMarjoram 0 _ = return []
abcMarjoram n par@ABC{ tolerances=(tolerance : tolerances'), .. } = let
  sObs = summary yObs
  in case lastPar of
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
      then (last:) <$> abcMarjoram (n-1) par{ lastPar = Just θ', tolerances = tolerances' }
      else (last:) <$> abcMarjoram (n-1) par

-- convenience constructor for rejection sampling
abcRejectionPar :: ω -> (ω -> s) -> Dist θ -> (θ -> Double) -> (θ -> Dist ω) -> (s -> s -> Double) -> [Double] -> AbcPar θ ω s
abcRejectionPar yObs summary prior priorDensity model distance tolerances =
  ABC { yObs = yObs
      , summary = summary
      , prior = prior
      , priorDensity = priorDensity
      , proposal = const prior
      , proposalDensity = const priorDensity
      , model = model
      , distance = distance
      , tolerances = tolerances
      , lastPar = Nothing
      }

-- example of rejection sampling for a [Binomial 10 0.4] distribution
binomialExample :: IO ()
binomialExample =
  let obs = [2,1,2,4,5,4,3,2,6,2,3,4,8,4,4,6,5,4,3,5,3,4,1,2,5,5,6,4,4,3] -- <<< replicateM 30 $ (bin 10 .4)
      summary ω = (fromIntegral . sum) ω / (fromIntegral . length) ω
      prior = uniform01
      priorDensity = const 1
      model p = replicateM 30 $ binomial' 10 p
      distance s s' = (s - s')**2
      tolerances = repeat 1.0
      pars = abcRejectionPar obs summary prior priorDensity model distance tolerances
      estimate = abcMarjoram 1000 pars
  in do
    gen <- MWC.createSystemRandom
    θs <- runDist estimate gen
    print $ sum θs / (fromIntegral . length) θs

abcMCMCPar :: ω -> (ω -> s) -> Dist θ -> (θ -> Double) -> (θ -> Dist θ) -> (θ -> θ -> Double) -> (θ -> Dist ω) -> (s -> s -> Double) -> [Double] -> AbcPar θ ω s
abcMCMCPar yObs summary prior priorDensity proposal proposalDensity model distance tolerances =
  ABC { yObs = yObs
      , summary = summary
      , prior = prior
      , priorDensity = priorDensity
      , proposal = proposal
      , proposalDensity = proposalDensity
      , model = model
      , distance = distance
      , tolerances = tolerances
      , lastPar = Nothing
      }

-- ditto, using MCMC sampling
binomialExample' :: IO ()
binomialExample' =
  let obs = [2,1,2,4,5,4,3,2,6,2,3,4,8,4,4,6,5,4,3,5,3,4,1,2,5,5,6,4,4,3] -- <<< replicateM 30 $ (bin 10 .4)
      summary ω = (fromIntegral . sum) ω / (fromIntegral . length) ω
      prior = uniform01
      priorDensity = const 1
      proposal θ = normal' θ 1
      proposalDensity x μ = let σ² = 1 in exp $ (-1/(2*σ²)) * (x - μ)**2
      model p = replicateM 30 $ binomial' 10 p
      distance s s' = (s - s')**2
      tolerances = replicate 500 1.0 <> replicate 100 0.9 <> replicate 100 0.8 <> replicate 100 0.7 <> replicate 100 0.6 <> replicate 100 0.5
      pars = abcMCMCPar obs summary prior priorDensity proposal proposalDensity model distance tolerances
      estimate = abcMarjoram 1000 pars
  in do
    gen <- MWC.createSystemRandom
    θs <- runDist estimate gen
    let θs' = drop 970 θs
    putStrLn $ (show . head) θs ++ " ... " ++ show θs'
    print $ sum θs' / (fromIntegral . length) θs'
