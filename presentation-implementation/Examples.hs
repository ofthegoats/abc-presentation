{-# LANGUAGE DuplicateRecordFields #-}

module Examples where

import Distributions
import Rejection
import Metropolis
import Sampler

import qualified System.Random.MWC as MWC

import Control.Monad

import Data.List

-- call the examples for an external script to graph the results
main :: IO ()
main = do
  model <- getLine
  case model of
    "weibull" -> approxWeibull 1 1.8 >>= print
    "gk" -> abcGK >>= \pars -> print (fst <$> pars) >> print (snd <$> pars)

{-
import matplotlib.pyplot as plt
x = [...] # copied from Haskell
plt.hist(x, bins=25, range=(0, 5))
plt.show()
-}

-- approximate the Weibull distribution by rejection sampling
-- for convenience, show the result in R
approxWeibull :: Double -> Double -> IO [Double]
approxWeibull λ k = let
  kernel = RSMC
    { prior = uniform 0 5
    , priorDensity = const 2
    , targetDensity = \x -> (k / λ) * (x / λ)**(k-1) * exp (-(x / λ)**k)
    }
  in MWC.createSystemRandom >>= sample (rs 10000 kernel)

abcGK :: IO [(Double, Double)]
abcGK = let
  -- y <- gk 0 1 -0.3 0.8
  y = [5.046804855093987e-3,-0.24542355508624908,4.470242957337151,5.594355128064218,-1.3172331834534312,1.3875308405423779e-2,1.3096337589090183,-0.8812691818201448,0.5080792676781068,-0.47533590009200793,6.146282966275313e-2,-6.681486866555307e-2,-2.41345801762188,-0.5392314985542257,-6.3410131266695e-2,-0.6880851113700569,-6.214189439888811,0.43555449923349765,-0.4232656556229272,-2.7403713126664497,0.9586000546534859,0.5329229039767576,0.5924610155004499,-3.1652035194515324,-0.7334765445511355,-2.686078722672067e-3,-1.2884687412813562,1.8492026534466668,-0.9421064434146015,-0.9561847636837588,-0.16058828480944884,-0.4864881158816869,5.159466819491611,1.1067230032442048,-18.138924349083187,-0.7875114638691381,-2.0096601366939306,-1.0638258203670772,-1.2828677955159815,-0.7298037248415724,0.4214588646870232,-1.8758806400089498,-4.014633279315797,-0.35764930294213165,0.7972301214195806,0.22143874045232806,1.4159765598185179,0.1258699970372251,-11.284722468538341,-2.0662858581682535]
  summarise x = let x' = sort x in (head x' , x' !! 25, last x')
  kernel = RSABC
    { observations = y
    , model = \(g, k) -> replicateM 50 $ gk 0 1 g k
    , prior = (,) <$> uniform (-1.5) 1.5 <*> uniform (-1.5) 1.5
    , distance = \x y -> let
        (x0, x1, x2) = summarise x
        (y0, y1, y2) = summarise y
        in sqrt $ (x0 - y0)**2 + (x1 - y1)**2 + (x2 - y2)**2
    , tolerance = 1.5 }
  in MWC.createSystemRandom >>= sample (rs 30000 kernel)


myProblem :: (Double, Double) -> Sampler Int
myProblem = myProblem' 0 0
  where
    myProblem' :: Double -> Int -> (Double, Double) -> Sampler Int
    myProblem' count n _ | count >= 100 = return n
    myProblem' count n (b, k) = do
      a <- abs <$> gaussian b k
      myProblem' (count + a) (n + 1) (a, k)

abcMyProblem :: IO [(Double, Double)]
abcMyProblem = let
  -- y <- myProblem (2,2)
  y = [35,25,25,17,28,17,16,19,30,28,18,30,34,12,18,19,22,35,21,11,16,34,32,9,15,24,35,32,22,11,11,17,31,33,20,40,29,16,13,20,19,16,22,39,37,22,19,39,23,38,16,21,22,15,33,33,19,32,19,28,18,26,30,19,13,21,21,36,17,23,14,23,13,37,40,14,15,16,15,20,44,12,10,12,33,12,28,19,18,31,21,31,15,27,38,20,35,16,19,42,33,33,13,36,25,27,20,31,29,14,35,22,27,24,26,30,17,18,18,36,22,13,24,16,22,23,13,15,19,20,16,18,18,20,23,15,11,21,28,17,14,15,12,27,16,24,26,28,30,19,41,15,19,22,34,16,23,15,17,27,12,40,21,40,18,22,19,23,24,41,22,31,16,16,14,17,42,16,21,13,16,24,23,31,12,16,15,11,21,31,37,32,12,16,31,14,17,28,13,26,17,15,23,26,22,36,22,37,16,18,14,22,35,23,13,33,17,28,22,35,34,28,12,9,13,37,11,14,26,40,17,24,30,12,19,45,11,20,29,25,10,51,16,42,37,14,23,27,21,25,37,15,10,19,18,35,18,16,34,32,22,20,22,20,23,12,24,22,10,20,16,11,21,12,20,17,22,21,21,20,7,12,14,16,38,19,21,12,18,14,10,15,27,24,21,18,23,18,16,27,32,22,26,11,25,26,11,30,15,29,13,31,42,27,14,48,25,12,19,35,27,32,26,10,24,30,27,14,37,19,29,18,21,26,13,37,15,14,32,16,12,20,23,28,14,45,28,23,31,19,14,24,16,20,17,13,23,20,22,21,15,11,22,12,29,32,22,27,26,25,27,16,16,31,27,26,18,10,38,27,35,16,20,29,38,29,18,18,16,22,39,34,18,26,27,29,18,18,30,23,29,23,13,20,20,18,17,23,35,16,12,20,26,16,21,28,21,10,31,13,43,27,18,25,26,16,32,37,29,28,13,23,22,22,28,25,39,30,33,22,25,22,23,17,29,14,22,19,30,20,31,17,13,27,29,13,18,31,30,11,17,26,22,25,37,36,23,17,10,12,24,15,44,27,15,45,17,11,48,21,36,22,15,24,25,27,16,42,14,38,20,33,10,32,13,14,21,20,28,57]
  summarise x = let x' = sort x in (head x', x' !! 25, last x')
  kernel = RSABC
    { observations = y
    , model = replicateM (length y) . myProblem
    , prior = (,) <$> uniform 0 5 <*> uniform 0 5
    , distance = \x y -> let
        (x0, x1, x2) = summarise x
        (y0, y1, y2) = summarise y
        in sqrt . fromIntegral $ (x0 - y0)^2 + (x1 - y1)^2 + (x2 - y2)^2
    , tolerance = 3 }
  in MWC.createSystemRandom >>= sample (rs 1000 kernel)
