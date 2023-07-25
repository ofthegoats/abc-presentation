-- | Kernel Density Estimation --- estimate a probability density function

module KDE where

import PPL

kde :: [Double] -> (Double -> Double) -> Double -> Double -> Double
kde ys kernel bandwidth x =
  let n = fromIntegral $ length ys
      sumTerm = (sum . map (\y -> kernel $ (x - y) / bandwidth)) ys
  in (1 / n * bandwidth) * sumTerm

gaussianKernel :: Double -> Double
gaussianKernel x = (1 / sqrt (2 * pi)) * exp ((-1/2) * x**2)
