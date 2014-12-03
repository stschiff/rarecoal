module Mcmc (runMcmc, reportMcmcResult, reportMcmcTrace, readMaxResult) where

import Numeric.LinearAlgebra.Data (Matrix, format, toRows, fromRows, fromList)
import RareAlleleHistogram (RareAlleleHistogram(..))
import ModelSpec (ModelTemplate(..))
import Numeric.LinearAlgebra.Data (Matrix, format, toRows, fromRows, fromList)

runMcmc :: ModelTemplate -> [Double] -> RareAlleleHistogram -> Int -> Either String ([[Double]], Matrix Double)
runMcmc modelTemplate params hist cycles = undefined

reportMcmcResult :: ModelTemplate -> [[Double]] -> IO ()
reportMcmcResult modelTemplate mcmcResult = undefined

reportMcmcTrace :: ModelTemplate -> Matrix Double -> FilePath -> IO ()
reportMcmcTrace modelTemplate trace path = undefined

readMaxResult :: FilePath -> IO [Double]
readMaxResult maxResultPath = do
    c <- readFile maxResultPath
    return [read (w!!1) | w <- map words . lines $ c] 
