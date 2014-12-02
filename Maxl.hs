module Maxl (maximizeLikelihood, reportMaxResult, reportTrace) where

import RareAlleleHistogram (RareAlleleHistogram)
import Logl (computeLikelihood)
import Numeric.LinearAlgebra.Data (Matrix, format, toRows, fromRows, fromList)
import Numeric.GSL.Minimization (minimizeD, MinimizeMethodD(..))
import Core (update)
import ModelSpec (ModelSpec(..), ModelEvent(..), EventType(..), ModelTemplate(..), instantiateModel)
import Data.List (intercalate)
import Control.Monad (when)

maximizeLikelihood :: ModelTemplate -> [Double] -> RareAlleleHistogram -> Int -> Either String ([Double], Matrix Double) 
maximizeLikelihood modelTemplate initialParams hist maxCycles = do
    let initialModel = instantiateModel modelTemplate initialParams
    validateModel initialModel
    _<- computeLikelihood initialModel hist
    let initialParams' = replicate (length initialParams) 1.0
        scalingFactors = initialParams                
        func = minFunc modelTemplate hist scalingFactors
        grad = minFuncGradient modelTemplate hist scalingFactors
        (minResult, trace) = minimizeD VectorBFGS2 1.0e-8 maxCycles 0.01 0.1 func grad initialParams'
        minResult' = zipWith (*) minResult scalingFactors
        trace' = scaleTraceMatrix scalingFactors trace
    return (minResult', trace')
  where
    scaleTraceMatrix factors t = 
        let rows = toRows t
            factors' = fromList $ [1.0, 1.0] ++ factors
        in  fromRows [row * factors' | row <- rows]

reportMaxResult :: ModelTemplate -> [Double] -> IO ()
reportMaxResult modelTemplate result =
    putStr $ unlines $ zipWith (\p v -> p ++ "\t" ++ show v) (mtParams modelTemplate) result 

reportTrace :: ModelTemplate -> Matrix Double -> FilePath -> IO ()
reportTrace modelTemplate trace path = do
    let header = intercalate "\t" $ ["Nr", "-Log-Likelihood"] ++ mtParams modelTemplate
    writeFile path $ header ++ "\n" ++ format "\t" show trace 

minFunc :: ModelTemplate -> RareAlleleHistogram -> [Double] -> [Double] -> Double
minFunc modelTemplate hist scalingFactors params =
    let newModelSpec = paramsToModelSpec modelTemplate scalingFactors params
    in  case validateModel newModelSpec of
        Right _ ->
            let result = computeLikelihood newModelSpec hist
            in  case result of
                Left _ -> penalty
                Right val -> -val
        Left _ -> penalty

validateModel :: ModelSpec -> Either String ()
validateModel (ModelSpec _ _ events) = 
    when (any (\p -> p < 0.001 || p > 100.0) [p | ModelEvent _ (SetPopSize _ p) <- events]) $ Left "illegal population sizes"

minFuncGradient :: ModelTemplate -> RareAlleleHistogram -> [Double] -> [Double] -> [Double]
minFuncGradient modelTemplate hist scalingFactors params =
    let f = minFunc modelTemplate hist scalingFactors params
        newPlist = [makeNewStep params i 1.0e-8 | i <- [0 .. (length params - 1)]]
        valList = map (minFunc modelTemplate hist scalingFactors) newPlist
    in  [(v - f)/1.0e-8 | v <- valList]
  where
    makeNewStep params' i d = let p = params'!!i in update i (p + d) params'

penalty :: Double
penalty = 1.0e20

paramsToModelSpec :: ModelTemplate -> [Double] -> [Double] -> ModelSpec
paramsToModelSpec modelTemplate scalingFactors params =
    let scaledParams = zipWith (*) params scalingFactors
    in  instantiateModel modelTemplate scaledParams

