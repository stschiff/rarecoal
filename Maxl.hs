module Maxl (maximizeLikelihood, reportMaxResult, reportTrace, minFunc, validateModel) where

import RareAlleleHistogram (RareAlleleHistogram)
import Logl (computeLikelihood)
import Numeric.LinearAlgebra.Data (toRows, toList)
import Numeric.GSL.Minimization (minimize, MinimizeMethod(..))
import ModelSpec (ModelSpec(..), ModelEvent(..), EventType(..), ModelTemplate(..), instantiateModel)
import Data.List (intercalate)
import Control.Monad (when)
import qualified Data.Vector.Unboxed as V

maximizeLikelihood :: ModelTemplate -> V.Vector Double -> RareAlleleHistogram -> Int -> Either String (V.Vector Double, [V.Vector Double]) 
maximizeLikelihood modelTemplate initialParams hist maxCycles = do
    let initialModel = instantiateModel modelTemplate initialParams
    validateModel initialModel
    _ <- computeLikelihood initialModel hist
    let k = V.length initialParams
        initialParams' = replicate k 1.0
        scalingFactors = initialParams                
        minFunc' = minFunc modelTemplate hist . unscaleParams scalingFactors . V.fromList
        (minResult, trace) = minimize NMSimplex2 1.0e-8 maxCycles [0.01 | _ <- [0..k-1]] minFunc' initialParams'
        minResult' = unscaleParams scalingFactors (V.fromList minResult)
        trace' = scaleTraceMatrix scalingFactors trace
    return (minResult', trace')
  where
    scaleTraceMatrix factors t = 
        let rows = toRows t
            factors' = V.fromList $ [1.0, 1.0, 1.0] ++ V.toList factors
        in  [V.zipWith (*) (V.fromList . toList $ row) factors' | row <- rows]
    unscaleParams = V.zipWith (*)

reportMaxResult :: ModelTemplate -> V.Vector Double -> IO ()
reportMaxResult modelTemplate result =
    putStr $ unlines $ zipWith (\p v -> p ++ "\t" ++ show v) (mtParams modelTemplate) (V.toList result)

reportTrace :: ModelTemplate -> [V.Vector Double] -> FilePath -> IO ()
reportTrace modelTemplate trace path = do
    let header = intercalate "\t" $ ["Nr", "-Log-Likelihood", "Simplex size"] ++ mtParams modelTemplate
        body = unlines [intercalate "\t" [show val | val <- V.toList row] | row <- trace]
    writeFile path $ header ++ "\n" ++ body

minFunc :: ModelTemplate -> RareAlleleHistogram -> V.Vector Double -> Double
minFunc modelTemplate hist params =
    let modelSpec = instantiateModel modelTemplate params
    in  case validateModel modelSpec of
        Right _ ->
            let result = computeLikelihood modelSpec hist
            in  case result of
                Left _ -> penalty
                Right val -> -val
        Left _ -> penalty

validateModel :: ModelSpec -> Either String ()
validateModel (ModelSpec _ _ events) = 
    when (any (\p -> p < 0.001 || p > 100.0) [p | ModelEvent _ (SetPopSize _ p) <- events]) $ Left "illegal population sizes"

-- minFuncGradient :: ModelTemplate -> RareAlleleHistogram -> [Double] -> [Double] -> [Double]
-- minFuncGradient modelTemplate hist scalingFactors params =
--     let f = minFunc modelTemplate hist scalingFactors params
--         newPlist = [makeNewStep params i 1.0e-8 | i <- [0 .. (length params - 1)]]
--         valList = map (minFunc modelTemplate hist scalingFactors) newPlist
--     in  [(v - f)/1.0e-8 | v <- valList]
--   where
--     makeNewStep params' i d = let p = params'!!i in update i (p + d) params'

penalty :: Double
penalty = 1.0e20


