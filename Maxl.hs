module Maxl (maximizeLikelihood, reportMaxResult, reportTrace) where

import RareAlleleHistogram (RareAlleleHistogram)
import Logl (computeLikelihood)
import Numeric.LinearAlgebra.Data (Matrix, format, toRows, fromRows, fromList)
import Numeric.GSL.Minimization (minimizeD, MinimizeMethodD(..))
import Core (ModelSpec(..), ModelEvent(..), EventType(..), update)
import Data.List (intercalate)

maximizeLikelihood :: ModelSpec -> RareAlleleHistogram -> Int -> Either String ([Double], Matrix Double) 
maximizeLikelihood modelSpec hist maxCycles =
    case computeLikelihood modelSpec hist of
        Left err -> Left $ "Error in initial Model: " ++ err
        Right _ ->
            let (initParams, scalingFactors) = makeInitialParams modelSpec
                func = minFunc modelSpec hist scalingFactors
                grad = minFuncGradient modelSpec hist scalingFactors
                (minResult, trace) = minimizeD VectorBFGS2 1.0e-8 maxCycles 0.01 0.1 func grad initParams
                minResult' = zipWith (*) minResult scalingFactors
                trace' = scaleTraceMatrix scalingFactors trace
            in  Right (minResult', trace')
  where
    scaleTraceMatrix factors t = 
        let rows = toRows t
            factors' = fromList $ [1.0, 1.0] ++ factors
        in  fromRows [row * factors' | row <- rows]

reportMaxResult :: ModelSpec -> [Double] -> IO ()
reportMaxResult modelSpec result =
    putStr $ unlines $ zipWith (\p v -> p ++ "\t" ++ show v) (makeParamNames modelSpec) result 

reportTrace :: ModelSpec -> Matrix Double -> FilePath -> IO ()
reportTrace modelSpec trace path = do
    let header = intercalate "\t" $ ["Nr", "-Log-Likelihood"] ++ makeParamNames modelSpec
    writeFile path $ header ++ "\n" ++ format "\t" show trace 

makeParamNames :: ModelSpec -> [String]
makeParamNames modelSpec =
    let events = mEvents modelSpec
    in ["P_" ++ show k | ModelEvent 0.0 (SetPopSize k _) <- events] ++
       ["t_(" ++ show k ++ "<-" ++ show l ++ ")" | ModelEvent _ (Join k l) <- events] ++
       ["P_(" ++ show k ++ "<-" ++ show l ++ ")" | ModelEvent _ (Join k l) <- events]

minFunc :: ModelSpec -> RareAlleleHistogram -> [Double] -> [Double] -> Double
minFunc initialModelSpec hist scalingFactors params =
    let newModelSpec = paramsToModelSpec initialModelSpec scalingFactors params
    in  if validModel newModelSpec then
            let result = computeLikelihood newModelSpec hist
            in  case result of
                Left _ -> penalty
                Right val -> -val
        else penalty

validModel :: ModelSpec -> Bool
validModel (ModelSpec _ _ events) = 
    all (\p -> p >= 0.001 && p <= 100.0) [p | ModelEvent _ (SetPopSize _ p) <- events]

minFuncGradient :: ModelSpec -> RareAlleleHistogram -> [Double] -> [Double] -> [Double]
minFuncGradient modelSpec hist scalingFactors params =
    let f = minFunc modelSpec hist scalingFactors params
        newPlist = [makeNewStep params i 1.0e-8 | i <- [0 .. (length params - 1)]]
        valList = map (minFunc modelSpec hist scalingFactors) newPlist
    in  [(v - f)/1.0e-8 | v <- valList]
  where
    makeNewStep params' i d = let p = params'!!i in update i (p + d) params'

penalty :: Double
penalty = 1.0e20

paramsToModelSpec :: ModelSpec -> [Double] -> [Double] -> ModelSpec
paramsToModelSpec m@(ModelSpec _ _ events) scalingFactors params =
    let initialPopSizeEvents = [e | e@(ModelEvent 0.0 (SetPopSize _ _)) <- events]
        nrPevents = length initialPopSizeEvents
        joins = [e | e@(ModelEvent _ (Join _ _)) <- events]
        nrJoins = length joins
        newInitialPopSizeEvents = zipWith3 makeNewInitialPopSizeEvents initialPopSizeEvents params scalingFactors
        newJoins = zipWith3 makeNewJoins joins (drop nrPevents params) (drop nrPevents scalingFactors)
        newPopSizeChanges = zipWith3 makeNewPopSizeChanges newJoins (drop (nrPevents + nrJoins) params) (drop (nrPevents + nrJoins) scalingFactors)
        newEvents = newInitialPopSizeEvents ++ newJoins ++ newPopSizeChanges
    in  m {mEvents = newEvents}
  where
    makeNewInitialPopSizeEvents (ModelEvent t (SetPopSize k _)) p' s = ModelEvent t (SetPopSize k (p' * s))
    makeNewInitialPopSizeEvents _ _ _ = undefined
    makeNewJoins (ModelEvent _ (Join k l)) t' s = ModelEvent (t' * s) (Join k l)
    makeNewJoins _ _ _ = undefined
    makeNewPopSizeChanges (ModelEvent t (Join k _)) p s = ModelEvent t (SetPopSize k (p * s))
    makeNewPopSizeChanges _ _ _ = undefined

makeInitialParams :: ModelSpec -> ([Double], [Double])
makeInitialParams (ModelSpec _ _ events) =
    let initialPopSizes = [p | ModelEvent 0.0 (SetPopSize _ p) <- events]
        joinTimes = [t | ModelEvent t (Join _ _) <- events]
        joinPopSizes = replicate (length joinTimes) 1.0
        scalingFactors = initialPopSizes ++ joinTimes ++ joinPopSizes
    in  (replicate (length scalingFactors) 1.0, scalingFactors)
