module Maxl (maximizeLikelihood, reportMaxResult, reportTrace) where

import RareAlleleHistogram (RareAlleleHistogram)
import Logl (computeLikelihood)
import Numeric.LinearAlgebra.Data (Matrix, format)
import Numeric.GSL.Minimization (minimize, MinimizeMethod(..))
import Core (ModelSpec(..), ModelEvent(..), EventType(..))
import Data.List (intercalate)

maximizeLikelihood :: ModelSpec -> RareAlleleHistogram -> Int -> Either String ([Double], Matrix Double) 
maximizeLikelihood modelSpec hist maxCycles =
    case computeLikelihood modelSpec hist of
        Left err -> Left $ "Error in initial Model: " ++ err
        Right _ ->
            let initParams = makeInitialParams modelSpec
                stepSizes = map (/100.0) initParams
            in  Right $ minimize NMSimplex2 1.0e-4 maxCycles stepSizes (minFunc modelSpec hist) initParams

reportMaxResult :: ModelSpec -> [Double] -> IO ()
reportMaxResult modelSpec result = do
    let model = paramsToModelSpec modelSpec result
    putStr $ unlines $ map show (mEvents model)

reportTrace :: ModelSpec -> Matrix Double -> FilePath -> IO ()
reportTrace modelSpec trace path = do
    let header = intercalate "\t" $ ["Nr", "-Log-Likelihood", "SimplexSize"] ++ makeParamNames modelSpec
    writeFile path $ header ++ "\n" ++ format "\t" show trace 

makeParamNames :: ModelSpec -> [String]
makeParamNames modelSpec =
    let events = mEvents modelSpec
    in ["P_" ++ show k | ModelEvent 0.0 (SetPopSize k _) <- events] ++
       ["t_(" ++ show k ++ "<-" ++ show l ++ ")" | ModelEvent _ (Join k l) <- events] ++
       ["P_(" ++ show k ++ "<-" ++ show l ++ ")" | ModelEvent _ (Join k l) <- events]

minFunc :: ModelSpec -> RareAlleleHistogram -> [Double] -> Double
minFunc initialModelSpec hist params =
    let newModelSpec = paramsToModelSpec initialModelSpec params
        result = computeLikelihood newModelSpec hist
    in  case result of
        Left _ -> penalty
        Right val -> -val

penalty :: Double
penalty = 1.0e20

paramsToModelSpec :: ModelSpec -> [Double] -> ModelSpec
paramsToModelSpec m@(ModelSpec _ _ events) params =
    let initialPopSizeEvents = [e | e@(ModelEvent 0.0 (SetPopSize _ _)) <- events]
        nrPevents = length initialPopSizeEvents
        joins = [e | e@(ModelEvent _ (Join _ _)) <- events]
        nrJoins = length joins
        newInitialPopSizeEvents = zipWith makeNewInitialPopSizeEvents initialPopSizeEvents params
        newJoins = zipWith makeNewJoins joins (drop nrPevents params)
        newPopSizeChanges = zipWith makeNewPopSizeChanges newJoins (drop (nrPevents + nrJoins) params)
        newEvents = newInitialPopSizeEvents ++ newJoins ++ newPopSizeChanges
    in  m {mEvents = newEvents}
  where
    makeNewInitialPopSizeEvents (ModelEvent t (SetPopSize k _)) p' = ModelEvent t (SetPopSize k p')
    makeNewInitialPopSizeEvents _ _ = undefined
    makeNewJoins (ModelEvent _ (Join k l)) t' = ModelEvent t' (Join k l)
    makeNewJoins _ _ = undefined
    makeNewPopSizeChanges (ModelEvent t (Join k _)) p = ModelEvent t (SetPopSize k p)
    makeNewPopSizeChanges _ _ = undefined

makeInitialParams :: ModelSpec -> [Double]
makeInitialParams (ModelSpec _ _ events) =
    let initialPopSizes = [p | ModelEvent 0.0 (SetPopSize _ p) <- events]
        joinTimes = [t | ModelEvent t (Join _ _) <- events]
        joinPopSizes = replicate (length joinTimes) 1.0
    in  initialPopSizes ++ joinTimes ++ joinPopSizes
