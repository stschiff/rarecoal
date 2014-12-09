module Maxl (minFunc, validateModel, runMaxl, MaxlOpt(..)) where

import RareAlleleHistogram (RareAlleleHistogram, loadHistogram)
import Logl (computeLikelihood)
import Numeric.LinearAlgebra.Data (toRows, toList)
import Numeric.GSL.Minimization (minimize, MinimizeMethod(..))
import ModelTemplate (ModelTemplate(..), instantiateModel, readModelTemplate)
import Data.List (sortBy, intercalate)
import Control.Monad (when)
import Data.Int (Int64)
import Core (defaultTimes,  ModelSpec(..), ModelEvent(..), EventType(..))
import qualified Data.Vector.Unboxed as V
import Control.Error (Script, scriptIO)
import Control.Monad.Trans.Either (hoistEither)

data MaxlOpt = MaxlOpt {
   maTheta :: Double,
   maTemplatePath :: FilePath,
   maInitialParams :: [Double],
   maMaxCycles :: Int,
   maTracePath :: FilePath,
   maMaxAf :: Int,
   maNrCalledSites :: Int64,
   maHistPath :: FilePath
}

runMaxl :: MaxlOpt -> Script ()
runMaxl opts = do
    modelTemplate <- scriptIO $ readModelTemplate (maTemplatePath opts) (maTheta opts) defaultTimes
    hist <- loadHistogram (maMaxAf opts) (maNrCalledSites opts) (maHistPath opts)
    modelSpec <- hoistEither $ instantiateModel modelTemplate (V.fromList $ maInitialParams opts)
    hoistEither $ validateModel modelSpec
    let k = length $ maInitialParams opts
        initialParams' = replicate k 1.0
        scalingFactors = V.fromList $ maInitialParams opts
        minFunc' = either (const penalty) id . minFunc modelTemplate hist . unscaleParams scalingFactors . V.fromList
        (minResult, trace) = minimize NMSimplex2 1.0e-8 (maMaxCycles opts) [0.01 | _ <- [0..k-1]] minFunc' initialParams'
        minResult' = unscaleParams scalingFactors (V.fromList minResult)
        trace' = scaleTraceMatrix scalingFactors trace
    scriptIO $ reportMaxResult modelTemplate minResult'
    scriptIO $ reportTrace modelTemplate trace' (maTracePath opts)
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

minFunc :: ModelTemplate -> RareAlleleHistogram -> V.Vector Double -> Either String Double
minFunc modelTemplate hist params = do
    modelSpec <- instantiateModel modelTemplate params
    case validateModel modelSpec of
        Right _ -> return $ -(computeLikelihood modelSpec hist)
        Left _ -> return penalty

validateModel :: ModelSpec -> Either String ()
validateModel (ModelSpec _ _ events) = do
    when (any (\p -> p < 0.001 || p > 100.0) [p | ModelEvent _ (SetPopSize _ p) <- events]) $ Left "illegal population sizes"
    let sortedEvents = sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) -> time1 `compare` time2) events
    checkEvents sortedEvents
  where
    checkEvents [] = Right ()
    checkEvents (ModelEvent _ (Join k l):rest) =
        if k == l || or [k' == l || l' == l | ModelEvent _ (Join k' l') <- rest]
            then Left "Illegal joins"
            else checkEvents rest
    checkEvents (ModelEvent _ (SetPopSize _ p):rest) =
        if p < 0.001 || p > 100.0 then Left "illegal populaton sizes" else checkEvents rest
    checkEvents (ModelEvent _ (SetGrowthRate _ r):rest) =
        if abs r  > 1000.0 then Left "Illegal growth rates" else checkEvents rest

penalty :: Double
penalty = 1.0e20


