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
import Control.Monad.Trans.Either (hoistEither, left)

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
    modelTemplate <- readModelTemplate (maTemplatePath opts) (maTheta opts) defaultTimes
    hist <- loadHistogram (maMaxAf opts) (maNrCalledSites opts) (maHistPath opts)
    modelSpec <- hoistEither $ instantiateModel modelTemplate (V.fromList $ maInitialParams opts)
    let val = computeLikelihood modelSpec hist
    when (isInfinite val) $ left "initial likelihood is Infinite"
    hoistEither $ validateModel modelSpec
    let minFunc' = either (const penalty) id . minFunc modelTemplate hist . V.fromList
        stepWidths = [max 1.0e-8 $ abs (0.01 * p) | p <- maInitialParams opts]
        (minResult, trace) = minimize NMSimplex2 1.0e-8 (maMaxCycles opts) stepWidths minFunc' (maInitialParams opts)
        minScore = minFunc' minResult
        trace' = map toList $ toRows trace
    scriptIO $ reportMaxResult modelTemplate (V.fromList minResult) minScore
    scriptIO $ reportTrace modelTemplate trace' (maTracePath opts)

reportMaxResult :: ModelTemplate -> V.Vector Double -> Double -> IO ()
reportMaxResult modelTemplate result minScore = do
    putStrLn $ "Score\t" ++ show minScore
    putStr $ unlines $ zipWith (\p v -> p ++ "\t" ++ show v) (mtParams modelTemplate) (V.toList result)

reportTrace :: ModelTemplate -> [[Double]] -> FilePath -> IO ()
reportTrace modelTemplate trace path = do
    let header = intercalate "\t" $ ["Nr", "-Log-Likelihood", "Simplex size"] ++ mtParams modelTemplate
        body = unlines [intercalate "\t" [show val | val <- row] | row <- trace]
    writeFile path $ header ++ "\n" ++ body

minFunc :: ModelTemplate -> RareAlleleHistogram -> V.Vector Double -> Either String Double
minFunc modelTemplate hist params = do
    modelSpec <- instantiateModel modelTemplate params
    validateModel modelSpec
    let val = computeLikelihood modelSpec hist
    if isInfinite val then return penalty else return (-val)

validateModel :: ModelSpec -> Either String ()
validateModel (ModelSpec _ _ events) = do
    when (or [t < 0 | ModelEvent t _ <- events]) $ Left "Negative event times"
    let sortedEvents = sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) -> time1 `compare` time2) events
    checkEvents sortedEvents
  where
    checkEvents [] = Right ()
    checkEvents (ModelEvent _ (Join k l):rest) =
        if k == l || or [k' == l || l' == l | ModelEvent _ (Join k' l') <- rest]
            then Left "Illegal joins"
            else checkEvents rest
    checkEvents (ModelEvent _ (SetPopSize _ p):rest) =
        if p < 0.001 then Left "illegal populaton sizes" else checkEvents rest
    checkEvents (ModelEvent _ (SetGrowthRate _ r):rest) =
        if abs r  > 1000.0 then Left "Illegal growth rates" else checkEvents rest

penalty :: Double
penalty = 1.0e20


