module Maxl (minFunc, penalty, runMaxl, MaxlOpt(..)) where

import RareAlleleHistogram (RareAlleleHistogram, loadHistogram)
import Logl (computeLikelihood)
import Numeric.LinearAlgebra.Data (toRows, toList)
import Numeric.GSL.Minimization (minimize, MinimizeMethod(..))
import ModelTemplate (ModelTemplate(..), instantiateModel, readModelTemplate, InitialParams(..), getInitialParams)
import Data.List (intercalate)
import Data.Int (Int64)
import Core (getTimeSteps, ModelSpec(..), ModelEvent(..))
import qualified Data.Vector.Unboxed as V
import Control.Error (Script, scriptIO)
import Control.Error.Safe (assertErr)
import Control.Monad.Trans.Either (hoistEither)

data MaxlOpt = MaxlOpt {
   maTheta :: Double,
   maTemplatePath :: FilePath,
   maInitialParams :: InitialParams,
   maMaxCycles :: Int,
   maTracePath :: FilePath,
   maMinAf :: Int,
   maMaxAf :: Int,
   maConditionOn :: [Int],
   maNrCalledSites :: Int64,
   maLinGen :: Int,
   maIndices :: [Int],
   maHistPath :: FilePath
}

runMaxl :: MaxlOpt -> Script ()
runMaxl opts = do
    let times = getTimeSteps 20000 (maLinGen opts) 20.0
    modelTemplate <- readModelTemplate (maTemplatePath opts) (maTheta opts) times
    hist <- loadHistogram (maIndices opts) (maMinAf opts) (maMaxAf opts) (maConditionOn opts) (maNrCalledSites opts) (maHistPath opts)
    x <- getInitialParams modelTemplate $ maInitialParams opts
    _ <- hoistEither $ minFunc modelTemplate [] hist x
    let minFunc' = either (const penalty) id . minFunc modelTemplate [] hist . V.fromList
        stepWidths = V.toList . V.map (max 1.0e-4 . abs . (0.01*)) $ x
        (minResult, trace) = minimize NMSimplex2 1.0e-8 (maMaxCycles opts) stepWidths minFunc' $ V.toList x
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

minFunc :: ModelTemplate -> [ModelEvent] -> RareAlleleHistogram -> V.Vector Double -> Either String Double
minFunc modelTemplate extraEvents hist params = do
    modelSpec <- instantiateModel modelTemplate params
    let events = mEvents modelSpec
        events' = extraEvents ++ events
        modelSpec' = modelSpec {mEvents = events'}
    val <- computeLikelihood modelSpec' hist
    assertErr ("likelihood infinite for params " ++ show params) $ not (isInfinite val)
    assertErr ("likelihood NaN for params " ++ show params) $ not (isNaN val)
    return (-val)

penalty :: Double
penalty = 1.0e20


