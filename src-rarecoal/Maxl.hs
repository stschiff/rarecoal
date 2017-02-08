module Maxl (minFunc, penalty, runMaxl, MaxlOpt(..)) where

import Logl (computeLikelihood)
import Rarecoal.Core (getTimeSteps, ModelSpec(..), ModelEvent(..))
import Rarecoal.ModelTemplate (ModelTemplate(..), instantiateModel, readModelTemplate,
                               getInitialParams, ParamsDesc)
import Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), loadHistogram)

import Control.Error (Script, scriptIO, assertErr, tryRight, err)
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as V
import GHC.Conc (getNumCapabilities, setNumCapabilities, getNumProcessors)
import Numeric.LinearAlgebra.Data (toRows, toList)
import Numeric.GSL.Minimization (minimize, MinimizeMethod(..))
import System.Log.Logger (infoM)

data MaxlOpt = MaxlOpt {
   maTheta :: Double,
   maTemplatePath :: FilePath,
   maAdditionalEvents :: [ModelEvent],
   maParamsDesc :: ParamsDesc,
   maMaxCycles :: Int,
   maNrRestarts :: Int,
   maTracePath :: FilePath,
   maMinAf :: Int,
   maMaxAf :: Int,
   maConditionOn :: [Int],
   maExcludePatterns :: [[Int]],
   maLinGen :: Int,
   maHistPath :: FilePath,
   maNrThreads :: Int,
   maReg :: Double
}

runMaxl :: MaxlOpt -> Script ()
runMaxl opts = do
    nrProc <- scriptIO getNumProcessors
    if maNrThreads opts == 0
        then scriptIO $ setNumCapabilities nrProc
        else scriptIO $ setNumCapabilities (maNrThreads opts)
    nrThreads <- scriptIO getNumCapabilities
    scriptIO $ err ("running on " ++ show nrThreads ++ " processors\n")
    let times = getTimeSteps 20000 (maLinGen opts) 20.0
    modelTemplate <- readModelTemplate (maTemplatePath opts) (maTheta opts)
        times (maReg opts)
    hist <- loadHistogram (maMinAf opts) (maMaxAf opts) (maConditionOn opts)
        (maExcludePatterns opts) (maHistPath opts)
    x <- getInitialParams modelTemplate (maParamsDesc opts)
    let extraEvents = maAdditionalEvents opts
    _ <- tryRight $ minFunc modelTemplate extraEvents hist x
    let minFunc' = either (const penalty) id . minFunc modelTemplate extraEvents hist
        minimizationRoutine = minimizeV (maMaxCycles opts) minFunc'
    (minResult, trace) <- scriptIO $ minimizeWithRestarts (maNrRestarts opts) minimizationRoutine x
    scriptIO $ reportMaxResult modelTemplate minResult (minFunc' minResult)
    scriptIO $ reportTrace modelTemplate trace (maTracePath opts)

minimizeV :: Int -> (V.Vector Double -> Double) -> V.Vector Double -> (V.Vector Double, [V.Vector Double])
minimizeV nrCycles minFunc' initial =
    let (vec, trace) = minimize NMSimplex2 1.0e-8 nrCycles stepWidths (minFunc' . V.fromList) . V.toList $ initial
    in  (V.fromList vec, map (V.fromList . toList) . toRows $ trace)
  where
    stepWidths = map (max 1.0e-4 . abs . (0.01*)) . V.toList $ initial

minimizeWithRestarts :: Int
                     -> (V.Vector Double -> (V.Vector Double, [V.Vector Double]))
                     -> V.Vector Double
                     -> IO (V.Vector Double, [V.Vector Double])
minimizeWithRestarts nrRestarts minimizationRoutine initialParams =
    go nrRestarts minimizationRoutine (initialParams, [])
  where
    go 0 _ (res, trace) = return (res, trace)
    go n minR (res, trace) = do
        infoM "rarecoal" $ "minimizing from point " ++ show res
        let (newRes, newTrace) = minR res
        go (n - 1) minR (newRes, trace ++ newTrace)

reportMaxResult :: ModelTemplate -> V.Vector Double -> Double -> IO ()
reportMaxResult modelTemplate result minScore = do
    putStrLn $ "Score\t" ++ show minScore
    putStr $ unlines $ zipWith (\p v -> p ++ "\t" ++ show v) (mtParams modelTemplate) (V.toList result)

reportTrace :: ModelTemplate -> [V.Vector Double] -> FilePath -> IO ()
reportTrace modelTemplate trace path = do
    let header = intercalate "\t" $ ["Nr", "-Log-Likelihood", "Simplex size"] ++ mtParams modelTemplate
        body = map (intercalate "\t" . map show . V.toList) trace
    writeFile path . unlines $ header : body

minFunc :: ModelTemplate -> [ModelEvent] -> RareAlleleHistogram -> V.Vector Double -> Either String Double
minFunc modelTemplate extraEvents hist params = do
    modelSpec <- instantiateModel modelTemplate params (raNames hist)
    let events = mEvents modelSpec
        events' = extraEvents ++ events
        modelSpec' = modelSpec {mEvents = events'}
    val <- computeLikelihood modelSpec' hist False
    assertErr ("likelihood infinite for params " ++ show params) $ not (isInfinite val)
    assertErr ("likelihood NaN for params " ++ show params) $ not (isNaN val)
    return (-val)

penalty :: Double
penalty = 1.0e20
