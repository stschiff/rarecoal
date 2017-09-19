module Maxl (minFunc, penalty, runMaxl, MaxlOpt(..)) where

import Rarecoal.Core (getTimeSteps, ModelSpec(..), ModelEvent(..),
    getRegularizationPenalty)
import Rarecoal.Formats.RareAlleleHistogram (RareAlleleHistogram(..),
    SitePattern)
import Rarecoal.ModelTemplate (ModelTemplate(..), instantiateModel,
    readModelTemplate, getInitialParams, makeFixedParamsTemplate,
    reportGhostPops)
import Rarecoal.Options (GeneralOptions(..), ModelOptions(..),
    HistogramOptions(..))
import Rarecoal.Utils (minFunc, penalty, loadHistogram, computeLogLikelihood)
import FitTable (writeFitTables)

import Control.Error (Script, scriptIO, assertErr, tryRight, errLn)
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as V
import GHC.Conc (getNumCapabilities, setNumCapabilities, getNumProcessors)
import Numeric.LinearAlgebra.Data (toRows, toList)
import Numeric.GSL.Minimization (minimize, MinimizeMethod(..))
import System.IO (Handle, IOMode(..), hPutStrLn, hPutStr, withFile)
import System.Log.Logger (infoM)

data MaxlOpt = MaxlOpt {
    maGeneralOpts :: GeneralOptions,
    maModelOpts :: ModelOptions,
    maParamOpts :: ParamOpts,
    maHistogramOpts :: HistogramOptions,
    maMaxCycles :: Int,
    maNrRestarts :: Int,
    maOutPrefix :: FilePath,
    maFixedParams :: [String]
}

runMaxl :: MaxlOpt -> Script ()
runMaxl opts = do
    setNrProcessors opts
    modelTemplate <- getModelTemplate (maModelOpts opts)
    modelParams <- makeParameterDict (maParamOpts opts)
    modelSpec <- tryRight $ instantiateModel (maGeneralOpts opts )
        modelTemplate modelParams
    hist <- loadHistogram (maHistogramOpts opts) modelTemplate
    let minFunc' = either (const penalty) id .
            minFunc (maGeneralOpts opts) modelTemplate hist
        minimizationRoutine = minimizeV (maMaxCycles opts) minFunc'
    xInit <- makeInitialPoint modelTemplate modelParams
    (minResult, trace) <- scriptIO $ minimizeWithRestarts (maNrRestarts opts)
        minimizationRoutine xInit
    let outMaxlFN = maOutPrefix opts ++ ".paramEstimates.txt"
        outTraceFN = maOutPrefix opts ++ ".trace.txt"
        outFullFitTableFN = maOutPrefix opts ++ ".frequencyFitTable.txt"
        outSummaryTableFN = maOutPrefix opts ++ ".summaryFitTable.txt"
    scriptIO . withFile outMaxlFN WriteMode $ \h ->
        reportMaxResult modelTemplateWithFixedParams minResult (minFunc' minResult) h
    scriptIO . withFile outTraceFN WriteMode $ \h ->
        reportTrace modelTemplateWithFixedParams trace h
    finalModelSpec <- tryRight $ instantiateModel modelTemplate minResult (raNames hist)
    writeFitTables outFullFitTableFN outSummaryTableFN hist finalModelSpec

minimizeV :: Int -> (V.Vector Double -> Double) -> V.Vector Double ->
    (V.Vector Double, [V.Vector Double])
minimizeV nrCycles minFunc' initial =
    let (vec, trace) =
            minimize NMSimplex2 1.0e-8 nrCycles stepWidths (minFunc' . V.fromList) . V.toList $
            initial
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

reportMaxResult :: ModelTemplate -> V.Vector Double -> Double -> Handle -> IO ()
reportMaxResult modelTemplate result minScore h = do
    hPutStrLn h $ "Score\t" ++ show minScore
    hPutStr h . unlines $ zipWith (\p v -> p ++ "\t" ++ show v) (mtParams modelTemplate) (V.toList result)

reportTrace :: ModelTemplate -> [V.Vector Double] -> Handle -> IO ()
reportTrace modelTemplate trace h = do
    let header = intercalate "\t" $ ["Nr", "-Log-Likelihood", "Simplex size"] ++
            mtParams modelTemplate
        body = map (intercalate "\t" . map show . V.toList) trace
    hPutStr h . unlines $ header : body
