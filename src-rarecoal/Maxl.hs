{-# LANGUAGE OverloadedStrings #-}
module Maxl (runMaxl, MaxlOpt(..)) where

import Rarecoal.ModelTemplate (ModelTemplate(..), instantiateModel, getModelTemplate, 
    ModelOptions(..), ParamOptions(..), makeParameterDict, getParamNames, fillParameterDictWithDefaults)
import Rarecoal.Utils (GeneralOptions(..), HistogramOptions(..), setNrProcessors, loadHistogram)
import Rarecoal.MaxUtils (penalty, minFunc, computeFrequencySpectrum, 
    writeFullFitTable, writeSummaryFitTable, makeInitialPoint)
import Rarecoal.Powell (powellV)

import Control.Error (Script, scriptIO, tryRight, errLn)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import Numeric.LinearAlgebra.Data (toRows, toList)
import Numeric.GSL.Minimization (minimize, MinimizeMethod(..))
import System.IO (Handle, IOMode(..), hPutStrLn, hPutStr, withFile)
import Turtle (format, (%), w)

data MaxlOpt = MaxlOpt {
    maGeneralOpts :: GeneralOptions,
    maModelOpts :: ModelOptions,
    maParamOpts :: ParamOptions,
    maHistogramOpts :: HistogramOptions,
    maMaxCycles :: Int,
    maNrRestarts :: Int,
    maOutPrefix :: FilePath,
    maUsePowell :: Bool,
    maPowellTolerance :: Double
}

runMaxl :: MaxlOpt -> Script ()
runMaxl opts = do
    scriptIO $ setNrProcessors (maGeneralOpts opts)
    modelTemplate <- getModelTemplate (maModelOpts opts)
    modelParams <- (scriptIO $ makeParameterDict (maParamOpts opts)) >>= 
        fillParameterDictWithDefaults modelTemplate
    _ <- tryRight $ instantiateModel (maGeneralOpts opts) modelTemplate modelParams
    let modelBranchNames = mtBranchNames modelTemplate
    (hist, siteRed) <- loadHistogram (maHistogramOpts opts) modelBranchNames
    xInit <- tryRight $ makeInitialPoint modelTemplate modelParams
    _ <- tryRight $ minFunc (maGeneralOpts opts) modelTemplate hist siteRed xInit
    let minFunc' = either (const penalty) id .
            minFunc (maGeneralOpts opts) modelTemplate hist siteRed
    (minResult, trace) <- if maUsePowell opts then do
            (r, _, tr) <- scriptIO $
                powellV (maPowellTolerance opts) (maMaxCycles opts) minFunc' xInit
            return (r, tr)
        else do
            let minimizationRoutine = minimizeV (maMaxCycles opts) minFunc'
            scriptIO $ minimizeWithRestarts (maNrRestarts opts) minimizationRoutine xInit
        
    let outMaxlFN = maOutPrefix opts ++ ".paramEstimates.txt"
        outTraceFN = maOutPrefix opts ++ ".trace.txt"
        outFullFitTableFN = maOutPrefix opts ++ ".frequencyFitTable.txt"
        outSummaryTableFN = maOutPrefix opts ++ ".summaryFitTable.txt"
    scriptIO . withFile outMaxlFN WriteMode $ \h ->
        reportMaxResult modelTemplate minResult (minFunc' minResult) h
    scriptIO . withFile outTraceFN WriteMode $ \h ->
        if maUsePowell opts then
            reportTracePowell modelTemplate trace h
        else
            reportTraceSimplex modelTemplate trace h
    let finalModelParams = [(n, r) | ((n, _), r) <- zip modelParams (V.toList minResult)]
    finalModelSpec <- tryRight $ instantiateModel (maGeneralOpts opts) modelTemplate 
        finalModelParams
    finalSpectrum <- tryRight $ computeFrequencySpectrum finalModelSpec
        (optCoreFunc . maGeneralOpts $ opts) hist modelBranchNames
    scriptIO $ do
        writeFullFitTable outFullFitTableFN finalSpectrum
        writeSummaryFitTable outSummaryTableFN finalSpectrum hist

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
        errLn $ format ("minimizing from point "%w) res
        let (newRes, newTrace) = minR res
        go (n - 1) minR (newRes, trace ++ newTrace)

reportMaxResult :: ModelTemplate -> V.Vector Double -> Double -> Handle -> IO ()
reportMaxResult modelTemplate result minScore h = do
    hPutStrLn h $ "Score\t" ++ show minScore
    hPutStr h . unlines $ zipWith (\p v -> T.unpack p ++ "\t" ++ show v) (getParamNames modelTemplate) 
        (V.toList result)

reportTraceSimplex :: ModelTemplate -> [V.Vector Double] -> Handle -> IO ()
reportTraceSimplex modelTemplate trace h = do
    let header = intercalate "\t" $ ["Nr", "-Log-Likelihood", "Simplex size"] ++
            map T.unpack (getParamNames modelTemplate)
        body = map (intercalate "\t" . map show . V.toList) trace
    hPutStr h . unlines $ header : body

reportTracePowell :: ModelTemplate -> [V.Vector Double] -> Handle -> IO ()
reportTracePowell modelTemplate trace h = do
    let header = intercalate "\t" $ ["Nr", "-Log-Likelihood"] ++
            map T.unpack (getParamNames modelTemplate)
        body = do
            (i, t) <- zip [1..] trace
            return $ intercalate "\t" (show i : [show par | par <- V.toList t])
    hPutStr h . unlines $ header : body
