module Maxl (minFunc, penalty, runMaxl, MaxlOpt(..)) where

import Logl (computeLogLikelihood)
import Rarecoal.Core (getTimeSteps, ModelSpec(..), ModelEvent(..), getRegularizationPenalty)
import Rarecoal.ModelTemplate (ModelTemplate(..), instantiateModel,
    readModelTemplate, getInitialParams, makeFixedParamsTemplate,
    reportGhostPops)
import Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), loadHistogram)
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
   maTheta :: Double,
   maTemplatePath :: FilePath,
   maAdditionalEvents :: [ModelEvent],
   maMaybeInputFile :: Maybe FilePath,
   maInitialValues :: [(String, Double)],
   maMaxCycles :: Int,
   maNrRestarts :: Int,
   maOutPrefix :: FilePath,
   maMinAf :: Int,
   maMaxAf :: Int,
   maConditionOn :: [Int],
   maExcludePatterns :: [[Int]],
   maLinGen :: Int,
   maHistPath :: FilePath,
   maNrThreads :: Int,
   maReg :: Double,
   maFixedParams :: [String]
}

runMaxl :: MaxlOpt -> Script ()
runMaxl opts = do
    nrProc <- scriptIO getNumProcessors
    if maNrThreads opts == 0
        then scriptIO $ setNumCapabilities nrProc
        else scriptIO $ setNumCapabilities (maNrThreads opts)
    nrThreads <- scriptIO getNumCapabilities
    scriptIO $ errLn ("running on " ++ show nrThreads ++ " processors")
    let times = getTimeSteps 20000 (maLinGen opts) 20.0
    modelTemplate <- readModelTemplate (maTemplatePath opts) (maTheta opts)
        times (maReg opts)
    hist <- loadHistogram (maMinAf opts) (maMaxAf opts) (maConditionOn opts)
        (maExcludePatterns opts) (maHistPath opts)
    x <- getInitialParams modelTemplate (maMaybeInputFile opts) (maInitialValues opts)
    reportGhostPops modelTemplate (raNames hist) x
    (modelTemplateWithFixedParams, xNew) <-
        if length (maFixedParams opts) > 0
        then do
            scriptIO $ errLn "loading modified model template with fixed parameters"
            mt <- tryRight $ makeFixedParamsTemplate modelTemplate (maFixedParams opts) x
            xNew <- getInitialParams mt (maMaybeInputFile opts) (maInitialValues opts)
            return (mt, xNew)
        else
            return (modelTemplate, x)
    let extraEvents = maAdditionalEvents opts
    _ <- tryRight $ minFunc modelTemplateWithFixedParams extraEvents hist xNew
    let minFunc' = either (const penalty) id . minFunc
            modelTemplateWithFixedParams extraEvents hist
        minimizationRoutine = minimizeV (maMaxCycles opts) minFunc'
    (minResult, trace) <- scriptIO $ minimizeWithRestarts (maNrRestarts opts)
        minimizationRoutine xNew
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

minFunc :: ModelTemplate -> [ModelEvent] -> RareAlleleHistogram -> V.Vector Double -> Either String Double
minFunc modelTemplate extraEvents hist params = do
    let names = raNames hist
    modelSpec <- instantiateModel modelTemplate params names
    regPenalty <- getRegularizationPenalty modelSpec
    let events = mEvents modelSpec
        events' = extraEvents ++ events
        modelSpec' = modelSpec {mEvents = events'}
    val <- computeLogLikelihood modelSpec' hist False
    assertErr ("likelihood infinite for params " ++ show params) $ not (isInfinite val)
    assertErr ("likelihood NaN for params " ++ show params) $ not (isNaN val)
    return (-val + regPenalty)
    -- return (-val)

penalty :: Double
penalty = 1.0e20
