module FitTable (runFitTable, FitTableOpt(..), writeFitTables) where

import Rarecoal.Options (GeneralOptions(..), ModelOptions(..), HistogramOptions(..))
import Rarecoal.Core (getProb, ModelSpec)
import Rarecoal.ModelTemplate (ModelDesc, getModelSpec)
import Rarecoal.Formats.RareAlleleHistogram (RareAlleleHistogram(..), SitePattern)
import Rarecoal.Utils (loadHistogram, computeStandardOrder)

import Control.Error (Script, tryRight, scriptIO)
import qualified Control.Foldl as F
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.List (intercalate, zip4)
import Data.STRef (newSTRef, modifySTRef, readSTRef)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import System.IO (withFile, IOMode(..), hPutStrLn)

data FitTableOpt = FitTableOpt {
    ftModelOpts :: ModelOptions,
    ftParamOpts :: ParamOpts,
    ftHistogramOpts :: HistogramOptions,
    ftOutPrefix :: FilePath
}

runFitTable :: FitTableOpt -> Script ()
runFitTable opts = do
    setNrProcessors opts
    let FitTableOpt modelOpts paramOpts histOpts outPrefix = opts
    let outFullTable = outPrefix ++ ".frequencyFitTable.txt"
        summaryTable = outPrefix ++ ".summaryFitTable.txt"

    modelTemplate <- getModelTemplate (ftModelOpts opts)
    modelParams <- makeParameterDict (ftParamOpts opts)
    modelSpec <- tryRight $ instantiateModel (ftGeneralOpts opts )
        modelTemplate modelParams
    hist <- loadHistogram histOpts modelTemplate
    writeFitTables outFullTable summaryTable hist modelSpec

writeFitTables :: FilePath -> FilePath -> RareAlleleHistogram -> ModelSpec ->
    Script ()
writeFitTables outFullTable outSummaryTable hist modelSpec = do
    standardOrder <- tryRight $ computeStandardOrder hist
    theoryValues <- mapM (tryRight . getProb modelSpec (raNVec hist) False) standardOrder

    scriptIO $ do
        writeFullTable outFullTable standardOrder hist theoryValues
        writeSummaryTable outSummaryTable standardOrder hist theoryValues

writeFullTable :: FilePath -> [SitePattern] -> RareAlleleHistogram -> [Double] -> IO ()
writeFullTable outFN standardOrder hist theoryValues = do
    let countMap = raCounts hist
        totalCounts = raTotalNrSites hist
    withFile outFN WriteMode $ \outF -> do
        let headerLine = case raJackknifeEstimates hist of
                Nothing -> "Pattern\tCount\tFreq\tTheoryFreq\trelDev%"
                Just _ -> "Pattern\tCount\tFreq\tTheoryFreq\trelDev%\tstdErr\tZ-score"
        hPutStrLn outF headerLine
        forM_ (zip standardOrder theoryValues) $ \(p, theoryFreq) -> do
            let realCount = M.findWithDefault 0 p countMap
                realFreq = fromIntegral realCount / fromIntegral totalCounts
                fitDev = if realCount == 0
                         then "n/a"
                         else
                             let val = round $ 100.0 * (theoryFreq - realFreq) / realFreq :: Int
                             in show val

                -- (mean, lower, upper) = wilsonScoreInterval totalCounts realCount
                -- stdErr = upper - lower
                -- zScore = (theoryFreq - realFreq) / stdErr
                patternString = "(" ++ intercalate "," (map show p) ++ ")"
                outS = case raJackknifeEstimates hist of
                        Nothing -> intercalate "\t" $ [patternString, show realCount, show realFreq,
                                                       show theoryFreq, fitDev]
                        Just jkDict ->
                            let Just (_, jkSE) = p `M.lookup` jkDict
                                zScore = (theoryFreq - realFreq) / jkSE
                            in  intercalate "\t" $ [patternString, show realCount, show realFreq,
                                                    show theoryFreq, fitDev, show jkSE, show zScore]
            hPutStrLn outF outS

writeSummaryTable :: FilePath -> [SitePattern] -> RareAlleleHistogram -> [Double] -> IO ()
writeSummaryTable outFN standardOrder hist theoryValues = do
    let names = raNames hist
        nVec = raNVec hist
        countMap = raCounts hist
        totalCounts = raTotalNrSites hist
        realFreqs = do
            pat <- standardOrder
            let count = M.findWithDefault 0 pat countMap
            return (pat, fromIntegral count / fromIntegral totalCounts)
        theoryFreqs = zipWith (\p v -> (p, v)) standardOrder theoryValues
        combinedFold = (,) <$> singletonProbsF nVec <*> sharingProbsF nVec
        (singletonProbs, sharingProbs) = runST $ F.foldM combinedFold realFreqs
        maybeErrorSummaries = case raJackknifeEstimates hist of
            Nothing -> Nothing
            Just jkDict ->
                let errors = do
                        pat <- standardOrder
                        let (_, se) = M.findWithDefault (0, 0) pat jkDict
                        return (pat, se)
                    combinedErrorFold = (,) <$> singletonErrorsF nVec <*> sharingErrorsF nVec
                in  Just (runST $ F.foldM combinedErrorFold errors)
        (singletonProbsTheory, sharingProbsTheory) = runST $ F.foldM combinedFold theoryFreqs
        singletonLabels = map (++ "(singletons)") names
        sharingLabels = [names!!i ++ "/" ++ names!!j | i <- [0..(length names - 1)],
                         j <- [i .. (length names - 1)]]
        allLabels = singletonLabels ++ sharingLabels
        allProbs = singletonProbs ++ sharingProbs
        allProbsTheory = singletonProbsTheory ++ sharingProbsTheory
        getFitDev r f = if r > 0 then
                            let val = round $ 100.0 * (f - r) / r :: Int in show val
                        else "n/a"
        (header, lines_) = case maybeErrorSummaries of
            Nothing ->
                let h = intercalate "\t" $ ["Populations", "AlleleSharing", "Predicted",
                                                 "relDev%"]
                    l = do
                        (label, real, fit) <- zip3 allLabels allProbs allProbsTheory
                        let fitDev = getFitDev real fit
                        return $ intercalate "\t" [label, show real, show fit, fitDev]
                in  (h, l)
            Just (singletonErrors, sharingErrors) ->
                let h = intercalate "\t" $ ["Populations", "AlleleSharing", "Predicted",
                                                 "relDev%", "stdErr", "zScore"]
                    l = do
                        let allErrors = singletonErrors ++ sharingErrors
                        (label, real, fit, se) <- zip4 allLabels allProbs allProbsTheory allErrors
                        let fitDev = getFitDev real fit
                        let zScore = (fit - real) / se
                        return $ intercalate "\t" [label, show real, show fit, fitDev, show se,
                                                   show zScore]
                in  (h, l)
    withFile outFN WriteMode $ \h -> do
        hPutStrLn h header
        mapM_ (hPutStrLn h) lines_

singletonProbsF :: [Int] -> F.FoldM (ST s) (SitePattern, Double) [Double]
singletonProbsF nVec = F.FoldM step initial extract
  where
    step vec (p, val) = do
        when (sum p == 1) $ do
            let i = snd . head . filter ((>0) . fst) $ zip p [0..]
            VM.modify vec (\v -> v + val) i
        return vec
    initial = do
        v <- VM.new (length nVec)
        VM.set v 0.0
        return v
    extract = fmap V.toList . V.freeze

sharingProbsF :: [Int] -> F.FoldM (ST s) (SitePattern, Double) [Double]
sharingProbsF nVec = F.FoldM step initial extract
  where
    step vec (p, val) = do
        index <- newSTRef 0
        forM_ [0 .. (length nVec - 1)] $ \i ->
            forM_ [i .. (length nVec - 1)] $ \j -> do
                let add = if i == j
                          then val * fromIntegral (p!!i * (p!!i - 1)) /
                                     fromIntegral (nVec!!i * (nVec!!i - 1))
                          else val * fromIntegral (p!!i * p!!j) /
                                     fromIntegral (nVec!!i * nVec!!j)
                index' <- readSTRef index
                VM.modify vec (\v -> v + add) index'
                modifySTRef index (+1)
        return vec
    initial = do
        v <- VM.new (length nVec * (length nVec + 1) `div` 2)
        VM.set v 0.0
        return v
    extract = fmap V.toList . V.freeze

singletonErrorsF :: [Int] -> F.FoldM (ST s) (SitePattern, Double) [Double]
singletonErrorsF nVec = F.FoldM step initial extract
  where
    step vec (p, se) = do
        when (sum p == 1) $ do
            let i = snd . head . filter ((>0) . fst) $ zip p [0..]
            VM.modify vec (\v -> v + se ^ (2::Int)) i
        return vec
    initial = do
        v <- VM.new (length nVec)
        VM.set v 0.0
        return v
    extract = fmap ((map sqrt) . V.toList) . V.freeze

sharingErrorsF :: [Int] -> F.FoldM (ST s) (SitePattern, Double) [Double]
sharingErrorsF nVec = F.FoldM step initial extract
  where
    step vec (p, se) = do
        index <- newSTRef 0
        forM_ [0 .. (length nVec - 1)] $ \i ->
            forM_ [i .. (length nVec - 1)] $ \j -> do
                let add = if i == j
                          then se * fromIntegral (p!!i * (p!!i - 1)) /
                                    fromIntegral (nVec!!i * (nVec!!i - 1))
                          else se * fromIntegral (p!!i * p!!j) /
                                    fromIntegral (nVec!!i * nVec!!j)
                index' <- readSTRef index
                VM.modify vec (\v -> v + add ^ (2::Int)) index'
                modifySTRef index (+1)
        return vec
    initial = do
        v <- VM.new (length nVec * (length nVec + 1) `div` 2)
        VM.set v 0.0
        return v
    extract = fmap ((map sqrt) . V.toList) . V.freeze
