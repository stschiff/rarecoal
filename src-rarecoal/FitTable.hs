module FitTable (runFitTable, FitTableOpt(..), writeFitTables) where

import Rarecoal.Core (getProb, ModelSpec)
import Rarecoal.ModelTemplate (ModelDesc, getModelSpec)
import Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), SitePattern)
import Rarecoal.Utils (loadHistogram, computeStandardOrder)

import Control.Error (Script, tryRight, scriptIO)
import qualified Control.Foldl as F
import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.List (intercalate)
import Data.STRef (newSTRef, modifySTRef, readSTRef)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import System.IO (withFile, IOMode(..), hPutStrLn)

data FitTableOpt = FitTableOpt {
    _ftModelDesc :: ModelDesc,
    _ftMaxAf :: Int,
    _ftMinAf :: Int,
    _ftConditionOn :: [Int],
    _ftExcludePatterns :: [SitePattern],
    _ftHistPath :: FilePath,
    _ftOutPrefix :: FilePath
}

runFitTable :: FitTableOpt -> Script ()
runFitTable opts = do
    let FitTableOpt modelDesc maxAf minAf conditionOn excludePatterns histPath outPrefix = opts
    
    let outFullTable = outPrefix ++ ".frequencyFitTable.txt"
        summaryTable = outPrefix ++ ".summaryFitTable.txt"
    
    hist <- loadHistogram minAf maxAf conditionOn excludePatterns histPath
    modelSpec <- getModelSpec modelDesc (raNames hist)
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

writeSummaryTable :: FilePath -> [[Int]] -> RareAlleleHistogram -> [Double] -> IO ()
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
        (singletonProbsTheory, sharingProbsTheory) = runST $ F.foldM combinedFold theoryFreqs
        singletonLabels = map (++ "(singletons)") names
        sharingLabels = [names!!i ++ "/" ++ names!!j | i <- [0..(length names - 1)],
                         j <- [i .. (length names - 1)]]
        allLabels = singletonLabels ++ sharingLabels
        allProbs = singletonProbs ++ sharingProbs
        allProbsTheory = singletonProbsTheory ++ sharingProbsTheory
        l = do
            (label, real, fit) <- zip3 allLabels allProbs allProbsTheory
            let fitDev = if real > 0
                         then
                             let val = round $ 100.0 * (fit - real) / real :: Int
                             in  show val
                             else "n/a"
            return $ intercalate "\t" [label, show real, show fit, fitDev]
    withFile outFN WriteMode $ \h -> do
        hPutStrLn h . intercalate "\t" $ ["Populations", "AlleleSharing", "Predicted", "relDev%"]
        mapM_ (hPutStrLn h) l

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
