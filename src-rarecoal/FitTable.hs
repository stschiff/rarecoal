module FitTable (runFitTable, FitTableOpt(..), writeFitTables) where

import Rarecoal.Core (getProb, ModelSpec)
import Rarecoal.ModelTemplate (ModelDesc, getModelSpec)
import Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), SitePattern)
import Rarecoal.Utils (loadHistogram, computeStandardOrder)

import Control.Error (Script, tryRight, scriptIO, errLn)
import qualified Control.Foldl as F
import Control.Monad (forM_)
-- import Data.Int (Int64)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
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
        (singletonProbs, sharingProbs) = F.fold combinedFold realFreqs
        (singletonProbsTheory, sharingProbsTheory) =
            F.fold combinedFold theoryFreqs
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

singletonProbsF :: [Int] -> F.Fold (SitePattern, Double) [Double]
singletonProbsF nVec = F.Fold step initial extract
  where
    step vec (p, val) = 
        if   sum p == 1
        then
            let i = snd . head . filter ((>0) . fst) $ zip p [0..]
            in  vec V.// [(i, vec V.! i + val)]
        else vec
    initial = V.replicate (length nVec) 0.0
    extract = V.toList

sharingProbsF :: [Int] -> F.Fold (SitePattern, Double) [Double]
sharingProbsF nVec = F.Fold step initial extract
  where
    step vec (p, val) = 
        let addVec = V.fromList $ do
                i <- [0 .. (length nVec - 1)]
                j <- [i .. (length nVec - 1)]
                if   i == j
                then return $ val * fromIntegral (p!!i * (p!!i - 1)) /
                                    fromIntegral (nVec!!i * (nVec!!i - 1))
                else return $ val * fromIntegral (p!!i * p!!j) /
                                    fromIntegral (nVec!!i * nVec!!j)
        in  V.zipWith (+) vec addVec
    initial = V.replicate (length nVec * (length nVec + 1) `div` 2) 0.0
    extract = V.toList

-- normFactorF :: F.Fold (SitePattern, Double) Double
-- normFactorF = F.Fold step initial extract
--   where
--     step count (_, val) = count + val
--     initial = 0
--     extract = id
