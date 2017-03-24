module FitTable (runFitTable, FitTableOpt(..), writeFitTables) where

import Rarecoal.Core (getProb, ModelSpec)
import Rarecoal.ModelTemplate (ModelDesc, getModelSpec)
import Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), loadHistogram,
    SitePattern(..), computeStandardOrder)

import Control.Error (Script, tryRight, scriptIO)
import qualified Control.Foldl as F
import Control.Monad (forM_)
import Data.Int (Int64)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import System.IO (withFile, IOMode(..), hPutStrLn)

data FitTableOpt = FitTableOpt {
    _ftModelDesc :: ModelDesc,
    _ftMaxAf :: Int,
    _ftMinAf :: Int,
    _ftConditionOn :: [Int],
    _ftExcludePatterns :: [[Int]],
    _ftHistPath :: FilePath,
    _ftOutPrefix :: FilePath
}

runFitTable :: FitTableOpt -> Script ()
runFitTable opts = do
    let FitTableOpt modelDesc maxAf minAf conditionOn
            excludePatterns histPath outPrefix = opts
    
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

writeFullTable :: FilePath -> [[Int]] -> RareAlleleHistogram -> [Double] -> IO ()
writeFullTable outFN standardOrder hist theoryValues = do
    let countMap = raCounts hist
        totalCounts = M.foldl' (+) 0 countMap
    withFile outFN WriteMode $ \outF -> do
        hPutStrLn outF $ "Pattern\tCount\tFreq\tTheoryFreq\trelDev%"
        forM_ (zip standardOrder theoryValues) $ \(p, theoryFreq) -> do
            let realCount = M.findWithDefault 0 (Pattern p) countMap
                realFreq = fromIntegral realCount / fromIntegral totalCounts
                fitDev = if realCount == 0
                         then "n/a"
                         else show . round $ 100.0 * (theoryFreq - realFreq) / realFreq
                (mean, lower, upper) = wilsonScoreInterval totalCounts realCount
                -- stdErr = upper - lower
                -- zScore = (theoryFreq - realFreq) / stdErr
                patternString = "(" ++ intercalate "," (map show p) ++ ")"
            hPutStrLn outF . intercalate "\t" $ [patternString, show realCount, show realFreq,
                show theoryFreq, fitDev]

-- https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
-- not used at the moment
wilsonScoreInterval :: Int64 -> Int64 -> (Double, Double, Double)
wilsonScoreInterval n ns = (mean, lowerCI, upperCI)
  where
    mean = fromIntegral ns / fromIntegral n
    lowerCI = term1 * (term2 - term3)
    upperCI = term1 * (term2 + term3)
    term1 = 1.0 / (fromIntegral n + z^2)
    z = 1.96
    term2 = fromIntegral ns + 0.5 * z^2
    term3 = z * sqrt (1.0 / fromIntegral n * fromIntegral ns * fromIntegral nf + 0.25 * z^2)
    nf = n - ns

writeSummaryTable :: FilePath -> [[Int]] -> RareAlleleHistogram -> [Double] -> IO ()
writeSummaryTable outFN standardOrder hist theoryValues = do
    let RareAlleleHistogram names nVec _ _ _ _ countMap = hist
    let totalCounts = fromIntegral $ M.foldl' (+) 0 countMap :: Int
        realFreqs = do
            pat <- standardOrder
            let count = M.findWithDefault 0 (Pattern pat) countMap
            return (Pattern pat, fromIntegral count / fromIntegral totalCounts)
        theoryFreqs = zipWith (\p v -> (Pattern p, v)) standardOrder
            theoryValues
        combinedFold = (,) <$> singletonProbsF nVec <*> sharingProbsF nVec
        (singletonProbs, sharingProbs) = F.fold combinedFold realFreqs
        (singletonProbsTheory, sharingProbsTheory) =
            F.fold combinedFold theoryFreqs
        singletonLabels = map (++ "(singletons)") names
        sharingLabels =
            [names!!i ++ "/" ++ names!!j |
            i <- [0..(length names - 1)], j <- [i .. (length names - 1)]]
        allLabels = singletonLabels ++ sharingLabels
        allProbs = singletonProbs ++ sharingProbs
        allProbsTheory = singletonProbsTheory ++ sharingProbsTheory
        l = [intercalate "\t" [label, show real, show fit,
                if real > 0 then show . round $ 100.0 * (fit - real) / real else "n/a"] |
             (label, real, fit) <- zip3 allLabels allProbs allProbsTheory]
    withFile outFN WriteMode $ \h -> do
        hPutStrLn h . intercalate "\t" $ ["Populations", "AlleleSharing", "Predicted", "relDev%"]
        mapM_ (hPutStrLn h) l

singletonProbsF :: [Int] -> F.Fold (SitePattern, Double) [Double]
singletonProbsF nVec = F.Fold step initial extract
  where
    step vec (pat, val) = case pat of
        Higher -> vec
        Pattern p ->
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
    step vec (pat, val) = case pat of
        Higher -> vec
        Pattern p ->
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
