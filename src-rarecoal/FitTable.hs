module FitTable (runFitTable, FitTableOpt(..)) where

import Rarecoal.Core (getProb)
import Rarecoal.ModelTemplate (ModelDesc, getModelSpec)
import Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), loadHistogram,
    SitePattern(..), computeStandardOrder)

import Control.Error (Script, tryRight, scriptIO)
import qualified Control.Foldl as F
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V

data FitTableOpt = FitTableOpt {
    _ftModelDesc :: ModelDesc,
    _ftMaxAf :: Int,
    _ftMinAf :: Int,
    _ftConditionOn :: [Int],
    _ftExcludePatterns :: [[Int]],
    _ftHistPath :: FilePath
}

runFitTable :: FitTableOpt -> Script ()
runFitTable opts = do
    let FitTableOpt modelDesc maxAf minAf conditionOn
            excludePatterns histPath = opts
    hist <- loadHistogram minAf maxAf conditionOn excludePatterns histPath
    standardOrder <- tryRight $ computeStandardOrder hist
    let RareAlleleHistogram names nVec _ _ _ _ countMap = hist
    modelSpec <- getModelSpec modelDesc names
    let totalCounts = fromIntegral $ M.foldl' (+) 0 countMap
    let realFreqs = do
            pat <- standardOrder
            let count = M.findWithDefault 0 (Pattern pat) countMap
            return (Pattern pat, fromIntegral count / fromIntegral totalCounts)
    theoryValues <- mapM (tryRight . getProb modelSpec nVec False) standardOrder
    let theoryFreqs = zipWith (\p v -> (Pattern p, v)) standardOrder
            theoryValues
    let combinedFold = (,) <$> singletonProbsF nVec <*> sharingProbsF nVec
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
        l = [intercalate "\t" [label, show real, show fit] |
             (label, real, fit) <- zip3 allLabels allProbs allProbsTheory]
    scriptIO . putStrLn . intercalate "\t" $ ["POP", "REAL", "FIT"]
    scriptIO $ mapM_ putStrLn l

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

normFactorF :: F.Fold (SitePattern, Double) Double
normFactorF = F.Fold step initial extract
  where
    step count (_, val) = count + val
    initial = 0
    extract = id
