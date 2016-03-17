module FitTable (runFitTable, FitTableOpt(..)) where

import Rarecoal.Core (getProb)
import Rarecoal.ModelTemplate (ModelDesc, getModelSpec)
import Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), loadHistogram, SitePattern(..))

import Control.Error (Script, tryRight, scriptIO)
import qualified Control.Foldl as F
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V

data FitTableOpt = FitTableOpt {
    _ftModelDesc :: ModelDesc,
    _ftTheta :: Double,
    _ftLinGen :: Int,
    _ftMaxAf :: Int,
    _ftHistPath :: FilePath
}

runFitTable :: FitTableOpt -> Script ()
runFitTable (FitTableOpt modelDesc theta linGen maxAf histPath) = do
    RareAlleleHistogram names nVec _ _ _ countMap <- loadHistogram 1 maxAf [] histPath
    modelSpec <- getModelSpec modelDesc names theta linGen
    let rawCountList = [(p, fromIntegral c) | (p, c) <- M.toList countMap]
    probList <- tryRight . sequence $ [(\prob -> (p, prob)) <$> getProb modelSpec nVec False pat |
                                       (p@(Pattern pat), _) <- rawCountList]
    let combinedFold = (,,) <$> singletonProbsF nVec <*> sharingProbsF nVec <*> normFactorF
        combinedFoldTheory = (,) <$> singletonProbsF nVec <*> sharingProbsF nVec
        (singletonProbs, sharingProbs, normFactor) = F.fold combinedFold rawCountList
        (singletonProbsTheory, sharingProbsTheory) = F.fold combinedFoldTheory probList
        singletonLabels = map (++ "_singletons") names
        sharingLabels = [names!!i ++ "/" ++ names!!j |
                         i <- [0..(length names - 1)], j <- [i .. (length names - 1)]]
        allLabels = singletonLabels ++ sharingLabels
        allProbs = singletonProbs ++ sharingProbs
        allProbsTheory = singletonProbsTheory ++ sharingProbsTheory
        l = [intercalate "\t" [label, show (real / normFactor), show fit] |
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