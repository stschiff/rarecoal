module Rarecoal.Utils (computeAllConfigs, loadHistogram,
    -- filterConditionOn, filterExcludePatterns, filterMaxAf, filterGlobalMinAf,
    computeStandardOrder, GeneralOptions(..), HistogramOptions(..), histLookup) where

import Rarecoal.Formats.RareAlleleHistogram (RareAlleleHistogram(..),
    SitePattern, readHistogram)
import Rarecoal.Core (ModelEvent(..))

import Control.Error
import Control.Monad ((>=>), when)
import qualified Data.Map.Strict as Map
import Data.Text (Text)


data GeneralOptions = GeneralOptions {
    optTheta :: Double,
    optNrThreads :: Int,
    optNoShortcut :: Bool,
    optRegPenalty :: Double,
    optN0 :: Int,
    optLinGen :: Int,
    optTMax :: Double
}

data HistogramOptions = HistogramOptions {
    optHistPath :: FilePath,
    optMinAf :: Int,
    optMaxAf :: Int,
    optConditionOn :: [Int],
    optExcludePatterns :: [SitePattern]
}

setNrProcessors :: GeneralOptions -> IO ()
setNrProcessors generalOpts = do
    nrThreads <- optNrThreds generalOpts
    nrProc <- getNumProcessors
    if   nrThreads == 0
    then setNumCapabilities nrProc
    else setNumCapabilities nrThreads
    nrThreads' <- getNumCapabilities
    err ("running on " ++ show nrThreads' ++ " processors\n")

computeAllConfigs :: Int -> Int -> [Int] -> [SitePattern]
computeAllConfigs nrPop maxFreq nVec =
   let maxPowerNum = (maxFreq + 1) ^ nrPop
       order = map (digitize (maxFreq + 1) nrPop) [1..maxPowerNum]
   in  filter (\v -> (sum v <= maxFreq) && and (zipWith (<=) v nVec)) order

turnHistPatternIntoModelPattern :: RareAlleleHistogram -> Maybe [String] ->
    SitePattern -> Either String SitePattern
turnHistPatternIntoModelPattern hist maybeModelBranchNames histPattern = do
    case maybeModelBranchNames of
        Just modelBranchNames -> do
            let histBranchNames = raNames hist
            forM modelBranchNames $ \modelBranchName ->
                case modelBranchName `elemIndex` histBranchNames of
                    Just i -> return (histPattern !! i)
                    Nothing -> return 0 -- ghost branch
        Nothing -> return histPattern

digitize :: Int -> Int -> Int -> [Int]
digitize base nrDigit num
    | nrDigit == 1 = [num]
    | otherwise    = let digitBase = base ^ (nrDigit - 1)
                         digit = div num digitBase
                         rest = num - digit * digitBase
                     in  (digit:digitize base (nrDigit - 1) rest)

loadHistogram :: HistogramOptions -> ModelTemplate -> Script RareAlleleHistogram
loadHistogram histOpts modelTemplate = do
    let HistogramOptions path minAf maxAf conditionOn excludePatterns = histOpts
    (_, modelBranchNames) <- getNrAndNamesOfBranches modelTemplate
    hist <- readHistogram path
    validateBranchNameCongruency modelBranchNames (raNames hist)
    reportGhostBranches hist modelBranchNames
    tryRight $ (filterMaxAf maxAf >=> filterGlobalMinAf minAf >=>
        filterConditionOn conditionOn >=> filterExcludePatterns excludePatterns)
        hist
  where
    validateBranchNameCongruency modelBranchNames histBranchNames =
        case modelBranchNames of
            [] -> return ()
            modelNames ->
                forM_ histBranchNames $ \histName ->
                    when (histName `notElem` modelNames) $
                        Left $ "histogram branch " ++ histName ++
                            " not found in model branches (" ++
                            show modelNames ++ ")"
                forM_ modelNames $ \modelName ->
                    when (modelName `notElem` histBranchNames) $
                        errLn $ "found ghost branch: " ++ show modelName


filterConditionOn :: [Int] -> RareAlleleHistogram ->
    Either String RareAlleleHistogram
filterConditionOn indices hist =
    if null indices then return hist else do
        let newBody = Map.filterWithKey conditionPatternOn (raCounts hist)
        return $ hist {raCounts = newBody, raConditionOn = indices}
  where
    conditionPatternOn pat _ = all (\i -> pat !! i > 0) indices

filterExcludePatterns :: [[Int]] -> RareAlleleHistogram ->
    Either String RareAlleleHistogram
filterExcludePatterns excludePatterns hist =
    if null excludePatterns then return hist else do
        let newBody = Map.filterWithKey pruneExcludePatterns (raCounts hist)
        return $ hist {raExcludePatterns = excludePatterns, raCounts = newBody}
  where
    pruneExcludePatterns pat _ = pat `notElem` excludePatterns

filterMaxAf :: Int -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterMaxAf maxAf' hist = do
    let maxAf = if maxAf' == 0 then raMaxAf hist else maxAf'
    when (maxAf > raMaxAf hist || maxAf < raMinAf hist) $ Left "illegal maxAF"
    if maxAf == raMaxAf hist then return hist else do
        let newBody = Map.filterWithKey (prunePatternFreq maxAf) (raCounts hist)
        return $ hist {raMaxAf = maxAf, raCounts = newBody}
  where
    prunePatternFreq maxM pat _ = sum pat <= maxM

filterGlobalMinAf :: Int -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterGlobalMinAf minAf hist = do
    when (minAf > raMaxAf hist || minAf < raMinAf hist) $ Left "illegal minAf"
    if minAf == raMinAf hist then return hist else do
        let newBody = Map.filterWithKey prunePatternMinTotalFreq (raCounts hist)
        return $ hist {raCounts = newBody, raMinAf = minAf}
  where
    prunePatternMinTotalFreq pat _ = sum pat >= minAf

computeStandardOrder :: RareAlleleHistogram -> [SitePattern]
computeStandardOrder histogram =
    let nrPop = length $ raNVec histogram
        nVec = raNVec histogram
    in  filter (\p -> sum p >= raMinAf histogram &&
            hasConditioning (raConditionOn histogram) p &&
            p `notElem` raExcludePatterns histogram) $
            computeAllConfigs nrPop (raMaxAf histogram) nVec
  where
    hasConditioning indices pat = all (\i -> pat !! i > 0) indices

minFunc :: GeneralOptions -> ModelTemplate -> RareAlleleHistogram -> V.Vector Double -> Either String Double
minFunc generalOpts modelTemplate hist paramsVec = do
    let paramNames = getParamNames modelTemplate
    let paramsDict = zip paramNames . V.toList $ paramsVec
    modelSpec <- instantiateModel generalOpts modelTemplate paramsDict
    regPenalty <- getRegularizationPenalty modelSpec
    val <- computeLogLikelihood modelSpec hist False
    assertErr ("likelihood infinite for params " ++ show params) $ not (isInfinite val)
    assertErr ("likelihood NaN for params " ++ show params) $ not (isNaN val)
    return (-val + regPenalty)
    -- return (-val)

penalty :: Double
penalty = 1.0e20

computeLogLikelihood :: ModelSpec -> RareAlleleHistogram -> Bool ->
    Either String Double
computeLogLikelihood modelSpec histogram noShortcut = do
    assertErr "minFreq must be greater than 0" $ raMinAf histogram > 0
    standardOrder <- computeStandardOrder histogram
    let nVec = raNVec histogram
    patternProbs <- sequence $
        parMap rdeepseq (getProb modelSpec nVec noShortcut) standardOrder
    let patternCounts = map (defaultLookup histogram) standardOrder
        ll = sum $
            zipWith (\p c -> log p * fromIntegral c) patternProbs patternCounts
        otherCounts = raTotalNrSites histogram - sum patternCounts
    return $ ll + fromIntegral otherCounts * log (1.0 - sum patternProbs)

makeInitialPoint :: ModelTemplate -> [(String, Double)] ->
    Either String V.Vector Double
makeInitialPoint modelTemplate modelParams = do
    let paramNames = getParamNames modelTemplate
    vals <- forM paramNames $ \n ->
        case n `lookup` modelParams of
            Just x -> return x
            Nothing -> Left "did not find parameter " ++ show n
    return $ V.fromList vals
