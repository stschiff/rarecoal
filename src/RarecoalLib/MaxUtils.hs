{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module RarecoalLib.MaxUtils (penalty, makeInitialPoint, minFunc, computeLogLikelihood,
    computeFrequencySpectrum, computeLogLikelihoodFromSpec, SpectrumEntry(..), Spectrum,
    writeFullFitTable, writeSummaryFitTable)
where

import qualified RarecoalLib.Core                    as C1
import           RarecoalLib.ModelTemplate           (ModelTemplate (..),
                                                      getParamNames,
                                                      instantiateModel)
import           RarecoalLib.StateSpace              (getRegularizationPenalty,
                                                      makeJointStateSpace)
import           RarecoalLib.Utils                   (GeneralOptions (..),
                                                      ModelBranch,
                                                      RarecoalException (..),
                                                      computeStandardOrder,
                                                      turnHistPatternIntoModelPattern)
import           SequenceFormats.RareAlleleHistogram (RareAlleleHistogram (..),
                                                      SitePattern)

import qualified Control.Foldl                       as F
import           Control.Monad                       (forM, forM_, when)
import           Control.Monad.ST                    (ST, runST)
import           Control.Parallel.Strategies         (parMap, rdeepseq)
import           Data.Int                            (Int64)
import           Data.List                           (intercalate, zip4,
                                                      zipWith5)
import qualified Data.Map.Strict                     as Map
-- import qualified Data.MemoCombinators as M
import           Data.STRef                          (modifySTRef, newSTRef,
                                                      readSTRef)
import qualified Data.Vector.Unboxed                 as V
import qualified Data.Vector.Unboxed.Mutable         as VM
-- import Debug.Trace (trace)
import           System.IO                           (IOMode (..), hPutStrLn,
                                                      withFile)

data SpectrumEntry = SpectrumEntry
    { spPattern        :: SitePattern
    , spCount          :: Int64
    , spFreq           :: Double
    , spJackknifeError :: Maybe Double
    , spTheory         :: Double
    }

type Spectrum = [SpectrumEntry]

penalty :: Double
penalty = 1.0e20

makeInitialPoint :: ModelTemplate -> [(String, Double)] -> Either RarecoalException (V.Vector Double)
makeInitialPoint modelTemplate modelParams = do
    let paramNames = getParamNames modelTemplate
    vals <- forM paramNames $ \n ->
        case n `lookup` modelParams of
            Just x  -> return x
            Nothing -> Left . RarecoalModelException $ "did not find parameter " ++ show n
    return $ V.fromList vals


minFunc :: GeneralOptions -> ModelTemplate -> RareAlleleHistogram -> Double -> V.Vector Double ->
    Either RarecoalException Double
minFunc generalOpts modelTemplate hist siteRed paramsVec = do
    let paramNames = getParamNames modelTemplate
        paramsDict = zip paramNames . V.toList $ paramsVec
    modelSpec <- instantiateModel generalOpts modelTemplate paramsDict
    regPenalty <- getRegularizationPenalty modelSpec
    let modelBranchNames = mtBranchNames modelTemplate
    val <- computeLogLikelihood modelSpec hist modelBranchNames siteRed
    when (isInfinite val) $
        Left (RarecoalCompException ("likelihood infinite for params " ++ show paramsVec))
    when (isNaN val) $
        Left (RarecoalCompException ("likelihood NaN for params " ++ show paramsVec))
    return (-val + siteRed * regPenalty)

computeLogLikelihood :: C1.ModelSpec -> RareAlleleHistogram -> [ModelBranch] -> Double ->
    Either RarecoalException Double
computeLogLikelihood modelSpec histogram modelBranchNames siteRed =
    let totalNrSites = raTotalNrSites histogram
    in  computeLogLikelihoodFromSpec totalNrSites siteRed <$>
            computeFrequencySpectrum modelSpec histogram modelBranchNames

computeLogLikelihoodFromSpec :: Int64 -> Double -> Spectrum -> Double
computeLogLikelihoodFromSpec totalNrSites siteRed spectrum =
    let ll = sum [log (spTheory e) * fromIntegral (spCount e) | e <- spectrum]
        patternCounts = map spCount spectrum
        patternProbs = map spTheory spectrum
        otherCounts = totalNrSites - sum patternCounts
    in  siteRed * (ll + fromIntegral otherCounts * log (1.0 - sum patternProbs))

computeFrequencySpectrum :: C1.ModelSpec -> RareAlleleHistogram -> [ModelBranch] ->
    Either RarecoalException Spectrum
computeFrequencySpectrum modelSpec histogram modelBranchNames = do
    when (raMinAf histogram > 0) $ Left (RarecoalCompException "minFreq must be greater than 0")
    let standardOrder = computeStandardOrder histogram
    -- scriptIO . putStrLn $
    --     "computing probabilities for " ++ show (length standardOrder) ++
    --     " patterns"
    standardOrderModelMapped <-
        mapM (turnHistPatternIntoModelPattern (raNames histogram) modelBranchNames)
            standardOrder
    let nVec = raNVec histogram
    nVecModelMapped <-
        turnHistPatternIntoModelPattern (raNames histogram) modelBranchNames nVec
    let jointStateSpace = makeJointStateSpace (C1.mNrPops modelSpec) (raMaxAf histogram)
    let coreFunc = C1.getProb modelSpec jointStateSpace nVecModelMapped
    patternProbs <- sequence $
        parMap rdeepseq coreFunc standardOrderModelMapped
    -- trace (show $ zip standardOrderModelMapped patternProbs) $ return ()
    let patternCounts = [Map.findWithDefault 0 k (raCounts histogram) | k <- standardOrder]
        totalCounts = raTotalNrSites histogram
        patternFreqs = [fromIntegral c / fromIntegral totalCounts | c <- patternCounts]
        errors = case raJackknifeEstimates histogram of
            Just errMap ->
                [Just . snd $ Map.findWithDefault (0, 0) k errMap | k <- standardOrder]
            Nothing -> [Nothing | _ <- standardOrder]
    return $ zipWith5 SpectrumEntry standardOrder patternCounts patternFreqs errors patternProbs

-- memo4 :: M.Memo a -> M.Memo b -> M.Memo c -> M.Memo d ->
--     (a -> b -> c -> d -> r) -> (a -> b -> c -> d -> r)
-- memo4 a b c d = a . (M.memo3 b c d .)

writeFullFitTable :: FilePath -> Spectrum -> IO ()
writeFullFitTable outFN spectrum = do
    withFile outFN WriteMode $ \outF -> do
        let headerLine = case (spJackknifeError . head $ spectrum) of
                Nothing -> "Pattern\tCount\tFreq\tTheoryFreq\trelDev%"
                Just _ -> "Pattern\tCount\tFreq\tTheoryFreq\trelDev%\tstdErr\tZ-score"
        hPutStrLn outF headerLine
        forM_ spectrum $ \(SpectrumEntry pat count freq maybeErr theory) -> do
            let fitDev = if count == 0
                         then "n/a"
                         else
                             let val = round $ 100.0 * (theory - freq) / freq :: Int
                             in show val

                patternString = "(" ++ intercalate "," (map show pat) ++ ")"
                outS = case maybeErr of
                        Nothing -> intercalate "\t" [patternString, show count, show freq,
                            show theory,fitDev]
                        Just err ->
                            let zScore = (theory - freq) / err
                            in  intercalate "\t" [patternString, show count, show freq, show theory,
                                    fitDev, show err, show zScore]
            hPutStrLn outF outS

writeSummaryFitTable :: FilePath -> Spectrum -> RareAlleleHistogram -> IO ()
writeSummaryFitTable outFN spectrum hist = do
    let nVec = raNVec hist
        realFreqDict = [(pat, freq) | SpectrumEntry pat _ freq _ _ <- spectrum]
        theoryFreqDict = [(pat, theory) | SpectrumEntry pat _ _ _ theory <- spectrum]
        combinedFold = (,) <$> singletonProbsF (length nVec) <*> sharingProbsF nVec
        (singletonProbs, sharingProbs) = runST $ F.foldM combinedFold realFreqDict
        maybeErrorSummaries = case (spJackknifeError . head $ spectrum) of
            Nothing -> Nothing
            Just _ ->
                let errorDict = [(pat, err) | SpectrumEntry pat _ _ (Just err) _ <- spectrum]
                    combinedErrorFold = (,) <$> singletonErrorsF (length nVec) <*>
                        sharingErrorsF nVec
                in  Just (runST $ F.foldM combinedErrorFold errorDict)
        (singletonProbsTheory, sharingProbsTheory) = runST $ F.foldM combinedFold theoryFreqDict
        names = raNames hist
        singletonLabels = map (++ "(singletons)") names
        sharingLabels = [(names!!i) ++ "/" ++ (names!!j) | i <- [0..(length names - 1)], j <- [i .. (length names - 1)]]
        allLabels = singletonLabels ++ sharingLabels
        allProbs = singletonProbs ++ sharingProbs
        allProbsTheory = singletonProbsTheory ++ sharingProbsTheory
        getFitDev r f = if r > 0 then
                            let val = round $ 100.0 * (f - r) / r :: Int in show val
                        else "n/a"
        (header, lines_) = case maybeErrorSummaries of
            Nothing ->
                let h = intercalate "\t" ["Populations", "AlleleSharing",
                        "Predicted", "relDev%"]
                    l = do
                        (label, real, fit) <- zip3 allLabels allProbs allProbsTheory
                        let fitDev = getFitDev real fit
                        return $ intercalate "\t" [label, show real, show fit, fitDev]
                in  (h, l)
            Just (singletonErrors, sharingErrors) ->
                let h = intercalate "\t" ["Populations", "AlleleSharing",
                        "Predicted", "relDev%", "stdErr", "zScore"]
                    l = do
                        let allErrors = singletonErrors ++ sharingErrors
                        (label, real, fit, se) <- zip4 allLabels allProbs allProbsTheory allErrors
                        let fitDev = getFitDev real fit
                        let zScore = (fit - real) / se
                        return $ intercalate "\t" [label, show real, show fit, fitDev,
                            show se, show zScore]
                in  (h, l)
    withFile outFN WriteMode $ \h -> do
        hPutStrLn h header
        mapM_ (hPutStrLn h) lines_

singletonProbsF :: Int -> F.FoldM (ST s) (SitePattern, Double) [Double]
singletonProbsF nrPops = F.FoldM step initial extract
  where
    step vec (p, val) = do
        when (sum p == 1) $ do
            let i = snd . head . filter ((>0) . fst) $ zip p [0..]
            VM.modify vec (+val) i
        return vec
    initial = do
        v <- VM.new nrPops
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
                VM.modify vec (+add) index'
                modifySTRef index (+1)
        return vec
    initial = do
        v <- VM.new (length nVec * (length nVec + 1) `div` 2)
        VM.set v 0.0
        return v
    extract = fmap V.toList . V.freeze

singletonErrorsF :: Int -> F.FoldM (ST s) (SitePattern, Double) [Double]
singletonErrorsF nrPops = F.FoldM step initial extract
  where
    step vec (p, se) = do
        when (sum p == 1) $ do
            let i = snd . head . filter ((>0) . fst) $ zip p [0..]
            VM.modify vec (\v -> v + se ^ (2::Int)) i
        return vec
    initial = do
        v <- VM.new nrPops
        VM.set v 0.0
        return v
    extract = fmap (map sqrt . V.toList) . V.freeze

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
    extract = fmap (map sqrt . V.toList) . V.freeze
