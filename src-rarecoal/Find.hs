{-# LANGUAGE OverloadedStrings #-}
module Find (runFind, FindOpt(..)) where

import Rarecoal.Utils (GeneralOptions(..), HistogramOptions(..), setNrProcessors)
import Rarecoal.Core (ModelSpec(..), ModelEvent(..), EventType(..))
import SequenceFormats.RareAlleleHistogram (RareAlleleHistogram(..))
import Rarecoal.Utils (loadHistogram, ModelBranch)
import Rarecoal.MaxUtils (computeLogLikelihood)
import Rarecoal.ModelTemplate (getModelTemplate, makeParameterDict, ModelOptions(..), 
    instantiateModel, ParamOptions(..), ModelTemplate(..))

import Control.Error (Script, scriptIO, tryAssert, tryRight, tryJust)
import Data.List (maximumBy, elemIndex)
import Data.Text (pack)
import System.IO (stderr, hPutStrLn, openFile, IOMode(..), hClose)
import Turtle (format, s, (%), d)

data FindOpt = FindOpt {
    fiGeneralOpts :: GeneralOptions,
    fiModelOpts :: ModelOptions,
    fiParamOpts :: ParamOptions,
    fiHistOpts :: HistogramOptions,
    fiQueryBranch :: ModelBranch,
    fiEvalPath :: FilePath,
    fiBranchAge :: Double,
    fiDeltaTime :: Double,
    fiMaxTime :: Double
}

runFind :: FindOpt -> Script ()
runFind opts = do
    scriptIO $ setNrProcessors (fiGeneralOpts opts)
    modelTemplate <- getModelTemplate (fiModelOpts opts)
    modelParams <- scriptIO $ makeParameterDict (fiParamOpts opts)
    modelSpec <- tryRight $ instantiateModel (fiGeneralOpts opts) modelTemplate modelParams
    (hist, siteRed) <- loadHistogram (fiHistOpts opts) (mtBranchNames modelTemplate)
    l <- findQueryIndex (mtBranchNames modelTemplate) (fiQueryBranch opts)
    tryAssert (format ("model must have free branch "%d) l) $ hasFreeBranch l modelSpec
    let modelSpec' =
            if fiBranchAge opts > 0.0
            then
                let events = ModelEvent 0.0 (SetFreeze l True) :
                             ModelEvent (fiBranchAge opts) (SetFreeze l False) :
                             mEvents modelSpec
                in  modelSpec {mEvents = events}
            else
                modelSpec
    let nrPops = mNrPops modelSpec'
        allParamPairs = do
            branch <- [0..(nrPops - 1)]
            False <- return $ branch == l
            False <- return $ isEmptyBranch modelSpec' branch (fiBranchAge opts)
            time <- getJoinTimes modelSpec' (fiDeltaTime opts) (fiMaxTime opts) (fiBranchAge opts)
                                  branch
            return (branch, time)
    allLikelihoods <- sequence $ do
        (k, t) <- allParamPairs
        let useCore2 = optUseCore2 . fiGeneralOpts $ opts
        return $ computeLogLikelihoodIO hist siteRed modelSpec' useCore2
            (mtBranchNames modelTemplate) k l t
    scriptIO $ writeResult (fiEvalPath opts) allParamPairs allLikelihoods
    let ((minBranch, minTime), minLL) = maximumBy (\(_, ll1) (_, ll2) -> ll1 `compare` ll2) $
                                        zip allParamPairs allLikelihoods
    scriptIO . putStrLn $ "highest likelihood point:\nbranch " ++ show minBranch ++
                          "\ntime " ++ show minTime ++ "\nlog-likelihood " ++ show minLL
  where
    hasFreeBranch queryBranch modelSpec =
        let e = mEvents modelSpec
            jIndices = concat [[k, l] | ModelEvent _ (Join k l) <- e]
        in  queryBranch `notElem` jIndices

findQueryIndex :: [ModelBranch] -> ModelBranch -> Script Int
findQueryIndex names branchName =
    tryJust (format ("could not find branch name "%s) (pack branchName)) $ elemIndex branchName names

isEmptyBranch :: ModelSpec -> Int -> Double -> Bool
isEmptyBranch modelSpec l t = not $ null previousJoins
  where
    previousJoins = [j | ModelEvent t' j@(Join _ l') <- mEvents modelSpec, l' == l, t' < t]

getJoinTimes :: ModelSpec -> Double -> Double -> Double -> Int -> [Double]
getJoinTimes modelSpec deltaT maxT branchAge k =
    let allTimes = takeWhile (<=maxT) $ map ((+branchAge) . (*deltaT)) [1.0,2.0..]
        leaveTimes = [t | ModelEvent t (Join _ l) <- mEvents modelSpec, k == l]
    in  if null leaveTimes then allTimes else filter (<head leaveTimes) allTimes

computeLogLikelihoodIO :: RareAlleleHistogram -> Double -> ModelSpec -> Bool -> [ModelBranch] ->
    Int -> Int -> Double -> Script Double
computeLogLikelihoodIO hist siteRed modelSpec useCore2 modelBranchNames k l t = do
    let e = mEvents modelSpec
        newE = ModelEvent t (Join k l)
        modelSpec' = modelSpec {mEvents = newE : e}
    ll <- tryRight $ computeLogLikelihood modelSpec' useCore2 hist modelBranchNames siteRed
    scriptIO $ hPutStrLn stderr ("branch=" ++ show k ++ ", time=" ++ show t ++ ", ll=" ++ show ll)
    return ll

writeResult :: FilePath -> [(Int, Double)] -> [Double] -> IO ()
writeResult fp paramPairs allLikelihoods = do
    h <- openFile fp WriteMode
    hPutStrLn h "Branch\tTime\tLikelihood"
    let f (k, t) l = show k ++ "\t" ++ show t ++ "\t" ++ show l
        l_ = zipWith f paramPairs allLikelihoods
    mapM_ (hPutStrLn h) l_
    hClose h
