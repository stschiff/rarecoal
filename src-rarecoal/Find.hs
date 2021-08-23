{-# LANGUAGE OverloadedStrings #-}
module Find (runFind, FindOpt(..)) where

import RarecoalLib.Utils (GeneralOptions(..), HistogramOptions(..), setNrProcessors)
import RarecoalLib.Core (ModelSpec(..), ModelEvent(..), EventType(..))
import SequenceFormats.RareAlleleHistogram (RareAlleleHistogram(..))
import RarecoalLib.Utils (loadHistogram, ModelBranch, RarecoalException(..), tryEither)
import RarecoalLib.MaxUtils (computeLogLikelihood)
import RarecoalLib.ModelTemplate (getModelTemplate, makeParameterDict, ModelOptions(..), 
    instantiateModel, ParamOptions(..), ModelTemplate(..))

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.List (maximumBy, elemIndex)
import System.IO (stderr, hPutStrLn, openFile, IOMode(..), hClose)

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

runFind :: FindOpt -> IO ()
runFind opts = do
    setNrProcessors (fiGeneralOpts opts)
    modelTemplate <- getModelTemplate (fiModelOpts opts)
    modelParams <- makeParameterDict (fiParamOpts opts)
    modelSpec <- tryEither $ instantiateModel (fiGeneralOpts opts) modelTemplate modelParams
    (hist, siteRed) <- loadHistogram (fiHistOpts opts) (mtBranchNames modelTemplate)
    l <- tryEither $ findQueryIndex (mtBranchNames modelTemplate) (fiQueryBranch opts)
    unless (hasFreeBranch l modelSpec) $
        throwIO $ RarecoalModelException ("model must have free branch " ++ show l) 
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
        return $ computeLogLikelihoodIO hist siteRed modelSpec' (mtBranchNames modelTemplate) k l t
    writeResult (fiEvalPath opts) allParamPairs allLikelihoods
    let ((minBranch, minTime), minLL) = maximumBy (\(_, ll1) (_, ll2) -> ll1 `compare` ll2) $
                                        zip allParamPairs allLikelihoods
    putStrLn $ "highest likelihood point:\nbranch " ++ show minBranch ++
               "\ntime " ++ show minTime ++ "\nlog-likelihood " ++ show minLL
  where
    hasFreeBranch queryBranch modelSpec =
        let e = mEvents modelSpec
            jIndices = concat [[k, l] | ModelEvent _ (Join k l) <- e]
        in  queryBranch `notElem` jIndices

findQueryIndex :: [ModelBranch] -> ModelBranch -> Either RarecoalException Int
findQueryIndex names branchName = case elemIndex branchName names of
    Nothing -> Left $ RarecoalModelException ("could not find branch name " ++ branchName)
    Just i -> return i

isEmptyBranch :: ModelSpec -> Int -> Double -> Bool
isEmptyBranch modelSpec l t = not $ null previousJoins
  where
    previousJoins = [j | ModelEvent t' j@(Join _ l') <- mEvents modelSpec, l' == l, t' < t]

getJoinTimes :: ModelSpec -> Double -> Double -> Double -> Int -> [Double]
getJoinTimes modelSpec deltaT maxT branchAge k =
    let allTimes = takeWhile (<=maxT) $ map ((+branchAge) . (*deltaT)) [1.0,2.0..]
        leaveTimes = [t | ModelEvent t (Join _ l) <- mEvents modelSpec, k == l]
    in  if null leaveTimes then allTimes else filter (<head leaveTimes) allTimes

computeLogLikelihoodIO :: RareAlleleHistogram -> Double -> ModelSpec -> [ModelBranch] ->
    Int -> Int -> Double -> IO Double
computeLogLikelihoodIO hist siteRed modelSpec modelBranchNames k l t = do
    let e = mEvents modelSpec
        newE = ModelEvent t (Join k l)
        modelSpec' = modelSpec {mEvents = newE : e}
    ll <- tryEither $ computeLogLikelihood modelSpec' hist modelBranchNames siteRed
    hPutStrLn stderr ("branch=" ++ show k ++ ", time=" ++ show t ++ ", ll=" ++ show ll)
    return ll

writeResult :: FilePath -> [(Int, Double)] -> [Double] -> IO ()
writeResult fp paramPairs allLikelihoods = do
    h <- openFile fp WriteMode
    hPutStrLn h "Branch\tTime\tLikelihood"
    let f (k, t) l = show k ++ "\t" ++ show t ++ "\t" ++ show l
        l_ = zipWith f paramPairs allLikelihoods
    mapM_ (hPutStrLn h) l_
    hClose h
