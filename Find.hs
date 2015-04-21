module Find (runFind, FindOpt(..)) where

import ModelTemplate (getModelSpec)
import RareAlleleHistogram (loadHistogram, RareAlleleHistogram(..), SitePattern(..))
import Control.Monad.Trans.Either (hoistEither)
import Control.Error.Script (Script, scriptIO)
import Control.Error.Safe (tryAssert)
import Control.Monad (forM_, (<=<))
import Logl (computeLikelihood)
import Core (ModelSpec(..), ModelEvent(..), EventType(..))
import Data.Int (Int64)
import System.IO (stderr, hPutStrLn)

data FindOpt = FindOpt {
    fiQueryIndex :: Int,
    fiBranchAge :: Double,
    fiDeltaTime :: Double,
    fiMaxTime :: Double,
    fiTheta :: Double,
    fiTemplatePath :: FilePath,
    fiParams :: [Double],
    fiModelEvents :: [ModelEvent],
    fiMinAf :: Int,
    fiMaxAf :: Int,
    fiConditionOn :: [Int],
    fiNrCalledSites :: Int64,
    fiLinGen :: Int,
    fiIndices :: [Int],
    fiIgnoreList :: [SitePattern],
    fiHistPath :: FilePath
}

runFind :: FindOpt -> Script ()
runFind opts = do
    modelSpec' <- getModelSpec (fiTemplatePath opts) (fiTheta opts) (fiParams opts) (fiModelEvents opts) (fiLinGen opts)
    let l = fiQueryIndex opts
        modelSpec = if fiBranchAge opts > 0.0 then
            let events' = mEvents modelSpec'
                events = ModelEvent 0.0 (SetFreeze l True) : ModelEvent (fiBranchAge opts) (SetFreeze l False) : events'
            in  modelSpec' {mEvents = events'}
        else
            modelSpec'
    tryAssert ("model must have free branch " ++ show (fiQueryIndex opts)) $ hasFreeBranch l modelSpec
    hist <- loadHistogram (fiIndices opts) (fiMinAf opts) (fiMaxAf opts) (fiConditionOn opts) (fiNrCalledSites opts) (fiHistPath opts)
    let nrPops = length $ raNVec hist
        targetBranches = [branch | branch <- [0..nrPops-1], branch /= l]
        allJoinTimes = [getJoinTimes modelSpec (fiDeltaTime opts) (fiMaxTime opts) (fiBranchAge opts) k | k <- targetBranches]
        allParamPairs = concat $ zipWith (\k times -> [(k, t) | t <- times]) targetBranches allJoinTimes
    allLikelihoods <- mapM (\(k, t) -> computeLikelihoodIO hist modelSpec k l t) allParamPairs
    scriptIO $ writeResult allParamPairs allLikelihoods
  where
    hasFreeBranch queryBranch modelSpec =
        let e = mEvents modelSpec
            jIndices = concat [[k, l] | ModelEvent _ (Join k l) <- e]
        in  queryBranch `notElem` jIndices

getJoinTimes :: ModelSpec -> Double -> Double -> Double -> Int -> [Double]
getJoinTimes modelSpec deltaT maxT branchAge k =
    let allTimes = takeWhile (<=maxT) $ map ((+branchAge) . (*deltaT) . fromIntegral) [1..]
        leaveTimes = [t | ModelEvent t (Join _ l) <- mEvents modelSpec, k == l]
    in  if null leaveTimes then allTimes else filter (<head leaveTimes) allTimes

computeLikelihoodIO :: RareAlleleHistogram -> ModelSpec -> Int -> Int -> Double -> Script Double
computeLikelihoodIO hist modelSpec k l t = do
    let e = mEvents modelSpec
        newE = ModelEvent t (Join k l)
        modelSpec' = modelSpec {mEvents = newE : e}
    ll <- hoistEither $ computeLikelihood modelSpec' hist
    scriptIO $ hPutStrLn stderr ("branch=" ++ show k ++ ", time=" ++ show t ++ ", ll=" ++ show ll)
    return ll

writeResult :: [(Int, Double)] -> [Double] -> IO ()
writeResult paramPairs allLikelihoods = do
    putStrLn "Branch\tTime\tLikelihood"
    let l_ = zipWith (\(k, t) l -> show k ++ "\t" ++ show t ++ "\t" ++ show l) paramPairs allLikelihoods
    forM_ l_ putStrLn
