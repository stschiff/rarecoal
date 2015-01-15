module Find (runFind, FindOpt(..)) where

import ModelTemplate (getModelSpec)
import RareAlleleHistogram (loadHistogram, RareAlleleHistogram(..))
import Control.Monad.Trans.Either (hoistEither)
import Control.Error.Script (Script, scriptIO)
import Control.Error.Safe (tryAssert)
import Control.Monad (forM_)
import Logl (computeLikelihood)
import Core (ModelSpec(..), ModelEvent(..), EventType(..))
import Data.Int (Int64)
import System.IO (stderr, hPutStrLn)

data FindOpt = FindOpt {
    fiQueryIndex :: Int,
    fiDeltaTime :: Double,
    fiMaxTime :: Double,
    fiTheta :: Double,
    fiTemplatePath :: FilePath,
    fiParams :: [Double],
    fiModelEvents :: [ModelEvent],
    fiMaxAf :: Int,
    fiNrCalledSites :: Int64,
    fiIndices :: [Int],
    fiHistPath :: FilePath
}

runFind :: FindOpt -> Script ()
runFind opts = do
    modelSpec <- getModelSpec (fiTemplatePath opts) (fiTheta opts) (fiParams opts) (fiModelEvents opts)
    let l = fiQueryIndex opts
    tryAssert ("model must have free branch " ++ show (fiQueryIndex opts)) $ hasFreeBranch l modelSpec
    hist <- loadHistogram (fiIndices opts) (fiMaxAf opts) (fiNrCalledSites opts) (fiHistPath opts)
    let nrPops = length $ raNVec hist
        targetBranches = [branch | branch <- [0..nrPops-1], branch /= l]
        allJoinTimes = [getJoinTimes modelSpec (fiDeltaTime opts) (fiMaxTime opts) k | k <- targetBranches]
        allParamPairs = concat $ zipWith (\k times -> [(k, t) | t <- times]) targetBranches allJoinTimes
    allLikelihoods <- mapM (\(k, t) -> computeLikelihoodIO hist modelSpec k l t) allParamPairs
    scriptIO $ writeResult allParamPairs allLikelihoods
  where
    hasFreeBranch queryBranch modelSpec =
        let e = mEvents modelSpec
            jIndices = concat [[k, l] | ModelEvent _ (Join k l) <- e]
        in  all (/=queryBranch) jIndices

getJoinTimes :: ModelSpec -> Double -> Double -> Int -> [Double]
getJoinTimes modelSpec deltaT maxT k =
    let allTimes = takeWhile (<=maxT) $ map ((*deltaT) . fromIntegral) [1..]
        leaveTimes = [t | ModelEvent t (Join _ l) <- mEvents modelSpec, k == l]
    in  if leaveTimes == [] then allTimes else filter (<head leaveTimes) allTimes

computeLikelihoodIO :: RareAlleleHistogram -> ModelSpec -> Int -> Int -> Double -> Script Double
computeLikelihoodIO hist modelSpec k l t = do
    let e = mEvents modelSpec
        newE = ModelEvent t (Join k l)
        modelSpec' = modelSpec {mEvents = (newE:e)}
    ll <- hoistEither $ computeLikelihood modelSpec' hist
    scriptIO $ hPutStrLn stderr ("branch=" ++ show k ++ ", time=" ++ show t ++ ", ll=" ++ show ll)
    return ll

writeResult :: [(Int, Double)] -> [Double] -> IO ()
writeResult paramPairs allLikelihoods = do
    putStrLn "Branch\tTime\tLikelihood"
    let l_ = zipWith (\(k, t) l -> show k ++ "\t" ++ show t ++ "\t" ++ show l) paramPairs allLikelihoods
    forM_ l_ putStrLn
