module Find (runFind, FindOpt(..)) where

import ModelTemplate (getModelSpec)
import Rarecoal.RareAlleleHistogram (loadHistogram, RareAlleleHistogram(..), SitePattern(..))
import Control.Error (Script, scriptIO, tryAssert, tryRight)
import Logl (computeLikelihood)
import Core (ModelSpec(..), ModelEvent(..), EventType(..))
import Data.Int (Int64)
import System.IO (stderr, hPutStrLn, openFile, IOMode(..), hClose)
import Data.List (sortBy)

data FindOpt = FindOpt {
    fiQueryIndex :: Int,
    fiEvalPath :: FilePath,
    fiBranchAge :: Double,
    fiDeltaTime :: Double,
    fiMaxTime :: Double,
    fiTheta :: Double,
    fiTemplatePath :: FilePath,
    fiParamsFile :: FilePath,
    fiParams :: [Double],
    fiModelEvents :: [ModelEvent],
    fiMinAf :: Int,
    fiMaxAf :: Int,
    fiConditionOn :: [Int],
    fiNrCalledSites :: Int64,
    fiLinGen :: Int,
    fiIndices :: [Int],
    fiIgnoreList :: [SitePattern],
    fiHistPath :: FilePath,
    fiNoShortcut :: Bool
}

runFind :: FindOpt -> Script ()
runFind opts = do
    modelSpec' <- getModelSpec (fiTemplatePath opts) (fiTheta opts) (fiParamsFile opts) (fiParams opts) (fiModelEvents opts) (fiLinGen opts)
    let l = fiQueryIndex opts
        modelSpec = if fiBranchAge opts > 0.0 then
                let events' = mEvents modelSpec'
                    events = ModelEvent 0.0 (SetFreeze l True) :
                             ModelEvent (fiBranchAge opts) (SetFreeze l False) : events'
                in  modelSpec' {mEvents = events}
            else
                modelSpec'
    tryAssert ("model must have free branch " ++ show (fiQueryIndex opts)) $ hasFreeBranch l modelSpec
    hist <- loadHistogram (fiIndices opts) (fiMinAf opts) (fiMaxAf opts) (fiConditionOn opts) (fiNrCalledSites opts) (fiHistPath opts)
    let nrPops = length $ raNVec hist
        targetBranches = [branch | branch <- [0..nrPops-1], branch /= l]
        allJoinTimes = [getJoinTimes modelSpec (fiDeltaTime opts) (fiMaxTime opts) (fiBranchAge opts) k | k <- targetBranches]
        allParamPairs = concat $ zipWith (\k times -> [(k, t) | t <- times]) targetBranches allJoinTimes
    allLikelihoods <- mapM (\(k, t) -> computeLikelihoodIO hist modelSpec k l t (fiNoShortcut opts)) allParamPairs
    scriptIO $ writeResult (fiEvalPath opts) allParamPairs allLikelihoods
    let ((minBranch, minTime), minLL) = last . sortBy (\(_, ll1) (_, ll2) -> ll1 `compare` ll2) $
                                        zip allParamPairs allLikelihoods
    scriptIO . putStrLn $ "highest likelihood point:\nbranch " ++ show minBranch ++
                          "\ntime " ++ show minTime ++ "\nlog-likelihood " ++ show minLL
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

computeLikelihoodIO :: RareAlleleHistogram -> ModelSpec -> Int -> Int -> Double -> Bool -> Script Double 
computeLikelihoodIO hist modelSpec k l t noShortcut = do
    let e = mEvents modelSpec
        newE = ModelEvent t (Join k l)
        modelSpec' = modelSpec {mEvents = newE : e}
    ll <- tryRight $ computeLikelihood modelSpec' hist noShortcut
    scriptIO $ hPutStrLn stderr ("branch=" ++ show k ++ ", time=" ++ show t ++ ", ll=" ++ show ll)
    return ll

writeResult :: FilePath -> [(Int, Double)] -> [Double] -> IO ()
writeResult fp paramPairs allLikelihoods = do
    h <- openFile fp WriteMode
    hPutStrLn h "Branch\tTime\tLikelihood"
    let l_ = zipWith (\(k, t) l -> show k ++ "\t" ++ show t ++ "\t" ++ show l) paramPairs allLikelihoods
    mapM_ (hPutStrLn h) l_
    hClose h
