module Find (runFind, FindOpt(..)) where

import Rarecoal.Core (ModelSpec(..), ModelEvent(..), EventType(..))
import Rarecoal.RareAlleleHistogram (loadHistogram, RareAlleleHistogram(..), SitePattern(..))
import Rarecoal.ModelTemplate (getModelSpec, ModelDesc, BranchSpec)

import Control.Error (Script, scriptIO, tryAssert, tryRight, err, tryJust)
import Data.List (sortBy, maximumBy, elemIndex)
import GHC.Conc (getNumCapabilities, setNumCapabilities, getNumProcessors)
import Logl (computeLikelihood)
import System.IO (stderr, hPutStrLn, openFile, IOMode(..), hClose)

data FindOpt = FindOpt {
    fiQueryBranch :: BranchSpec,
    -- fiBranchPopSize :: Double,
    fiEvalPath :: FilePath,
    fiBranchAge :: Double,
    fiDeltaTime :: Double,
    fiMaxTime :: Double,
    fiModelDesc :: ModelDesc,
    fiMinAf :: Int,
    fiMaxAf :: Int,
    fiConditionOn :: [Int],
    fiExcludePatterns :: [[Int]],
    fiIgnoreList :: [SitePattern],
    fiHistPath :: FilePath,
    fiNoShortcut :: Bool,
    fiNrThreads :: Int
}

runFind :: FindOpt -> Script ()
runFind opts = do
    nrProc <- scriptIO getNumProcessors
    if   fiNrThreads opts == 0
    then scriptIO $ setNumCapabilities nrProc
    else scriptIO $ setNumCapabilities (fiNrThreads opts)
    nrThreads <- scriptIO getNumCapabilities
    scriptIO $ err ("running on " ++ show nrThreads ++ " processors\n")
    hist <- loadHistogram (fiMinAf opts) (fiMaxAf opts) (fiConditionOn opts)
        (fiExcludePatterns opts) (fiHistPath opts)
    modelSpec <- getModelSpec (fiModelDesc opts) (raNames hist)
    l <- findQueryIndex (raNames hist) (fiQueryBranch opts)
    let modelSpec' =
            if fiBranchAge opts > 0.0
            then
                let events = ModelEvent 0.0 (SetFreeze l True) : mEvents modelSpec
                in  modelSpec {mEvents = events}
            else
                modelSpec
                -- let events = ModelEvent 0.0 (SetPopSize l (fiBranchPopSize opts)) : mEvents modelSpec
                -- in  modelSpec {mEvents = events}
    tryAssert ("model must have free branch " ++ show l) $ hasFreeBranch l modelSpec'
    let nrPops = length $ raNVec hist
        allParamPairs = do
            branch <- [0..(nrPops - 1)]
            False <- return $ branch == l
            False <- return $ isEmptyBranch modelSpec' branch (fiBranchAge opts)
            time <- getJoinTimes modelSpec' (fiDeltaTime opts) (fiMaxTime opts) (fiBranchAge opts)
                                  branch
            return (branch, time)
    allLikelihoods <- do
            let f (k, t) = computeLikelihoodIO hist modelSpec' k l t (fiNoShortcut opts)
            mapM f allParamPairs
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
    findQueryIndex _ (Left i) = return i
    findQueryIndex names (Right branchName) =
        tryJust ("could not find branch name " ++ branchName) $ elemIndex branchName names

isEmptyBranch :: ModelSpec -> Int -> Double -> Bool
isEmptyBranch modelSpec l t = not $ null previousJoins
  where
    previousJoins = [j | ModelEvent t' j@(Join _ l') <- mEvents modelSpec, l' == l, t' < t]

getJoinTimes :: ModelSpec -> Double -> Double -> Double -> Int -> [Double]
getJoinTimes modelSpec deltaT maxT branchAge k =
    let allTimes = takeWhile (<=maxT) $ map ((+branchAge) . (*deltaT)) [1.0,2.0..]
        leaveTimes = [t | ModelEvent t (Join _ l) <- mEvents modelSpec, k == l]
    in  if null leaveTimes then allTimes else filter (<head leaveTimes) allTimes

computeLikelihoodIO :: RareAlleleHistogram -> ModelSpec -> Int -> Int -> Double -> Bool ->
                       Script Double
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
    let f (k, t) l = show k ++ "\t" ++ show t ++ "\t" ++ show l
        l_ = zipWith f paramPairs allLikelihoods
    mapM_ (hPutStrLn h) l_
    hClose h
