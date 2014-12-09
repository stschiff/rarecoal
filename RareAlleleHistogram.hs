module RareAlleleHistogram (RareAlleleHistogram(..),
                            SitePattern(..),
                            addHistograms,
                            loadHistogram) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate, sortBy)
import Data.List.Split (splitOn)
import Control.Monad (liftM, when, (<=<), unless)
import System.Log.Logger (infoM)
import Data.Int (Int64)
import Control.Error.Script (Script, scriptIO)
import Control.Monad.Trans.Either (hoistEither)

data RareAlleleHistogram = RareAlleleHistogram {
    raNVec :: [Int],
    raMaxAf :: Int,
    raGlobalMax :: Bool,
    raCounts :: Map.Map SitePattern Int64
}

instance Show RareAlleleHistogram where
    show hist = 
        let head1 = "N=" ++ commaSep (raNVec hist)
            head2 = "MAX_M=" ++ show (raMaxAf hist)
            head3 = "GLOBAL_MAX=" ++ show (raGlobalMax hist)
            sorted = sortBy (\(_, v1) (_, v2)  -> compare v2 v1) $ Map.toList (raCounts hist)
            body = [show k ++ " " ++ show v | (k, v) <- sorted]
        in  unlines (head1:head2:head3:body)

instance Read RareAlleleHistogram where
    readsPrec _ s =
        let lines_ = lines s
            nVec = map read $ splitOn "," $ drop 2 (head lines_)
            maxM = read $ drop 6 (lines_!!1)
            max_type = read $ drop 11 (lines_!!2)
            body = map readBodyLine (filter (/="") $ drop 3 lines_)
        in  [(RareAlleleHistogram nVec maxM max_type (Map.fromList body), "")]
        where
            readBodyLine :: String -> (SitePattern, Int64)
            readBodyLine line =
                let fields = words line
                in  (read $ head fields, read $ last fields)

commaSep :: Show a => [a] -> String
commaSep = intercalate "," . map show

addHistograms :: RareAlleleHistogram -> RareAlleleHistogram -> Either String RareAlleleHistogram
addHistograms hist1 hist2 = do
    when (raNVec hist1 /= raNVec hist2 || raMaxAf hist1 /= raMaxAf hist2 ||
                      raGlobalMax hist1 /= raGlobalMax hist2) $ Left "histograms not compatible"
    return $ hist1 {raCounts = Map.unionWith (+) (raCounts hist1) (raCounts hist2)}
        
data SitePattern = Pattern [Int] | Higher deriving (Eq, Ord)
instance Show SitePattern where
    show (Pattern nVec) = commaSep nVec
    show Higher = "HIGHER"

instance Read SitePattern where
    readsPrec _ s =
        let val = if s == "HIGHER" then Higher else Pattern $ map read . splitOn "," $ s
        in  [(val, "")]

loadHistogram :: [Int] -> Int -> Int64 -> FilePath -> Script RareAlleleHistogram
loadHistogram indices maxAf nrCalledSites path = do
    scriptIO $ infoM "rarecoal" "Loading histogram ... "
    hist <- scriptIO $ liftM read $ readFile path
    scriptIO $ infoM "rarecoal" "... Done loading"
    hoistEither $ (setNrCalledSites nrCalledSites <=< filterMaxAf maxAf <=< reduceIndices indices <=< return) hist

setNrCalledSites :: Int64 -> RareAlleleHistogram -> Either String RareAlleleHistogram
setNrCalledSites nrCalledSites hist = do
    let zeroKey = Pattern $ replicate (length $ raNVec hist) 0
        sum_ = Map.foldr (+) 0 $ raCounts hist
        add_ = nrCalledSites - sum_
    when (add_ < 0) $ Left "Illegal nrCalledSites" 
    let newHistBody = Map.adjust (+add_) zeroKey (raCounts hist)
    return $ hist {raCounts = newHistBody}

filterMaxAf :: Int -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterMaxAf maxAf hist = do
    when (maxAf > raMaxAf hist) $ Left "maximum AF exceeds AF in histogram"
    if maxAf == raMaxAf hist then return hist else do
        let newBody = Map.mapKeysWith (+) (prunePatternTotalFreq maxAf) (raCounts hist)
        return $ hist {raMaxAf = maxAf, raCounts = newBody, raGlobalMax = True}

reduceIndices :: [Int] -> RareAlleleHistogram -> Either String RareAlleleHistogram
reduceIndices indices hist =
    if null indices || indices == [0..(length $ raNVec hist)] then return hist else do
        unless (raGlobalMax hist) $ Left "Histogram cannot have global maxAF for this operation"
        let newNvec = selectFromList (raNVec hist) indices
            newBody = Map.mapKeysWith (+) (prunePatternIndices indices) (raCounts hist)
        return $ hist {raNVec = newNvec, raCounts = newBody}

prunePatternIndices :: [Int] -> SitePattern -> SitePattern
prunePatternIndices indices (Pattern pattern) = Pattern $ selectFromList pattern indices
prunePatternIndices _ Higher = Higher

prunePatternTotalFreq :: Int -> SitePattern -> SitePattern
prunePatternTotalFreq maxM (Pattern pattern) = if sum pattern > maxM then Higher else Pattern pattern
prunePatternTotalFreq _ Higher = Higher

selectFromList :: [a] -> [Int] -> [a]
selectFromList l [] = l
selectFromList l i = map (l!!) i

