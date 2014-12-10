module RareAlleleHistogram (RareAlleleHistogram(..),
                            SitePattern(..),
                            addHistograms, filterMaxAf, setNrCalledSites,
                            loadHistogram, reduceIndices, combineIndices) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate, sortBy)
import Data.List.Split (splitOn)
import Control.Monad (liftM, when, (<=<))
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

loadHistogram :: Int -> Int64 -> FilePath -> Script RareAlleleHistogram
loadHistogram maxAf nrCalledSites path = do
    scriptIO $ infoM "rarecoal" "Loading histogram ... "
    hist <- scriptIO $ liftM read $ readFile path
    scriptIO $ infoM "rarecoal" "... Done loading"
    hoistEither $ (setNrCalledSites nrCalledSites <=< filterMaxAf maxAf <=< return) hist

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
        when (raGlobalMax hist) $ Left "Histogram cannot have global maxAF for this operation"
        let newNvec = selectFromList (raNVec hist) indices
            newBody = Map.mapKeysWith (+) (prunePatternIndices indices) (raCounts hist)
        return $ hist {raNVec = newNvec, raCounts = newBody}

combineIndices :: [Int] -> RareAlleleHistogram -> RareAlleleHistogram
combineIndices indices hist = 
    if null indices then
        hist
    else
        let newNvec = combineInPattern indices (raNVec hist)
            newBody = Map.mapKeysWith (+) transformPattern (raCounts hist)
        in  hist {raNVec = newNvec, raCounts = newBody}
  where
    transformPattern Higher = Higher
    transformPattern (Pattern pattern) =
        let newPattern = combineInPattern indices pattern
        in  if sum newPattern > raMaxAf hist then Higher else Pattern newPattern

combineInPattern :: [Int] -> [Int] -> [Int]
combineInPattern indices pattern =
    let newCount = sum $ map (pattern!!) indices
        restPattern = removeFromList pattern indices
    in  insertIntoList restPattern (head indices) newCount

prunePatternIndices :: [Int] -> SitePattern -> SitePattern
prunePatternIndices indices (Pattern pattern) = Pattern $ selectFromList pattern indices
prunePatternIndices _ Higher = Higher

prunePatternTotalFreq :: Int -> SitePattern -> SitePattern
prunePatternTotalFreq maxM (Pattern pattern) = if sum pattern > maxM then Higher else Pattern pattern
prunePatternTotalFreq _ Higher = Higher

selectFromList :: [a] -> [Int] -> [a]
selectFromList l [] = l
selectFromList l i = map (l!!) i

insertIntoList :: [a] -> Int -> a -> [a]
insertIntoList l i el =
    let (l1,l2) = splitAt i l in l1 ++ [el] ++ l2

removeFromList :: [a] -> [Int] -> [a]
removeFromList l indices =
    [el | (el, i) <- zip l [0..],  i `notElem` indices]

