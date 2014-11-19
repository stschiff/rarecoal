module RareAlleleHistogram (RareAlleleHistogram(..),
                            SitePattern(..),
                            addHistograms,
                            loadHistogram,
                            InputSpec(..)) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate, sortBy)
import Data.List.Split (splitOn)
import Control.Exception (assert)
import Debug.Trace (trace)
import Control.Monad (liftM, when)
import System.Log.Logger (infoM)
import Data.Int (Int64)

data InputSpec = InputSpec {
    isIndices :: [Int],
    isMaxAf :: Int,
    isNrCalledSites :: Int64,
    isPath :: FilePath
}

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
            nVec = map read $ splitOn "," $ drop 2 (lines_!!0)
            maxM = read $ drop 6 (lines_!!1)
            max_type = read $ drop 11 (lines_!!2)
            body = map readBodyLine $ (filter (/="") $ drop 3 lines_)
        in  [(RareAlleleHistogram nVec maxM max_type (Map.fromList body), "")]
        where
            readBodyLine :: String -> (SitePattern, Int64)
            readBodyLine line =
                let fields = words line
                in  (read $ head fields, read $ last fields)

commaSep :: Show a => [a] -> String
commaSep = intercalate "," . map show

addHistograms :: RareAlleleHistogram -> RareAlleleHistogram -> RareAlleleHistogram
addHistograms hist1 hist2 = 
    let ass = assert (raNVec hist1 == raNVec hist2 && raMaxAf hist1 == raMaxAf hist2 &&
                      raGlobalMax hist1 == raGlobalMax hist2)
    in  ass $ hist1 {raCounts = Map.unionWith (+) (raCounts hist1) (raCounts hist2)}
        
data SitePattern = Pattern [Int] | Higher deriving (Eq, Ord)
instance Show SitePattern where
    show (Pattern nVec) = commaSep nVec
    show Higher = "HIGHER"

instance Read SitePattern where
    readsPrec _ s =
        let val = if s == "HIGHER" then Higher else Pattern $ map read . splitOn "," $ s
        in  [(val, "")]

loadHistogram :: InputSpec -> IO RareAlleleHistogram
loadHistogram (InputSpec indices maxAf nrCalledSites path) = do
    infoM "rarecoal" "Loading histogram ... "
    hist <- liftM read $ readFile path
    infoM "rarecoal" "... Done loading"
    return $ (setNrCalledSites nrCalledSites . filterMaxAf maxAf . reduceIndices indices) hist

setNrCalledSites :: Int64 -> RareAlleleHistogram -> RareAlleleHistogram
setNrCalledSites nrCalledSites hist =
    let zeroKey = Pattern $ replicate (length $ raNVec hist) 0
        sum_ = Map.foldr (+) 0 $ raCounts hist
        add_ = nrCalledSites - sum_
        ass = assert $ add_ >= 0
        newHistBody = ass $ Map.adjust (+add_) zeroKey (raCounts hist)
    in  hist {raCounts = newHistBody}

filterMaxAf :: Int -> RareAlleleHistogram -> RareAlleleHistogram
filterMaxAf maxAf hist =
    if maxAf == raMaxAf hist then hist else
        let ass = assert $ maxAf <= raMaxAf hist
            newBody = Map.mapKeysWith (+) (prunePatternTotalFreq maxAf) (raCounts hist)
        in  ass $ hist {raMaxAf = maxAf, raCounts = newBody, raGlobalMax = True}

reduceIndices :: [Int] -> RareAlleleHistogram -> RareAlleleHistogram
reduceIndices indices hist =
    if null indices || indices == [0..(length $ raNVec hist)] then hist else
        let ass = assert $ raGlobalMax hist == False
            newNvec = selectFromList (raNVec hist) indices
            newBody = Map.mapKeysWith (+) (prunePatternIndices indices) (raCounts hist)
        in  ass $ hist {raNVec = newNvec, raCounts = newBody}

prunePatternIndices :: [Int] -> SitePattern -> SitePattern
prunePatternIndices indices (Pattern pattern) = Pattern $ selectFromList pattern indices
prunePatternIndices indices Higher = Higher

prunePatternTotalFreq :: Int -> SitePattern -> SitePattern
prunePatternTotalFreq maxM (Pattern pattern) = if sum pattern > maxM then Higher else Pattern pattern
prunePatternTotalFreq maxM Higher = Higher

selectFromList :: [a] -> [Int] -> [a]
selectFromList l [] = l
selectFromList l i = map (l!!) i

