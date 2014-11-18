module RareAlleleHistogram (RareAlleleHistogram(..),
                            SitePattern(..),
                            addHistograms,
                            filterHistogram,
                            loadHistogram,
                            InputSpec(..)) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate, sortBy)
import Data.List.Split (splitOn)
import Control.Exception (assert)
import Debug.Trace (trace)
import Control.Monad (liftM)

data InputSpec = InputSpec {
    isIndices :: [Int],
    isMaxAf :: Int,
    isNrCalledSites :: Int,
    isPath :: FilePath
}

data RareAlleleHistogram = RareAlleleHistogram {
    raNVec :: [Int],
    raMaxAf :: Int,
    raIsCompleteMax :: Bool,
    raCounts :: Map.Map SitePattern Int
}

instance Show RareAlleleHistogram where
    show hist = 
        let head1 = "N=" ++ commaSep (raNVec hist)
            head2 = "MAX_M=" ++ show (raMaxAf hist)
            sorted = sortBy (\(_, v1) (_, v2)  -> compare v2 v1) $ Map.toList (raCounts hist)
            body = [show k ++ " " ++ show v | (k, v) <- sorted]
        in  unlines (head1:head2:body)

instance Read RareAlleleHistogram where
    readsPrec _ s =
        let lines_ = lines s
            nVec = map read $ splitOn "," $ drop 2 (lines_!!0)
            maxM = read $ drop 6 (lines_!!1)
            body = map readBodyLine $ (filter (/="") $ drop 2 lines_)
        in  [(RareAlleleHistogram nVec maxM False (Map.fromList body), "")]
        where
            readBodyLine :: String -> (SitePattern, Int)
            readBodyLine line =
                let fields = words line
                in  (read $ head fields, read $ last fields)

commaSep :: Show a => [a] -> String
commaSep = intercalate "," . map show

addHistograms :: RareAlleleHistogram -> RareAlleleHistogram -> RareAlleleHistogram
addHistograms hist1 hist2 = 
    let ass = assert (raNVec hist1 == raNVec hist2 && raMaxAf hist1 == raMaxAf hist2)
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
loadHistogram (InputSpec indices maxAf isNrCalledSites path) = do
    hist <- liftM read $ readFile path
    return $ filterHistogram maxAf indices isNrCalledSites hist

filterHistogram :: Int -> [Int] -> Int -> RareAlleleHistogram -> RareAlleleHistogram
filterHistogram maxAf indices nrCalledSites (RareAlleleHistogram histNVec histMaxAf False histBody) =
    let newNvec = selectFromList histNVec indices
        newBody = Map.mapKeysWith (+) f histBody
        ass = assert $ histMaxAf >= maxAf
    in  ass $ setNrCalledSites nrCalledSites (RareAlleleHistogram newNvec maxAf True newBody)
  where
    f = prunePatternTotalFreq maxAf . prunePatternIndices indices

setNrCalledSites :: Int -> RareAlleleHistogram -> RareAlleleHistogram
setNrCalledSites nrCalledSites hist =
    let zeroKey = Pattern $ replicate (length $ raNVec hist) 0
        sum_ = Map.foldr (+) 0 $ raCounts hist
        add_ = nrCalledSites - sum_
        ass = assert $ add_ > 0
        newHistBody = ass $ Map.adjust (+add_) zeroKey (raCounts hist)
    in  hist {raCounts = newHistBody}

prunePatternIndices :: [Int] -> SitePattern -> SitePattern
prunePatternIndices indices (Pattern pattern) = Pattern $ selectFromList pattern indices
prunePatternIndices indices Higher = Higher

prunePatternTotalFreq :: Int -> SitePattern -> SitePattern
prunePatternTotalFreq maxM (Pattern pattern) = if sum pattern > maxM then Higher else Pattern pattern
prunePatternTotalFreq maxM Higher = Higher

selectFromList :: [a] -> [Int] -> [a]
selectFromList l [] = l
selectFromList l i = map (l!!) i

