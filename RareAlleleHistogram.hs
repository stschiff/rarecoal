module RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..), addHistograms) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate, sortBy)
import Data.List.Split (splitOn)
import Control.Exception (assert)
import Debug.Trace (trace)

data RareAlleleHistogram = RareAlleleHistogram {
    raNVec :: [Int],
    raMaxAf :: Int,
    raCounts :: Map.Map SitePattern Int
}

commaSep :: [Int] -> String
commaSep = intercalate "," . map show

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
        in  [(RareAlleleHistogram nVec maxM (Map.fromList body), "")]
        where
            readBodyLine :: String -> (SitePattern, Int)
            readBodyLine line =
                let fields = words line
                in  (read $ head fields, read $ last fields)

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
        