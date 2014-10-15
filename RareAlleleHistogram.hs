module RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..)) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate, sortBy)
import Data.List.Split (splitOn)

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

-- instance Read RareAlleleHistogram where
--     readsPrec _ s =
        
data SitePattern = Pattern [Int] | Higher deriving (Eq, Ord)
instance Show SitePattern where
    show (Pattern nVec) = commaSep nVec
    show Higher = "HIGHER"

instance Read SitePattern where
    readsPrec _ s =
        let val = if s == "HIGHER" then Higher else Pattern $ map read . splitOn "," $ s
        in  [(val, "")]
        