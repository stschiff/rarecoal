module FreqSumEntry (FreqSumEntry(..)) where

import Data.List (intercalate)

data FreqSumEntry = FreqSumEntry {
    fsChrom  :: String,
    fsPos    :: Int,
    fsRef    :: Char,
    fsAlt    :: Char,
    fsCounts :: [Int]
}

instance Show FreqSumEntry where
    show (FreqSumEntry c p r a counts) = intercalate "\t" $ [c, show p, [r], [a]] ++ map show counts

instance Read FreqSumEntry where
    readsPrec _ line =
        let (c:pStr:rStr:aStr:countsStr) = words line
        in  [(FreqSumEntry c (read pStr) (head rStr) (head aStr) (map read countsStr), "")]

instance Eq FreqSumEntry where
    fs1 == fs2 = fsPos fs1 == fsPos fs2

instance Ord FreqSumEntry where
    compare fs1 fs2 = compare (fsPos fs1) (fsPos fs2)
