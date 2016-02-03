module Rarecoal.FreqSumEntry (parseFreqSumEntry, FreqSumEntry(..)) where

import Data.List (intercalate)
import qualified Data.Attoparsec.Text as A
import Data.Text (unpack)

data FreqSumEntry = FreqSumEntry {
    fsChrom  :: String,
    fsPos    :: Int,
    fsRef    :: Char,
    fsAlt    :: Char,
    fsCounts :: [Int]
}

instance Show FreqSumEntry where
    show (FreqSumEntry chrom pos ref alt counts) =
        intercalate "\t" [chrom, show pos, [ref], [alt], intercalate "\t" . map show $ counts]

parseFreqSumEntry :: A.Parser FreqSumEntry
parseFreqSumEntry = FreqSumEntry <$> word <* A.skipSpace <*> A.decimal <* A.skipSpace <*>
                                     A.letter <* A.skipSpace <*> A.letter <* A.skipSpace <*>
                                     counts <* A.endOfLine
  where
    word = unpack <$> A.takeWhile1 (A.notInClass "\n\t\r")
    counts = (A.signed A.decimal) `A.sepBy` A.char '\t'
