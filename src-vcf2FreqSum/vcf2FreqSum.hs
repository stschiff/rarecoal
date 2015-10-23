{-# LANGUAGE OverloadedStrings #-}

import Rarecoal.FreqSumEntry (FreqSumEntry(..))

import Turtle
import Control.Error (runScript, tryAssert)
import qualified Data.Text as T

data VCFentry = VCFentry Text Int Text Text [(Char,Char)]

main :: IO ()
main = foldIO (grep (prefix (noneOf "#")) stdin) foldLines

foldLines :: FoldM IO Text ()
foldLines = FoldM processLine initial extract
  where
    initial = return 0
    extract = const (return ())

processLine :: Int -> Text -> IO Int
processLine lastPos line = runScript $ do
    let parseResult = match vcfPattern line
        msg = format ("vcf parse error for line "%s) line
    tryAssert (T.unpack msg) $ length parseResult == 1
    let [VCFentry chrom pos ref alt genotypes] = parseResult
    if lastPos == pos || T.length ref > 1 || T.length alt > 1 then
        return pos
    else do
        let gens = [[g1, g2] | (g1, g2) <- genotypes]
        let counts = [length $ filter (=='1') c | c <- gens]
            fs = FreqSumEntry (T.unpack chrom) pos (T.head ref) (T.head alt) counts
        echo . T.pack . show $ fs
        return pos

vcfPattern :: Pattern VCFentry
vcfPattern = do
    chrom <- word
    skip tab
    pos <- decimal
    skip tab
    skip word
    skip tab
    ref <- word
    skip tab
    alt <- word
    skip tab
    skip $ count 4 (word >> tab)
    genotypes <- genotype `sepBy1` tab
    return $ VCFentry chrom pos ref alt genotypes

word :: Pattern Text
word = plus $ noneOf "\r\t\n "

genotype :: Pattern (Char, Char)
genotype = do
    gen1 <- (char '0' <|> char '1' <|> char '.')
    skip (char '/' <|> char '|')
    gen2 <- (char '0' <|> char '1' <|> char '.')
    skip $ star (noneOf "\r\t\n ")
    return (gen1, gen2)
