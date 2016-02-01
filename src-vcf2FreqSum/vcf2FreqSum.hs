{-# LANGUAGE OverloadedStrings #-}

import Rarecoal.FreqSumEntry (FreqSumEntry(..))

import Control.Applicative ((<|>))
import Control.Error (runScript, tryAssert, errLn)
import Control.Monad (void)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Options.Applicative as OP
import Pipes (runEffect, (>->), lift, Effect, for, next)
import Pipes.Attoparsec (parsed)
import Pipes.Prelude (foldM')
import Pipes.Text.IO as PT

data VCFentry = VCFheader T.Text | VCFentry T.Text Int T.Text T.Text [(Char,Char)] deriving (Show)

main :: IO ()
main = OP.execParser opts >> run
  where
    opts = OP.info (OP.helper <*> pure ()) (OP.progDesc "convert a multi-sample VCF into a freqSum file")

run :: IO () 
run = do
    let vcfProd = parsed vcfParser PT.stdin
    ((), res) <- foldM' processVCFentry (return 0) (const (return ())) vcfProd
    case res of
        Left (e, restProd) -> do
            (errLn . show) e
            Right (l, _) <- next restProd
            T.putStrLn l
        Right () -> return ()

processVCFentry :: Int -> VCFentry -> IO Int
processVCFentry lastPos (VCFheader s) = return 0
processVCFentry lastPos (VCFentry chrom pos ref alt genotypes) = do
    if lastPos == pos || T.length ref > 1 || T.length alt > 1 then
        return pos
    else do
        let gens = [[g1, g2] | (g1, g2) <- genotypes]
        let counts = [if any (=='.') c then -1 else length $ filter (=='1') c | c <- gens]
            fs = FreqSumEntry (T.unpack chrom) pos (T.head ref) (T.head alt) counts
        print fs
        return pos

vcfParser :: A.Parser VCFentry
vcfParser = parseHeader <|> parseVCFentry
  where
    parseHeader = do
        void (A.char '#')
        s <- A.takeTill A.isEndOfLine
        void A.endOfLine
        return (VCFheader s)
    parseVCFentry = do
        chrom <- word
        void tab
        pos <- A.decimal
        void tab
        void word
        void tab
        ref <- word
        void tab
        alt <- word
        void tab
        void $ A.count 4 (word >> tab)
        genotypes <- genotype `A.sepBy1` tab
        void A.endOfLine
        return $ VCFentry chrom pos ref alt genotypes
    word = A.takeTill (\a -> a `elem` ['\r', '\t', '\n', ' '])
    tab = A.char '\t'

genotype :: A.Parser (Char, Char)
genotype = do
    gen1 <- (A.char '0' <|> A.char '1' <|> A.char '.')
    void (A.char '/' <|> A.char '|')
    gen2 <- (A.char '0' <|> A.char '1' <|> A.char '.')
    _ <- A.takeTill (\a -> (a `elem` ['\r', '\t', '\n', ' ']))
    return (gen1, gen2)
