import Pipes (runEffect, (>->), Producer)
import qualified Pipes.Prelude as P
import qualified Pipes.Text as PT
import qualified Pipes.Text.IO as PT
import qualified Data.Text as T
import Pipes.Group (folds)
import Control.Foldl (purely, mconcat)
import Control.Applicative ((<|>))
import Control.Error (runScript, tryRight, Script)
import Control.Lens (view)
import Data.Attoparsec.Text (Parser, char, decimal, letter, notInClass, parseOnly, sepBy1, takeWhile)
import Prelude hiding (takeWhile, mconcat)
import Control.Monad (replicateM_)
import Rarecoal.FreqSumEntry (FreqSumEntry(..))

data VCFentry = VCFentry T.Text Int Char Char [(Char,Char)]

main :: IO ()
main = runScript . runEffect $ lineProducer >-> P.filter (\l -> T.head l /= '#') >->
                               P.mapM (tryRight . processVCFline) >-> PT.stdout

lineProducer :: Producer T.Text Script ()
lineProducer = purely folds mconcat $ view PT.lines PT.stdin

processVCFline :: T.Text -> Either String T.Text
processVCFline line = do
    VCFentry chrom pos ref alt genotypes <- parseOnly parseVCFentry line
    let gens = [[g1, g2] | (g1, g2) <- genotypes]
    let counts = [length $ filter (=='1') c | c <- gens]
        fs = FreqSumEntry (T.unpack chrom) pos ref alt counts
    return . flip T.snoc '\n' . T.pack . show $ fs

parseVCFentry :: Parser VCFentry
parseVCFentry = do
    chrom <- word
    char '\t'
    pos <- decimal
    char '\t'
    _ <- word
    char '\t'
    ref <- letter
    char '\t'
    alt <- letter
    char '\t'
    replicateM_ 4 (word >> char '\t')
    genotypes <- genotype `sepBy1` (char '\t')
    return $ VCFentry chrom pos ref alt genotypes

word :: Parser T.Text
word = takeWhile (notInClass "\r\t\n ")

genotype :: Parser (Char, Char)
genotype = do
    gen1 <- (char '0' <|> char '1')
    (char '/' <|> char '|')
    gen2 <- (char '0' <|> char '1')
    word
    return (gen1, gen2)

