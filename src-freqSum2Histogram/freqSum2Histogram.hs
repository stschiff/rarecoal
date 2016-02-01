import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..), setNrCalledSites, showHistogram)
import qualified Options.Applicative as OP
import qualified Pipes.Text.IO as PT
import qualified Pipes.Prelude as P
import Pipes.Attoparsec (parsed)
import Data.Int (Int64)
import Control.Error (scriptIO, runScript, tryRight, throwE)
import qualified Data.Text.IO as T
import Control.Foldl (purely, Fold(..))
import Rarecoal.FreqSumEntry (FreqSumEntry(..), parseFreqSumEntry)

data MyOpts = MyOpts [Int] Int [Int] Int64

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) (OP.progDesc "Tool to convert a freqSum file to to a histogram file as needed for rarecoal. Any pattern containing -1 in any selected column is excluded.")

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.option OP.auto (OP.short 'n' <> OP.long "nVec" <> OP.metavar "nVec" <> OP.help "comma-separated list of the number \
                                       \haplotypes for each population, surrounded by square brackets. Example: -n [100,100,100] for three population \
                                       \each with 100 haplotypes.")
                <*> OP.option OP.auto (OP.long "maxM"
                                       <> OP.short 'm'
                                       <> OP.metavar "INT"
                                       <> OP.value 10
                                       <> OP.showDefault
                                       <> OP.help "Specify the maximum allele count per population")
                <*> OP.option OP.auto (OP.long "popIndices"
                                       <> OP.short 'i'
                                       <> OP.metavar "LIST"
                                       <> OP.value []
                                       <> OP.showDefault
                                       <> OP.help "If given, select only the populations at the 0-based indices from the input freqSum file. Example: \
                                                  \-i [0,2,3]: select only the first, the third and the fourth population. Note that the number of samples, \
                                                  \specified using -n still must be given for all populations.")
                <*> OP.option OP.auto (OP.long "nrCalledSites" <> OP.short 'N'
                                       <> OP.help "set the total nr of called sites. This sets the number of non-variant sites (via the pattern \
                                                   \consisting of zeros only) such that the total number of sites matches the number given. This number is \
                                                   \important for estimating population sizes correctly, see the README for instructions.")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts nVec maxM popIndices nrCalledSites) = runScript $ do
    (patternHist, res) <- purely P.fold' buildPatternHist (parsed parseFreqSumEntry PT.stdin)
    case res of
        Left (err, _) -> throwE $ "Parsing error: " ++ show err
        Right () -> return ()
    let hist = RareAlleleHistogram (selectFromList popIndices nVec) 0 maxM [] patternHist
    hist' <- tryRight $ setNrCalledSites nrCalledSites hist
    outs <- tryRight $ showHistogram hist'
    scriptIO $ T.putStr outs
  where
    buildPatternHist = Fold step Map.empty id
    step m fse =
        case mkPat fse of
            Pattern pat -> if any (<0) pat then m else Map.insertWith (\_ v -> v + 1) (Pattern pat) 1 m
            Higher -> Map.insertWith (\_ v -> v + 1) Higher 1 m
    mkPat = makePattern . selectFromList popIndices . fsCounts
    makePattern vec = if isHigherAF vec then Higher else Pattern vec
    isHigherAF = (>maxM) . sum

selectFromList :: [Int] -> [a] -> [a]
selectFromList [] v = v
selectFromList i v = map (v !!) i
