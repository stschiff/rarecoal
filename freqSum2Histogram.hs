import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..), setNrCalledSites, showHistogram)
import qualified Options.Applicative as OP
import qualified Pipes.Text.IO as PT
import qualified Pipes.Prelude as P
import Pipes.Attoparsec (parsed)
import Data.Int (Int64)
import Control.Error (scriptIO, runScript, tryRight, left)
import qualified Data.Text.IO as T
import Control.Foldl (purely, Fold(..))
import FreqSumEntry (FreqSumEntry(..), parseFreqSumEntry)

data MyOpts = MyOpts [Int] Int [Int] Int64 Bool

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) mempty

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.option OP.auto (OP.short 'n' <> OP.long "nVec" <> OP.metavar "nVec")
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
                                       <> OP.help "Specify the exact populations for the histogram")
                <*> OP.option OP.auto (OP.long "nrCalledSites" <> OP.short 'N'
                                       <> OP.help "set the total nr of called sites")
                <*> OP.switch (OP.long "globalMax" <> OP.short 'g' <> OP.help "constrain global allele frequency")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts nVec maxM popIndices nrCalledSites globalMax) = runScript $ do
    (patternHist, res) <- purely P.fold' buildPatternHist (parsed parseFreqSumEntry PT.stdin)
    case res of
        Left (err, _) -> left $ "Parsing error: " ++ show err
        Right () -> return ()
    let hist = RareAlleleHistogram (selectFromList popIndices nVec) 0 maxM globalMax [] patternHist
    hist' <- tryRight $ setNrCalledSites nrCalledSites hist
    outs <- tryRight $ showHistogram hist'
    scriptIO $ T.putStr outs
  where
    buildPatternHist = Fold step Map.empty id
    step m fse = Map.insertWith (\_ v -> v + 1) (mkPat fse) 1 m
    mkPat = makePattern . selectFromList popIndices . fsCounts
    makePattern vec = if isHigherAF vec then Higher else Pattern vec
    isHigherAF = if globalMax then (>maxM) . sum else any (>maxM)

selectFromList :: [Int] -> [a] -> [a]
selectFromList [] v = v
selectFromList i v = map (v !!) i
