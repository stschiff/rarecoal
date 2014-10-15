import qualified Data.Map.Strict as Map
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mempty)
import RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..))
import qualified Options.Applicative as OP
import Pipes ((>->), runEffect)
import qualified Pipes.Prelude as P
import FreqSumEntry (FreqSumEntry(..))

data MyOpts = MyOpts [Int] Int

main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info parser mempty

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.argument OP.auto (OP.metavar "nVec") <*> OP.argument OP.auto (OP.metavar "maxM")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts nVec maxM) = do
    let prod = P.stdinLn >-> P.map (mkPat maxM)
    res <- P.fold insertPattern Map.empty id prod
    print $ RareAlleleHistogram nVec maxM res

mkPat :: Int -> String -> SitePattern
mkPat maxM line =
    let pattern = fsCounts $ read line
    in if any (>maxM) pattern then Higher else Pattern $ pattern

insertPattern :: Map.Map SitePattern Int -> SitePattern -> Map.Map SitePattern Int
insertPattern m p = Map.insertWith (\_ v -> v + 1) p 1 m
