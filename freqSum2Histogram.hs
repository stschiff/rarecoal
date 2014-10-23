import qualified Data.Map.Strict as Map
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mempty, (<>))
import RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..))
import qualified Options.Applicative as OP
import Pipes ((>->), runEffect)
import qualified Pipes.Prelude as P
import FreqSumEntry (FreqSumEntry(..))

data MyOpts = MyOpts [Int] Int [Int]

main = OP.execParser opts >>= runWithOptions
  where
    opts = OP.info (OP.helper <*> parser) mempty

parser :: OP.Parser MyOpts
parser = MyOpts <$> OP.argument OP.auto (OP.metavar "nVec")
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

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts nVec maxM popIndices) = do
    let prod = P.stdinLn >-> P.map (mkPat maxM popIndices)
    res <- P.fold insertPattern Map.empty id prod
    print $ RareAlleleHistogram (map (*2) $ selectFromList nVec popIndices) maxM res

mkPat :: Int -> [Int] -> String -> SitePattern
mkPat maxM popIndices line =
    let pattern = selectFromList (fsCounts $ read line) popIndices
    in if any (>maxM) pattern then Higher else Pattern $ pattern

insertPattern :: Map.Map SitePattern Int -> SitePattern -> Map.Map SitePattern Int
insertPattern m p = Map.insertWith (\_ v -> v + 1) p 1 m

selectFromList :: [a] -> [Int] -> [a]
selectFromList l [] = l
selectFromList l i = map (l!!) i