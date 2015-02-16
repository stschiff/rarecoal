import qualified Data.Map.Strict as Map
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mempty, (<>))
import RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..), setNrCalledSites, showHistogram)
import qualified Options.Applicative as OP
import Pipes ((>->))
import qualified Pipes.Prelude as P
import FreqSumEntry (FreqSumEntry(..))
import Data.Int (Int64)
import Control.Error.Script (scriptIO, runScript)
import Control.Monad.Trans.Either (hoistEither)

data MyOpts = MyOpts {
    optNVec:: [Int],
    optMaxM :: Int,
    optPopIndices :: [Int],
    optNrCalledSites :: Int64,
    globalMax :: Bool
}

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
                <*> OP.option OP.auto (OP.long "globalMax" <> OP.short 'g' <> OP.value False <> OP.showDefault
                                       <> OP.help "constrain global allele frequency")

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts nVec maxM popIndices nrCalledSites globalMax) = runScript $ do
    let prod = P.stdinLn >-> P.map (mkPat maxM popIndices globalMax)
    res <- scriptIO $ P.fold insertPattern Map.empty id prod
    let hist = RareAlleleHistogram (selectFromList nVec popIndices) 0 maxM False res
    hist' <- hoistEither $ setNrCalledSites nrCalledSites hist 
    outs <- hoistEither $ showHistogram hist'
    scriptIO $ putStr outs

mkPat :: Int -> [Int] -> Bool -> String -> SitePattern
mkPat maxM popIndices globalMax line =
    let pattern = selectFromList (fsCounts $ read line) popIndices
    in if filterPattern pattern then Higher else Pattern pattern
  where
    filterPattern = if globalMax then (>maxM) . sum else any (>maxM)

insertPattern :: Map.Map SitePattern Int64 -> SitePattern -> Map.Map SitePattern Int64
insertPattern m p = Map.insertWith (\_ v -> v + 1) p 1 m

selectFromList :: [a] -> [Int] -> [a]
selectFromList l [] = l
selectFromList l i = map (l!!) i
