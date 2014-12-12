import Pipes (runEffect, (>->))
import qualified Pipes.Prelude as P
import Data.List.Split (splitPlaces)
import Control.Exception.Assert (assert)
import Control.Applicative ((<$>))
import Data.Monoid (mempty, (<>))
import qualified Options.Applicative as OP
import FreqSumEntry (FreqSumEntry(..))

data MyOpts = MyOpts [Int]

main :: IO ()
main = OP.execParser opts >>= runWithOptions
  where
    parser = MyOpts <$> OP.option OP.auto (OP.short 'n' <> OP.long "nVec" <> OP.metavar "NVec")
    opts = OP.info parser mempty

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts nVec) =
    runEffect $ P.stdinLn >-> P.filter (\l -> head l /= '#') >-> P.map (processVCFline nVec) >-> P.stdoutLn

processVCFline :: [Int] -> String -> String
processVCFline nVec line = 
    let (chrom:pos:_:ref:alt:_:_:_:_:genFields) = words line
        gens = concat [[g1, g2] | (g1:_:g2:_) <- genFields]
        ass = assert $ 2 * sum nVec == length gens
        counts = ass [length $ filter (=='1') c | c <- splitPlaces (map (*2) nVec) gens]
    in  show $ FreqSumEntry chrom (read pos) (head ref) (head alt) counts
