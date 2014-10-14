import Pipes (runEffect, (>->))
import qualified Pipes.Prelude as P
import Data.List.Split (splitOn, splitPlaces)
import Control.Exception.Assert (assert)
import Control.Applicative ((<$>))
import Data.Monoid (mempty)
import qualified Options.Applicative as OP
import FreqSumEntry (FreqSumEntry(..))

data MyOpts = MyOpts String

main = OP.execParser opts >>= runWithOptions
  where
    parser = MyOpts <$> OP.argument OP.str (OP.metavar "NVec")
    opts = OP.info parser mempty

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts nVecStr) = do
    let nVec = (*2) . read <$> splitOn "," nVecStr
    runEffect $ P.stdinLn >-> P.filter (\l -> head l /= '#') >-> P.map (processVCFline nVec) >-> P.stdoutLn

processVCFline :: [Int] -> String -> String
processVCFline nVec line = 
    let (chrom:pos:_:ref:alt:_:_:_:_:genFields) = words line
        gens = concat [[g1, g2] | (g1:_:g2:_) <- genFields]
        ass = assert $ (sum $ nVec) == (length gens)
        counts = ass $ [length $ filter (=='1') c | c <- splitPlaces nVec gens]
    in  show $ FreqSumEntry chrom (read pos) (head ref) (head alt) counts
