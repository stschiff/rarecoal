import Data.List (intercalate, sortBy, foldl')
import Data.List.Split (splitOn, splitPlaces)
import qualified Data.Map.Strict as Map
import Control.Exception.Assert (byEq, assert)
import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Data.Monoid (mempty)
import RareAlleleHistogram (RareAlleleHistogram(..), SitePattern(..))
import qualified Options.Applicative as OP

data MyOpts = MyOpts String

main = OP.execParser opts >>= runWithOptions
  where
    parser = MyOpts <$> OP.argument OP.str (OP.metavar "NVec")
    opts = OP.info parser mempty

runWithOptions :: MyOpts -> IO ()
runWithOptions (MyOpts nVecStr) = do
    let nVec = (*2) . read <$> splitOn "," nVecStr
    content <- getContents
    let maxM = 10
    let hist = makeHist [parseVcfLine l nVec maxM | l <- lines content, head l /= '#'] nVec maxM
    print hist

makeHist :: [SitePattern] -> [Int] -> Int -> RareAlleleHistogram
makeHist patterns nVec maxM =
    let hist = foldl' insertPattern Map.empty patterns
    in  RareAlleleHistogram nVec maxM hist
    where
        insertPattern m p = Map.insertWith (\_ v -> v + 1) p 1 m

-- parseVcfLine :: String -> [Int] -> Int -> SitePattern
-- parseVcfLine line nVec maxM =
--     let fields = words line
--         [[_, ac_field]] = fields!!7 =~ "AC=([0-9]+)" :: [[String]]
--     in if read ac_field > maxM then Higher else Pattern $ getPattern fields nVec

parseVcfLine :: String -> [Int] -> Int -> SitePattern
parseVcfLine line nVec maxM = 
    let fields = words line
        pattern = getPattern fields nVec
    in if any (>maxM) pattern then Higher else Pattern $ pattern

getPattern :: [String] -> [Int] -> [Int]
getPattern fields nVec =
    let gen_fields = drop 9 fields
        gens = concat [[g1, g2] | (g1:_:g2:_) <- gen_fields]
        ass = byEq assert "Nr haplotypes" (sum $ nVec) (length gens)
    in  ass [length $ filter (=='1') c | c <- splitPlaces nVec gens]
