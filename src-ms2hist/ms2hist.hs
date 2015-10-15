{-# LANGUAGE TemplateHaskell #-}

import Data.List.Split (splitPlaces)
import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Options.Applicative as OP
import Rarecoal.RareAlleleHistogram(RareAlleleHistogram(..), showHistogram, setNrCalledSites, SitePattern(..))
import Control.Lens (makeLenses)
import Control.Error (runScript, scriptIO, assertErr, tryRight)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text.IO as T

data MyOpts = MyOpts [Int] Int Int64

makeLenses ''MyOpts

main :: IO ()
main = OP.execParser opts >>= mainWithOptions
  where
    parser = MyOpts <$> OP.option OP.auto (OP.short 'n' <> OP.long "nVec" <> OP.metavar "<LIST>")
                    <*> OP.option OP.auto (OP.short 'm' <> OP.long "maxM" <> OP.metavar "<INT>")
                    <*> OP.option OP.auto (OP.short 'N' <> OP.long "nrCalledSites" <> OP.metavar "INT")
    opts = OP.info parser mempty

mainWithOptions :: MyOpts -> IO ()
mainWithOptions (MyOpts nVec maxAf nrCalledSites) = runScript $
    scriptIO B.getContents >>= tryRight . makeHist nVec maxAf
                           >>= tryRight . setNrCalledSites nrCalledSites
                           >>= tryRight . showHistogram
                           >>= scriptIO . T.putStr

makeHist :: [Int] -> Int -> B.ByteString -> Either String RareAlleleHistogram
makeHist nVec maxAf s = do
    let loci = B.transpose . B.lines $ s
    assertErr "nVec doesn't sum up to correct number of samples" $ B.length (head loci) == sum (map fromIntegral nVec)
    let getFreqSum = map (length . filter (=='1')) . splitPlaces nVec . B.unpack
        freqSums = map getFreqSum loci
        pred_ = (<=maxAf) . sum
        toPattern p = if pred_ p then Pattern p else Higher
        insert m k = M.insertWith (+) k 1 m
        counts = foldl insert M.empty $ map toPattern freqSums
    return $ RareAlleleHistogram nVec 0 maxAf [] counts
