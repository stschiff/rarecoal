{-# LANGUAGE TemplateHaskell #-}

import Data.List.Split (splitPlaces)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mempty, (<>))
import qualified Data.Map as M
import qualified Options.Applicative as OP
import RareAlleleHistogram(RareAlleleHistogram(..), showHistogram, setNrCalledSites, SitePattern(..))
import Control.Lens (makeLenses)
import Control.Error (runScript, scriptIO, assertErr, tryRight)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.Char8 as B

data MyOpts = MyOpts [Int] Int Int64 Bool

makeLenses ''MyOpts

main :: IO ()
main = OP.execParser opts >>= mainWithOptions
  where
    parser = MyOpts <$> OP.option OP.auto (OP.short 'n' <> OP.long "nVec" <> OP.metavar "<LIST>")
                    <*> OP.option OP.auto (OP.short 'm' <> OP.long "maxM" <> OP.metavar "<INT>")
                    <*> OP.option OP.auto (OP.short 'N' <> OP.long "nrCalledSites" <> OP.metavar "INT")
                    <*> OP.switch (OP.short 'g' <> OP.long "globalMax")
    opts = OP.info parser mempty

mainWithOptions :: MyOpts -> IO ()
mainWithOptions (MyOpts nVec maxAf nrCalledSites globalMax) = runScript $
    scriptIO B.getContents >>= tryRight . makeHist nVec maxAf globalMax
                           >>= tryRight . setNrCalledSites nrCalledSites
                           >>= tryRight . showHistogram
                           >>= scriptIO . B.putStr

makeHist :: [Int] -> Int -> Bool -> B.ByteString -> Either String RareAlleleHistogram
makeHist nVec maxAf global s = do
    let loci = B.transpose . B.lines $ s
    assertErr "nVec doesn't sum up to correct number of samples" $ B.length (head loci) == sum (map fromIntegral nVec)
    let getFreqSum = map (length . filter (=='1')) . splitPlaces nVec . B.unpack
        freqSums = map getFreqSum loci
        pred_ = if global then (<=maxAf) . sum else any (<=maxAf)
        toPattern p = if pred_ p then Pattern p else Higher
        insert m k = M.insertWith (+) k 1 m
        counts = foldl insert M.empty $ map toPattern freqSums
    return $ RareAlleleHistogram nVec 0 maxAf global [] counts
