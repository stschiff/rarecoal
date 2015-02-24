{-# LANGUAGE TemplateHaskell #-}

import Data.List.Split (splitPlaces)
import qualified Data.ByteString.Char8 as B
import Control.Applicative ((<$>), (<*>), pure)
import Data.Monoid (mempty, (<>))
import qualified Data.Map as M
import qualified Options.Applicative as OP
import RareAlleleHistogram(RareAlleleHistogram(..), showHistogram, setNrCalledSites, SitePattern(..))
import Control.Lens (makeLenses, views, view)
import Control.Error.Script (runScript, Script, scriptIO)
import Control.Error.Safe (assertErr)
import Control.Monad.Trans.Reader (runReaderT, ReaderT)
import Control.Monad.Trans.Either (hoistEither)
import Control.Monad.Trans.Class (lift)
import Data.List (transpose)
import Data.Int (Int64)

data MyOpts = MyOpts {
    _optNVec :: [Int],
    _optMaxM :: Int,
    _optNrCalledSites :: Int64,
    _optGlobalMax :: Bool
}

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
    scriptIO B.getContents >>= hoistEither . makeHist nVec maxAf globalMax
                           >>= hoistEither . setNrCalledSites nrCalledSites
                           >>= hoistEither . showHistogram
                           >>= scriptIO . putStr

makeHist :: [Int] -> Int -> Bool -> B.ByteString -> Either String RareAlleleHistogram
makeHist nVec maxAf global s = do
    let loci = B.transpose . B.lines $ s
    assertErr "nVec doesn't sum up to correct number of samples" $ B.length (head loci) == sum nVec
    let getFreqSum = map (length . filter (=='1')) . splitPlaces nVec . B.unpack
        freqSums = map getFreqSum loci
        pred_ = if global then (<=maxAf) . sum else any (<=maxAf)
        toPattern p = if pred_ p then Pattern p else Higher
        insert m k = M.insertWith (+) k 1 m
        counts = foldl insert M.empty $ map toPattern freqSums
    return $ RareAlleleHistogram nVec 0 maxAf global counts
