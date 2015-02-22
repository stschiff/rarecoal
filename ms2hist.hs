{-# LANGUAGE TemplateHaskell #-}

import Data.List.Split (splitPlaces)
import Control.Applicative ((<$>), (<*>), pure)
import Data.Monoid (mempty, (<>))
import qualified Data.Map as M
import qualified Options.Applicative as OP
import RareAlleleHistogram(RareAlleleHistogram(..), showHistogram, SitePattern(..))
import Control.Lens (makeLenses, views, view)
import Control.Error.Script (runScript, Script, scriptIO)
import Control.Error.Safe (assertErr)
import Control.Monad.Trans.Reader (runReaderT, ReaderT)
import Control.Monad.Trans.Either (hoistEither)
import Control.Monad.Trans.Class (lift)
import Data.List (transpose)

data MyOpts = MyOpts {
    _optNVec :: [Int],
    _optMaxM :: Int,
    _optGlobalMax :: Bool
}

makeLenses ''MyOpts

main :: IO ()
main = OP.execParser opts >>= mainWithOptions
  where
    parser = MyOpts <$> OP.option OP.auto (OP.short 'n' <> OP.long "nVec" <> OP.metavar "<LIST>")
                    <*> OP.option OP.auto (OP.short 'm' <> OP.long "maxM" <> OP.metavar "<INT>")
                    <*> OP.switch (OP.short 'g' <> OP.long "globalMax")
    opts = OP.info parser mempty

mainWithOptions :: MyOpts -> IO ()
mainWithOptions (MyOpts nVec maxAf globalMax) = runScript $ do
    s <- scriptIO getContents
    hist <- hoistEither $ makeHist nVec maxAf globalMax s
    outs <- hoistEither $ showHistogram hist
    scriptIO $ putStrLn outs

makeHist :: [Int] -> Int -> Bool -> String -> Either String RareAlleleHistogram
makeHist nVec maxAf global s = do
    let loci = transpose . drop 6 . lines $ s
    assertErr "nVec doesn't sum up to correct number of samples" $ length (head loci) == sum nVec
    let getFreqSum = map (length . filter (=='1')) . splitPlaces nVec
        freqSums = map getFreqSum loci
        pred_ = if global then (<=maxAf) . sum else any (<=maxAf)
        toPattern p = if pred_ p then Pattern p else Higher
        insert m k = M.insertWith (+) k 1 m
        counts = foldl insert M.empty $ map toPattern freqSums
    return $ RareAlleleHistogram nVec 0 maxAf global counts
