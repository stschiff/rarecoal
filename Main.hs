import Core (defaultTimes, getProb)
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Monoid (mempty)
import Control.Applicative ((<$>), (<*>))
import qualified Options.Applicative as OP

data MyOpts = MyOpts {
    nVecStr :: String,
    mVecStr :: String
}

runWithOptions :: MyOpts -> IO ()
runWithOptions opts = do
    let nVec = map read (splitOn "," (nVecStr opts))
        mVec = map read (splitOn "," (mVecStr opts))
        k = length nVec
        lambda = replicate k 1.0
        joins = []
        theta = 0.0005
    print $ getProb defaultTimes lambda joins theta nVec mVec

main = OP.execParser opts >>= runWithOptions
  where
    parser = MyOpts <$> OP.argument OP.str (OP.metavar "NVec") <*> OP.argument OP.str (OP.metavar "MVec")
    opts = OP.info parser mempty