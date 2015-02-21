{-# LANGUAGE TemplateHaskell #-}

import Data.List.Split (splitPlaces)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (mempty, (<>))
import qualified Options.Applicative as OP
import RareAlleleHistogram(RareAlleleHistogram(..), showHistogram)
import Control.Lens (makeLenses, views, view)
import Control.Error.Script (runScript, Script, scriptIO)
import Control.Monad.Trans.Reader (runReaderT, ReaderT)
import Control.Monad.Trans.Class (lift)

data MyOpts = MyOpts {
    _optNVec :: [Int],
    _optMaxM :: Int,
    _optGlobalMax :: Bool,
    _optFile :: FilePath
}

makeLenses ''MyOpts

main :: IO ()
main = OP.execParser opts >>= mainWithOptions
  where
    parser = MyOpts <$> OP.option OP.auto (OP.short 'n' <> OP.long "nVec" <> OP.metavar "<LIST>")
                    <*> OP.option OP.auto (OP.short 'm' <> OP.long "maxM" <> OP.metavar "<INT>")
                    <*> OP.switch (OP.short 'g' <> OP.long "globalMax")
                    <*> OP.argument OP.str (OP.metavar "<FILE>")
    opts = OP.info parser mempty

type App = ReaderT MyOpts Script 

mainWithOptions :: MyOpts -> IO ()
mainWithOptions opts = runScript $ runReaderT run opts

run :: App ()
run = do
    fn <- view optFile
    s <- lift . scriptIO $ readFile fn
    lift . scriptIO $ putStrLn s
    
