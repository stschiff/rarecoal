{-# LANGUAGE OverloadedStrings #-}

module SimCommand (SimCommandOpt(..), runSimCommand) where

import Rarecoal.Utils (GeneralOptions(..))
import Rarecoal.Core (ModelSpec(..), ModelEvent(..), EventType(..))
import Rarecoal.ModelTemplate (ModelOptions(..), ParamOptions(..),
                               getModelTemplate, makeParameterDict,
                               instantiateModel)

import Control.Error (Script, scriptIO, tryRight)
import Data.List (sortOn)
import Data.Text (Text, unwords)
import qualified Data.Text.IO as T
import Prelude hiding (unwords)
import Turtle (format, d, g, s, (%))

data SimCommandOpt = SimCommandOpt {
    _ftGeneralOpts :: GeneralOptions,
    _ftModelOpts :: ModelOptions,
    _ftParamOpts :: ParamOptions,
    _ftNrHaps :: [Int],
    _ftRecomb :: Double,
    _ftL :: Int
}

runSimCommand :: SimCommandOpt -> Script ()
runSimCommand opts = do
    modelTemplate <- getModelTemplate (_ftModelOpts opts)
    modelParams <- scriptIO $ makeParameterDict (_ftParamOpts opts)
    modelSpec <- tryRight $ instantiateModel (_ftGeneralOpts opts)
        modelTemplate modelParams
    let (ModelSpec _ theta _ _ _ events) = modelSpec
    let thetaL = 2.0 * theta * fromIntegral (_ftL opts)
    scriptIO . T.putStr $ format ("scrm "%d%" 1 -t "%g%" -r "%g%" "%d%
        " -l 100000 "%s%" "%s) nSamples thetaL rhoL (_ftL opts)
        (makeSubPopSpec (_ftNrHaps opts))
        (makeModelOpts events)
  where
    nSamples = sum $ _ftNrHaps opts
    rhoL = 2.0 * _ftRecomb opts * fromIntegral (_ftL opts)

makeSubPopSpec :: [Int] -> Text
makeSubPopSpec nrHaps = format ("-I "%d%" "%s) nPops nInds
  where
    nPops = length nrHaps
    nInds = unwords . map (format d) $ nrHaps

makeModelOpts :: [ModelEvent] -> Text
makeModelOpts events = unwords $ do
    e <- sortOn (\(ModelEvent t _) -> t) events
    case e of
        ModelEvent t (Join k l) -> return $ format ("-ej "%g%" "%d%" "%d) (0.5 * t) (l + 1) (k + 1)
        ModelEvent t (Split k l a) -> return $
                format ("-eps "%g%" "%d%" "%d%" "%g) (0.5 * t) (l + 1) (k + 1) (1.0 - a)
        ModelEvent t (SetPopSize k p) -> return $ format ("-en "%g%" "%d%" "%g) (0.5 * t) (k + 1) p
        _ -> error "currnently only Joins, PopSize changes and Admixture is implemented for \
                    \simulation parameters"
