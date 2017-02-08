{-# LANGUAGE OverloadedStrings #-}

module SimCommand (SimCommandOpt(..), runSimCommand) where

import Rarecoal.Core (ModelSpec(..), ModelEvent(..), EventType(..))
import Rarecoal.ModelTemplate (ModelDesc, getModelSpec)

import Control.Error (Script)
import Data.List (sortOn)
import Data.Text (Text, unwords)
import Prelude hiding (unwords)
import Turtle (echo, format, d, g, s, (%))

data SimCommandOpt = SimCommandOpt {
    _ftModelDesc :: ModelDesc,
    _ftBranchNames :: [String],
    _ftNrHaps :: [Int],
    _ftRecomb :: Double,
    _ftL :: Int
}

runSimCommand :: SimCommandOpt -> Script ()
runSimCommand (SimCommandOpt modelDesc names nrHaps rho chromLength) = do
    (ModelSpec _ theta _ _ events) <- getModelSpec modelDesc names
    let thetaL = 2.0 * theta * fromIntegral chromLength
    echo $ format ("scrm "%d%" 1 -t "%g%" -r "%g%" "%d%" -l 100000 "%s%" "%s)
        nSamples thetaL rhoL chromLength (makeSubPopSpec nrHaps)
        (makeModelOpts events)
  where
    nSamples = sum nrHaps
    rhoL = 2.0 * rho * fromIntegral chromLength

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
