{-# LANGUAGE OverloadedStrings #-}

module SimCommand (SimCommandOpt(..), runSimCommand) where

import Rarecoal.Core (ModelSpec(..), ModelEvent(..), EventType(..))
import Rarecoal.ModelTemplate (ModelDesc, getModelSpec)

import Control.Error (Script)
import Data.List (sortOn)
import Data.Text (Text, intercalate)
import Turtle (echo, format, d, g, s, (%))

data SimCommandOpt = SimCommandOpt {
    _ftModelDesc :: ModelDesc,
    _ftBranchNames :: [String],
    _ftNrHaps :: [Int],
    _ftTheta :: Double,
    _ftRecomb :: Double,
    _ftL :: Int
}

runSimCommand :: SimCommandOpt -> Script ()
runSimCommand (SimCommandOpt modelDesc names nrHaps theta rho chromLength) = do
    (ModelSpec _ _ events) <- getModelSpec modelDesc names theta 400
    echo $ format ("scrm "%d%" 1 -t "%g%" -r "%g%" "%d%" -l 100000 "%s%" "%s) nSamples thetaL 
                  rhoL chromLength (makeSubPopSpec nrHaps) (makeModelOpts events)
  where
    nSamples = length names
    thetaL = 2.0 * theta * fromIntegral chromLength
    rhoL = 2.0 * rho * fromIntegral chromLength
    
makeSubPopSpec :: [Int] -> Text
makeSubPopSpec nrHaps = format ("-I "%d%" "%s) nPops nInds
  where
    nPops = length nrHaps
    nInds = intercalate " " . map (format d) $ nrHaps

makeModelOpts :: [ModelEvent] -> Text
makeModelOpts events = intercalate " " $ do
    e <- sortOn (\(ModelEvent t _) -> t) events
    case e of
        ModelEvent t (Join k l) -> return $ format ("-ej "%g%" "%d%" "%d) (0.5 * t) k l
        ModelEvent t (Split k l a) -> return $
                format ("-eps "%g%" "%d%" "%d%" "%g) (0.5 * t) l k (1.0 - a)
        ModelEvent t (SetPopSize k p) -> return $ format ("-en "%g%" "%d%" "%g) (0.5 * t) k p
        _ -> error "currnently only Joins, PopSize changes and Admixture is implemented for \ 
                    \simulation parameters"
