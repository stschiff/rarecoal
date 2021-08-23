{-# LANGUAGE OverloadedStrings #-}

module SimCommand (SimCommandOpt(..), runSimCommand) where

import RarecoalLib.Utils (GeneralOptions(..), RarecoalException(..))
import RarecoalLib.Core (ModelSpec(..), ModelEvent(..), EventType(..))
import RarecoalLib.ModelTemplate (ModelOptions(..), ParamOptions(..),
                               getModelTemplate, makeParameterDict,
                               instantiateModel)

import Control.Exception (throwIO)
import Control.Monad (forM)
import Data.List (sortOn)

data SimCommandOpt = SimCommandOpt {
    _ftGeneralOpts :: GeneralOptions,
    _ftModelOpts :: ModelOptions,
    _ftParamOpts :: ParamOptions,
    _ftNrHaps :: [Int],
    _ftRecomb :: Double,
    _ftL :: Int
}

runSimCommand :: SimCommandOpt -> IO ()
runSimCommand opts = do
    modelTemplate <- getModelTemplate (_ftModelOpts opts)
    modelParams <- makeParameterDict (_ftParamOpts opts)
    modelSpec <- case instantiateModel (_ftGeneralOpts opts) modelTemplate modelParams of
        Left err -> throwIO $ RarecoalModelException err
        Right m -> return m
    let (ModelSpec _ _ theta _ _ _ events) = modelSpec
    let thetaL = 2.0 * theta * fromIntegral (_ftL opts)
    modelOptsString <- case makeModelOpts events of
        Left err -> throwIO err
        Right m -> return m
    putStrLn $ "scrm " ++ show nSamples ++ " 1 -t " ++ show thetaL ++
        " -r " ++ show rhoL ++ " " ++ show (_ftL opts) ++ " -l 100000 " ++ (makeSubPopSpec (_ftNrHaps opts)) ++
        " " ++ modelOptsString
  where
    nSamples = sum $ _ftNrHaps opts
    rhoL = 2.0 * _ftRecomb opts * fromIntegral (_ftL opts)

makeSubPopSpec :: [Int] -> String
makeSubPopSpec nrHaps = "-I " ++ show nPops ++ " " ++ nInds
  where
    nPops = length nrHaps
    nInds = unwords . map show $ nrHaps

makeModelOpts :: [ModelEvent] -> Either RarecoalException String 
makeModelOpts events = do
    eventStrings <- forM (sortOn (\(ModelEvent t _) -> t) events) $ \e -> 
        case e of
            ModelEvent t (Join k l) -> return $
                "-ej " ++ show (0.5 * t) ++ " " ++ show (l + 1) ++ " " ++ show (k + 1)
            ModelEvent t (Split k l a) -> return $
                "-eps " ++ show (0.5 * t) ++ " " ++ show (l + 1) ++ " " ++ show (k + 1) ++ " " ++ show (1.0 - a)
            ModelEvent t (SetPopSize k p) -> return $
                "-en " ++ show (0.5 * t) ++ " " ++ show (k + 1) ++ " " ++ show p
            _ -> Left $ RarecoalSimException "currently only Joins, PopSize changes and Admixture is implemented for \
                        \simulation parameters"
    return $ unwords eventStrings
