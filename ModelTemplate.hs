module ModelTemplate (ModelEvent(..), EventType(..), ModelSpec(..), ModelTemplate(..), instantiateModel, readModelTemplate, getModelSpec) where

import Data.String.Utils (replace)
import Data.List.Split (splitOn)
import Control.Monad (liftM)
import Control.Applicative ((<$>))
import Control.Error (Script, scriptIO)
import Control.Monad.Trans.Either (hoistEither)
import Core (defaultTimes, ModelSpec(..), ModelEvent(..), EventType(..))
import qualified Data.Vector.Unboxed as V

data ModelTemplate = ModelTemplate {
    mtParams :: [String],
    mtTheta :: Double,
    mtTimeSteps :: [Double],
    mtBody :: String
}

getModelSpec :: FilePath -> Double -> [Double] -> [ModelEvent] -> Script ModelSpec
getModelSpec path theta params events =
    if path /= "/dev/null" then do
        template <- scriptIO $ readModelTemplate path theta defaultTimes                    
        hoistEither $ instantiateModel template (V.fromList params)
    else
        return $ ModelSpec defaultTimes theta events


readModelTemplate ::  FilePath -> Double -> [Double] -> IO ModelTemplate
readModelTemplate path theta timeSteps = do
    (pL:bL) <- liftM lines . readFile $ path
    let names = splitOn "," pL
    return $ ModelTemplate names theta timeSteps (unlines bL)

instantiateModel :: ModelTemplate -> V.Vector Double -> Either String ModelSpec
instantiateModel (ModelTemplate pNames theta timeSteps body) params = do
    body' <- substituteParams pNames (V.toList params) body
    ModelSpec timeSteps theta <$> parseBody body'
  where
    substituteParams [] [] b = Right b
    substituteParams (name:names) (p:ps) b =
        let newB = replace ("<" ++ name ++ ">") (show p) b
        in  substituteParams names ps newB
    substituteParams _ _ _ = Left "wrong number of params for modelTemplate"

parseBody :: String -> Either String [ModelEvent]
parseBody body = mapM parseEvent $ lines body

parseEvent :: String -> Either String ModelEvent
parseEvent line =
    let [key, val] = words line
    in  case key of
        "P" -> Right $ makePopSizeChange val
        "J" -> Right $ makeJoin val
        "R" -> Right $ makeGrowthRate val
        _   -> Left $ "unknown event type '" ++ key ++ "'"

makePopSizeChange :: String -> ModelEvent
makePopSizeChange s = 
    let [t, k, p] = splitOn "," s
    in  ModelEvent (read t) (SetPopSize (read k) (read p))

makeJoin :: String -> ModelEvent
makeJoin s =
    let [t, k, l] = splitOn "," s
    in  ModelEvent (read t) (Join (read k) (read l))

makeGrowthRate :: String -> ModelEvent
makeGrowthRate s = 
    let [t, k, r] = splitOn "," s
    in  ModelEvent (read t) (SetGrowthRate (read k) (read r))

