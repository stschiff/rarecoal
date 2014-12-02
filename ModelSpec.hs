module ModelSpec (ModelEvent(..), EventType(..), ModelSpec(..), ModelTemplate(..), instantiateModel) where

import Data.String.Utils (replace)
import Data.List.Split (splitOn)

data ModelEvent = ModelEvent {
    meTime :: Double,
    meEventType ::EventType
} deriving (Show, Read)

data EventType = Join Int Int | SetPopSize Int Double | SetGrowthRate Int Double deriving (Show, Read)

data ModelSpec = ModelSpec {
    mTimeSteps :: [Double],
    mTheta :: Double,
    mEvents :: [ModelEvent]
} deriving (Show)

data ModelTemplate = ModelTemplate {
    mtParams :: [String],
    mtBody :: String
}

instance Read ModelTemplate where
    readsPrec _ s = 
        let (pL:bL) = lines s
            names = splitOn "," pL
        in  [(ModelTemplate names (unlines bL), "")]

instantiateModel :: ModelTemplate -> Double -> [Double] -> [Double] -> ModelSpec
instantiateModel (ModelTemplate pNames body) theta timeSteps params =
    let body' = substituteParams pNames params body
    in  ModelSpec timeSteps theta (parseBody body')
  where
    substituteParams [] [] b = b
    substituteParams (name:names) (p:ps) b =
        let newB = replace ("<" ++ name ++ ">") (show p) b
        in  substituteParams names ps newB

parseBody :: String -> [ModelEvent]
parseBody body = [parseEvent line | line <- lines body] 

parseEvent :: String -> ModelEvent
parseEvent line =
    let [key, val] = words line
    in  case key of
        "P" -> makePopSizeChange val
        "J" -> makeJoin val
        "R" -> makeGrowthRate val
        _   -> undefined

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

