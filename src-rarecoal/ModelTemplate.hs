module ModelTemplate (InitialParams(..), getInitialParams, ModelTemplate(..), readModelTemplate, instantiateModel,      
                      getModelSpec) where

import Data.String.Utils (replace)
import Data.List.Split (splitOn)
import Control.Monad (unless)
import Control.Error (Script, scriptIO, tryRight, readErr, justErr, tryJust, throwE)
import Core (getTimeSteps, ModelSpec(..), ModelEvent(..), EventType(..))
import qualified Data.Vector.Unboxed as V
import Text.Parsec.String (parseFromFile, Parser)
import Text.Parsec.Char (char, newline, letter, oneOf, noneOf, space, alphaNum)
import Text.Parsec (sepBy, many)
import System.Log.Logger (infoM)

data ModelTemplate = ModelTemplate {
    mtParams :: [String],
    mtTheta :: Double,
    mtTimeSteps :: [Double],
    mtEventTemplates :: [EventTemplate],
    mtConstraintTemplates :: [ConstraintTemplate]
}

data EventTemplate = EventTemplate Char String

data ConstraintTemplate = ConstraintTemplate String Char String

data InitialParams = InitialParamsList [Double] | InitialParamsFile FilePath

getInitialParams :: ModelTemplate -> InitialParams -> Script (V.Vector Double)
getInitialParams modelTemplate params = do
    case params of
        InitialParamsList x -> return . V.fromList $ x
        InitialParamsFile path -> do
            l <- lines <$> (scriptIO . readFile $ path)
            ret <- if (head . head $ l) == '#' then
                    loadFromDict [(k, read $ v !! 2) | (k : v) <- map words . drop 3 $ l]
                else
                    loadFromDict [(k, read v) | [k, v] <- map words $ l]
            scriptIO . infoM "rarecoal" $ "initial parameters loaded: " ++ show ret
            return ret
  where
    loadFromDict dict = do
        let err = "parameters in the initialParams-file do not match the parameters in the modelTemplate"
        x <- tryJust err . mapM (`lookup` dict) $ mtParams modelTemplate
        return . V.fromList $ x
        
readModelTemplate :: FilePath -> Double -> [Double] -> Script ModelTemplate
readModelTemplate path theta timeSteps = do
    parseResult <- scriptIO $ parseFromFile parseModelTemplate path
    (names, events, constraints) <- case parseResult of
        Left p -> throwE $ show p
        Right p -> return p
    return $ ModelTemplate names theta timeSteps events constraints

parseModelTemplate :: Parser ([String], [EventTemplate], [ConstraintTemplate])
parseModelTemplate = do
    params <- parseParams
    events <- parseEvents
    constrains <- parseConstraints
    return (params, events, constrains)

parseParams :: Parser [String]
parseParams = do
    names <- sepBy parseParamName (char ',')
    newline
    return names

parseParamName :: Parser String
parseParamName = (:) <$> letter <*> many alphaNum

parseEvents :: Parser [EventTemplate]
parseEvents = many $ do
    eChar <- oneOf "PJRM"
    space
    eBody <- parseLine
    newline
    return $ EventTemplate eChar eBody

parseLine = many $ noneOf "\n"

parseConstraints :: Parser [ConstraintTemplate]
parseConstraints = many $ do
    char 'C'
    space
    name1 <- parseParamName
    comp <- oneOf "<>"
    name2 <- parseParamName
    return $ ConstraintTemplate name1 comp name2

instantiateModel :: ModelTemplate -> V.Vector Double -> Either String ModelSpec
instantiateModel (ModelTemplate pNames theta timeSteps ets cts) params = do
    let params' = V.toList params
    events <- mapM (instantiateEvent pNames params') ets
    mapM_ (validateConstraint pNames params') cts
    return $ ModelSpec timeSteps theta events

instantiateEvent :: [String] -> [Double] -> EventTemplate -> Either String ModelEvent
instantiateEvent pnames params (EventTemplate et body) = do
    newB <- substituteParams pnames params body
    let fields = splitOn "," newB
        t = read . head $ fields
        err = "Illegal Modeltemplate statement, or undefined parameter in \"" ++ body ++ "\""
    case et of
        'P' -> do
            k <- readErr err $ fields!!1
            p <- readErr err $ fields!!2
            return $ ModelEvent t (SetPopSize k p)
        'R' -> do
            k <- readErr err $ fields!!1
            r <- readErr err $ fields!!2
            return $ ModelEvent t (SetGrowthRate k r)
        'J' -> do
            k <- readErr err $ fields!!1
            l <- readErr err $ fields!!2
            return $ ModelEvent t (Join k l)
        'M' -> do
            k <- readErr err $ fields!!1
            l <- readErr err $ fields!!2
            r <- readErr err $ fields!!3
            return $ ModelEvent t (SetMigration k l r)
        _   -> Left "cannot parse modelTemplate Event"

validateConstraint :: [String] -> [Double] -> ConstraintTemplate -> Either String ()
validateConstraint pNames params (ConstraintTemplate name1 comp name2) = do
    let l = zip pNames params
    p1 <- justErr ("Undefined parameter in constraint: \"" ++ name1 ++ "\"") $ lookup name1 l
    p2 <- justErr ("Undefined parameter in constraint: \"" ++ name2 ++ "\"") $ lookup name2 l
    if comp == '<' then
        unless (p1 < p2) $ Left $ "Constrained failed: " ++ show p1 ++ " < " ++ show p2
    else
        unless (p1 > p2) $ Left $ "Constrained failed: " ++ show p1 ++ " > " ++ show p2

substituteParams :: [String] -> [Double] -> String -> Either String String
substituteParams [] [] s = Right s
substituteParams (name:names) (p:ps) s =
    let newS = replace ("<" ++ name ++ ">") (show p) s
    in  substituteParams names ps newS
substituteParams _ _ _ = Left "wrong number of params for modelTemplate"

getModelSpec :: FilePath -> Double -> InitialParams -> [ModelEvent] -> Int -> Script ModelSpec
getModelSpec path theta params events lingen =
    let times = getTimeSteps 20000 lingen 20.0
    in  if path /= "/dev/null" then do
            template <- readModelTemplate path theta times
            x <- getInitialParams template params
            tryRight $ instantiateModel template x
        else
            return $ ModelSpec times theta events
