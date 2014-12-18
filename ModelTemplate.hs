module ModelTemplate (ModelTemplate(..), readModelTemplate, instantiateModel, getModelSpec) where

import Data.String.Utils (replace)
import Data.List.Split (splitOn)
import Control.Monad (liftM)
import Control.Error (Script, scriptIO)
import Control.Error.Safe (assertErr, readErr, justErr)
import Control.Monad.Trans.Either (hoistEither, left, right)
import Core (defaultTimes, ModelSpec(..), ModelEvent(..), EventType(..))
import qualified Data.Vector.Unboxed as V
import Text.Parsec.String (parseFromFile, Parser)
import Text.Parsec.Char (char, newline, letter, oneOf, noneOf, space, alphaNum)
import Text.Parsec (sepBy, many)

data ModelTemplate = ModelTemplate {
    mtParams :: [String],
    mtTheta :: Double,
    mtTimeSteps :: [Double],
    mtEventTemplates :: [EventTemplate],
    mtConstraintTemplates :: [ConstraintTemplate]
}

data EventTemplate = EventTemplate {
    etType :: Char,
    etBody :: String
}

data ConstraintTemplate = ConstraintTemplate {
    ctName1 :: String,
    ctComp :: Char,
    ctName2 :: String
}

readModelTemplate :: FilePath -> Double -> [Double] -> Script ModelTemplate
readModelTemplate path theta timeSteps = do
    parseResult <- scriptIO $ parseFromFile parseModelTemplate path
    (names, events, constraints) <- case parseResult of
        Left p -> left $ show p
        Right p -> right p
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
parseParamName = do
    s <- letter
    s' <- many alphaNum
    return (s:s')

parseEvents :: Parser [EventTemplate]
parseEvents = many $ do
    eChar <- oneOf "PJR"
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

validateConstraint :: [String] -> [Double] -> ConstraintTemplate -> Either String ()
validateConstraint pNames params (ConstraintTemplate name1 comp name2) = do
    let l = zip pNames params
    p1 <- justErr ("Undefined parameter in constraint: \"" ++ name1 ++ "\"") $ lookup name1 l
    p2 <- justErr ("Undefined parameter in constraint: \"" ++ name2 ++ "\"") $ lookup name2 l
    if comp == '<' then
        if p1 < p2 then return () else Left $ "Constrained failed: " ++ show p1 ++ " < " ++ show p2
    else
        if p1 > p2 then return () else Left $ "Constrained failed: " ++ show p1 ++ " > " ++ show p2

substituteParams :: [String] -> [Double] -> String -> Either String String
substituteParams [] [] s = Right s
substituteParams (name:names) (p:ps) s =
    let newS = replace ("<" ++ name ++ ">") (show p) s
    in  substituteParams names ps newS
substituteParams _ _ _ = Left "wrong number of params for modelTemplate"

getModelSpec :: FilePath -> Double -> [Double] -> [ModelEvent] -> Script ModelSpec
getModelSpec path theta params events =
    if path /= "/dev/null" then do
        template <- readModelTemplate path theta defaultTimes                    
        hoistEither $ instantiateModel template (V.fromList params)
    else
        return $ ModelSpec defaultTimes theta events

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

