module Rarecoal.ModelTemplate (getInitialParams, ModelTemplate(..), readModelTemplate, 
                               instantiateModel, ModelDesc(..),
                               getModelSpec, EventTemplate(..), ParamsDesc) where

import Rarecoal.Core (getTimeSteps, ModelSpec(..), ModelEvent(..), EventType(..))

import Control.Applicative ((<|>))
import Control.Error (Script, scriptIO, tryRight, tryJust, assertErr)
import Control.Monad (when)
import qualified Data.Attoparsec.Text as A
import Data.Char (isAlpha, isDigit)
-- import Debug.Trace (trace)
import System.Log.Logger (infoM)
import Data.Text (unpack)
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as V

data ModelTemplate = ModelTemplate {
    mtParams :: [String],
    mtTheta :: Double,
    mtTimeSteps :: [Double],
    mtEventTemplates :: [EventTemplate],
    mtConstraintTemplates :: [ConstraintTemplate]
} deriving (Eq, Show)

type ParamSpec = Either Double String

data EventTemplate = JoinEventTemplate ParamSpec Int Int
                   | SplitEventTemplate ParamSpec Int Int ParamSpec
                   | PopSizeEventTemplate ParamSpec Int ParamSpec
                   | JoinPopSizeEventTemplate ParamSpec Int Int ParamSpec
                   | GrowthRateEventTemplate ParamSpec Int ParamSpec
                   | MigrationRateEventTemplate ParamSpec Int Int ParamSpec
                   deriving (Eq, Show)

data ConstraintTemplate = SmallerConstraintTemplate ParamSpec ParamSpec
                        | GreaterConstraintTemplate ParamSpec ParamSpec
                        deriving (Eq, Show)

data ModelDesc = ModelDescTemplate FilePath ParamsDesc | ModelDescDirect [ModelEvent] 
type ParamsDesc = Either FilePath [Double]

getInitialParams :: ModelTemplate -> ParamsDesc -> Script (V.Vector Double)
getInitialParams modelTemplate paramsDesc = do
    case paramsDesc of
        Left paramsFile -> do
            l <- lines <$> (scriptIO . readFile $ paramsFile)
            ret <- if (head . head $ l) == '#' then
                    loadFromDict [(k, read $ v !! 2) | (k : v) <- map words . drop 3 $ l]
                else
                    loadFromDict [(k, read v) | [k, v] <- map words $ l]
            scriptIO . infoM "rarecoal" $ "initial parameters loaded: " ++ show ret
            return ret
        Right x -> return . V.fromList $ x
  where
    loadFromDict dict = do
        let err' = "parameters in the initialParams-file do not match the parameters in the modelTemplate"
        x' <- tryJust err' . mapM (`lookup` dict) $ mtParams modelTemplate
        return . V.fromList $ x'

readModelTemplate :: FilePath -> Double -> [Double] -> Script ModelTemplate
readModelTemplate path theta timeSteps = do
    c <- scriptIO $ T.readFile path
    (names, events, constraints) <- tryRight $ A.parseOnly (parseModelTemplate <* A.endOfInput) c
    return $ ModelTemplate names theta timeSteps events constraints

parseModelTemplate :: A.Parser ([String], [EventTemplate], [ConstraintTemplate])
parseModelTemplate = do
    params <- parseParams
    events <- parseEvents
    constraints <- parseConstraints
    return (params, events, constraints)

parseParams :: A.Parser [String]
parseParams = do
    names <- A.sepBy parseParamName (A.char ',')
    A.endOfLine
    return names

parseParamName :: A.Parser String
parseParamName = unpack <$> A.takeWhile (\c -> isAlpha c || isDigit c || c == '_')

parseEvents :: A.Parser [EventTemplate]
parseEvents = A.many' (parsePopSizeEvent <|> parseJoinEvent <|> parseSplitEvent <|> 
                       parseJoinPopSizeEvent <|> parseGrowthRateEvent <|> parseMigrationRateEvent)

parsePopSizeEvent :: A.Parser EventTemplate
parsePopSizeEvent = do
    _ <- A.char 'P'
    _ <- A.space
    t <- parseMaybeParam
    _ <- A.char ','
    k <- A.decimal
    _ <- A.char ','
    n <- parseMaybeParam
    A.endOfLine
    return $ PopSizeEventTemplate t k n

parseMaybeParam :: A.Parser ParamSpec
parseMaybeParam = do
    c <- A.peekChar'
    if c == '<' then do
        _ <- A.char '<'
        p <- parseParamName
        _ <- A.char '>'
        return $ Right p
    else do
        val <- A.double
        return $ Left val

parseJoinEvent :: A.Parser EventTemplate
parseJoinEvent = do
    _ <- A.char 'J'
    _ <- A.space
    t <- parseMaybeParam
    _ <- A.char ','
    k <- A.decimal
    _ <- A.char ','
    l <- A.decimal
    A.endOfLine
    return $ JoinEventTemplate t k l

parseSplitEvent :: A.Parser EventTemplate
parseSplitEvent = do
    _ <- A.char 'S'
    _ <- A.space
    t <- parseMaybeParam
    _ <- A.char ','
    k <- A.decimal
    _ <- A.char ','
    l <- A.decimal
    _ <- A.char ','
    m <- parseMaybeParam
    A.endOfLine
    return $ SplitEventTemplate t k l m

parseJoinPopSizeEvent :: A.Parser EventTemplate
parseJoinPopSizeEvent = do
    _ <- A.char 'K'
    _ <- A.space
    t <- parseMaybeParam
    _ <- A.char ','
    k <- A.decimal
    _ <- A.char ','
    l <- A.decimal
    _ <- A.char ','
    n <- parseMaybeParam
    A.endOfLine
    return $ JoinPopSizeEventTemplate t k l n

parseGrowthRateEvent :: A.Parser EventTemplate
parseGrowthRateEvent = do
    _ <- A.char 'R'
    _ <- A.space
    t <- parseMaybeParam
    _ <- A.char ','
    k <- A.decimal
    _ <- A.char ','
    n <- parseMaybeParam
    A.endOfLine
    return $ GrowthRateEventTemplate t k n

parseMigrationRateEvent :: A.Parser EventTemplate
parseMigrationRateEvent = do
    _ <- A.char 'M'
    _ <- A.space
    t <- parseMaybeParam
    _ <- A.char ','
    k <- A.decimal
    _ <- A.char ','
    l <- A.decimal
    _ <- A.char ','
    n <- parseMaybeParam
    A.endOfLine
    return $ MigrationRateEventTemplate t k l n

parseConstraints :: A.Parser [ConstraintTemplate]
parseConstraints = A.many' $ (A.try parseSmallerConstraint <|> parseGreaterConstraint)
  where
    parseSmallerConstraint = do
        _ <- A.char 'C'
        _ <- A.space
        name1 <- parseMaybeParam
        _ <- A.char '<'
        name2 <- parseMaybeParam
        A.endOfLine
        return $ SmallerConstraintTemplate name1 name2
    parseGreaterConstraint = do
        _ <- A.char 'C'
        _ <- A.space
        name1 <- parseMaybeParam
        _ <- A.char '>'
        name2 <- parseMaybeParam
        A.endOfLine
        return $ GreaterConstraintTemplate name1 name2

instantiateModel :: ModelTemplate -> V.Vector Double -> Either String ModelSpec
instantiateModel (ModelTemplate pNames theta timeSteps ets cts) params = do
    let params' = V.toList params
    events <- mapM (instantiateEvent pNames params') ets
    mapM_ (validateConstraint pNames params') cts
    return $ ModelSpec timeSteps theta (concat events)

instantiateEvent :: [String] -> [Double] -> EventTemplate -> Either String [ModelEvent]
instantiateEvent pnames params et = do
    case et of
        PopSizeEventTemplate t k n -> do
            t' <- getMaybeParam t
            n' <- getMaybeParam n
            return [ModelEvent t' (SetPopSize k n')]
        JoinEventTemplate t k l -> do
            t' <- getMaybeParam t
            return [ModelEvent t' (Join k l)]
        SplitEventTemplate t k l m -> do
            t' <- getMaybeParam t
            m' <- getMaybeParam m
            return [ModelEvent t' (Split k l m')]
        JoinPopSizeEventTemplate t k l n -> do
            t' <- getMaybeParam t
            n' <- getMaybeParam n
            return [ModelEvent t' (Join k l), ModelEvent t' (SetPopSize k n')]
        GrowthRateEventTemplate t k r -> do
            t' <- getMaybeParam t
            r' <- getMaybeParam r
            return [ModelEvent t' (SetGrowthRate k r')]
        MigrationRateEventTemplate t k l r -> do
            t' <- getMaybeParam t
            r' <- getMaybeParam r
            return [ModelEvent t' (SetMigration k l r')]
  where
    getMaybeParam = substituteParam pnames params

substituteParam :: [String] -> [Double] -> Either Double String -> Either String Double
substituteParam _ _ (Left val) = Right val
substituteParam pnames params (Right param) = do
    when (length params /= length pnames) $ Left "number of parameter values does not match number of parameters in template"
    case foundParam of
        Nothing -> Left $ "Error in Template: could not find parameter named " ++ param
        Just val -> Right val
  where
    foundParam = lookup param (zip pnames params)

validateConstraint :: [String] -> [Double] -> ConstraintTemplate -> Either String ()
validateConstraint pNames params ct =
    case ct of
        SmallerConstraintTemplate maybeParam1 maybeParam2 -> do
            p1 <- substituteParam pNames params maybeParam1
            p2 <- substituteParam pNames params maybeParam2
            assertErr ("Constrained failed: " ++ show p1 ++ " < " ++ show p2) (p1 < p2)
        GreaterConstraintTemplate maybeParam1 maybeParam2 -> do
            p1 <- substituteParam pNames params maybeParam1
            p2 <- substituteParam pNames params maybeParam2
            assertErr ("Constrained failed: " ++ show p1 ++ " > " ++ show p2) (p1 > p2)

getModelSpec :: ModelDesc -> Double -> Int -> Script ModelSpec
getModelSpec modelDesc theta lingen =
    case modelDesc of
        ModelDescTemplate path paramsDesc -> do
            template <- readModelTemplate path theta times
            x' <- getInitialParams template paramsDesc
            tryRight $ instantiateModel template x'
        ModelDescDirect events ->
            return $ ModelSpec times theta events
  where
    times = getTimeSteps 20000 lingen 20.0