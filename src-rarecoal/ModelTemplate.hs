module ModelTemplate (getInitialParams, ModelTemplate(..), readModelTemplate, instantiateModel,
                      getModelSpec) where

import Rarecoal.Core (getTimeSteps, ModelSpec(..), ModelEvent(..), EventType(..))

import Control.Applicative ((<|>))
import Control.Error (Script, scriptIO, tryRight, readErr, justErr, tryJust, throwE, assertErr)
import Control.Monad (unless)
import qualified Data.Attoparsec.Text as A
import System.Log.Logger (infoM)
import Data.List.Split (splitOn)
import Data.String.Utils (replace)
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as V

data ModelTemplate = ModelTemplate {
    mtParams :: [String],
    mtTheta :: Double,
    mtTimeSteps :: [Double],
    mtEventTemplates :: [EventTemplate],
    mtConstraintTemplates :: [ConstraintTemplate]
}

data EventTemplate = JoinEventTemplate (Either Double String) Int Int
                   | PopSizeEventTemplate (Either Double String) Int (Either Double String)
                   | JoinPopSizeEventTemplate (Either Double String) Int Int (Either Double String)
                   | GrowthRateEventTemplate (Either Double String) Int (Either Double String)
                   | MigrationRateEventTemplate (Either Double String) Int Int (Either Double String)

data ConstraintTemplate = SmallerConstraintTemplate String String
                        | GreaterConstraintTemplate String String

getInitialParams :: ModelTemplate -> FilePath -> [Double] -> Script (V.Vector Double)
getInitialParams modelTemplate paramsFile x = do
    if paramsFile == "/dev/null" then return . V.fromList $ x else do
        l <- lines <$> (scriptIO . readFile $ paramsFile)
        ret <- if (head . head $ l) == '#' then
                loadFromDict [(k, read $ v !! 2) | (k : v) <- map words . drop 3 $ l]
            else
                loadFromDict [(k, read v) | [k, v] <- map words $ l]
        scriptIO . infoM "rarecoal" $ "initial parameters loaded: " ++ show ret
        return ret
  where
    loadFromDict dict = do
        let err = "parameters in the initialParams-file do not match the parameters in the modelTemplate"
        x' <- tryJust err . mapM (`lookup` dict) $ mtParams modelTemplate
        return . V.fromList $ x'

readModelTemplate :: FilePath -> Double -> [Double] -> Script ModelTemplate
readModelTemplate path theta timeSteps = do
    c <- scriptIO $ T.readFile path
    (names, events, constraints) <- tryRight $ A.parseOnly parseModelTemplate c
    return $ ModelTemplate names theta timeSteps events constraints

parseModelTemplate :: A.Parser ([String], [EventTemplate], [ConstraintTemplate])
parseModelTemplate = do
    params <- parseParams
    events <- parseEvents
    constrains <- parseConstraints
    return (params, events, constrains)

parseParams :: A.Parser [String]
parseParams = do
    names <- A.sepBy parseParamName (A.char ',')
    A.endOfLine
    return names

parseParamName :: A.Parser String
parseParamName = (:) <$> A.letter <*> A.many' (A.letter <|> A.digit)

parseEvents :: A.Parser [EventTemplate]
parseEvents = A.many' (parsePopSizeEvent <|> parseJoinEvent <|> parseJoinPopSizeEvent <|> parseGrowthRateEvent <|> parseMigrationRateEvent)

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

parseMaybeParam :: A.Parser (Either Double String)
parseMaybeParam = do
    c <- A.peekChar'
    if A.inClass "1234567890-+" c then do
        val <- A.double
        return $ Left val
    else do
        p <- parseParamName
        return $ Right p

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
        name1 <- parseParamName
        _ <- A.char '<'
        name2 <- parseParamName
        return $ SmallerConstraintTemplate name1 name2
    parseGreaterConstraint = do
        _ <- A.char 'C'
        _ <- A.space
        name1 <- parseParamName
        _ <- A.char '>'
        name2 <- parseParamName
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
substituteParam pnames params (Right param) =
    case foundParam of
        Nothing -> Left $ "Error in Template: could not find parameter named " ++ param
        Just val -> Right val
  where
    foundParam = lookup param (zip pnames params)

validateConstraint :: [String] -> [Double] -> ConstraintTemplate -> Either String ()
validateConstraint pNames params ct =
    case ct of
        SmallerConstraintTemplate name1 name2 -> do
            p1 <- substituteParam pNames params (Right name1)
            p2 <- substituteParam pNames params (Right name2)
            assertErr ("Constrained failed: " ++ show p1 ++ " < " ++ show p2) (p1 < p2)
        GreaterConstraintTemplate name1 name2 -> do
            p1 <- substituteParam pNames params (Right name1)
            p2 <- substituteParam pNames params (Right name2)
            assertErr ("Constrained failed: " ++ show p1 ++ " > " ++ show p2) (p1 > p2)

getModelSpec :: FilePath -> Double -> FilePath -> [Double] -> [ModelEvent] -> Int -> Script ModelSpec
getModelSpec path theta paramsFile x events lingen =
    let times = getTimeSteps 20000 lingen 20.0
    in  if path /= "/dev/null" then do
            template <- readModelTemplate path theta times
            x' <- getInitialParams template paramsFile x
            tryRight $ instantiateModel template x'
        else
            return $ ModelSpec times theta events
