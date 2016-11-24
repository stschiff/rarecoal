module Rarecoal.ModelTemplate (getInitialParams, ModelTemplate(..), readModelTemplate, 
                               instantiateModel, ModelDesc(..),
                               getModelSpec, EventTemplate(..), ParamsDesc, BranchSpec) where

import Rarecoal.Core (getTimeSteps, ModelSpec(..), ModelEvent(..), EventType(..))

import Control.Applicative ((<|>))
import Control.Error (Script, scriptIO, tryRight, tryJust, assertErr, justErr)
import Control.Monad (when, forM)
import qualified Data.Attoparsec.Text as A
import Data.Char (isAlphaNum)
-- import Debug.Trace (trace)
import System.Log.Logger (infoM)
import Data.Text (unpack)
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as V

data ModelTemplate = ModelTemplate {
    mtParams :: [String],
    mtTheta :: Double,
    mtTimeSteps :: [Double],
    mtDiscoveryRate :: [(BranchSpec, ParamSpec)],
    mtEventTemplates :: [EventTemplate],
    mtConstraintTemplates :: [ConstraintTemplate]
} deriving (Eq, Show)

type ParamSpec = Either Double String
type BranchSpec = Either Int String

data EventTemplate =
    JoinEventTemplate ParamSpec BranchSpec BranchSpec |
    SplitEventTemplate ParamSpec BranchSpec BranchSpec ParamSpec |
    PopSizeEventTemplate ParamSpec BranchSpec ParamSpec |
    JoinPopSizeEventTemplate ParamSpec BranchSpec BranchSpec ParamSpec |
    GrowthRateEventTemplate ParamSpec BranchSpec ParamSpec |
    MigrationRateEventTemplate ParamSpec BranchSpec BranchSpec ParamSpec
    deriving (Eq, Show)

data ConstraintTemplate =
    SmallerConstraintTemplate ParamSpec ParamSpec |
    GreaterConstraintTemplate ParamSpec ParamSpec
    deriving (Eq, Show)

data ModelDesc =
    ModelDescTemplate FilePath ParamsDesc [ModelEvent] |
    ModelDescDirect [(String, Double)] [ModelEvent] 
type ParamsDesc = Either (FilePath, [(String, Double)]) [Double]

getInitialParams :: ModelTemplate -> ParamsDesc -> Script (V.Vector Double)
getInitialParams modelTemplate paramsDesc = do
    case paramsDesc of
        Left (paramsFile, additionalParams) -> do
            l <- lines <$> (scriptIO . readFile $ paramsFile)
            ret <- if (head . head $ l) == '#'
                then -- aha, have rarecoal mcmc output file
                    loadFromDict
                        [(k, read $ v !! 2) | (k : v) <- map words . drop 3 $ l] 
                        additionalParams
                else -- aha, have rarecoal maxl output file
                    loadFromDict
                        [(k, read v) | [k, v] <- map words $ l] additionalParams
            scriptIO . infoM "rarecoal" $ "initial parameters: " ++ show ret
            return ret
        Right x -> return . V.fromList $ x
  where
    loadFromDict dict additionalParams =
        fmap V.fromList . forM (mtParams modelTemplate) $ \p -> do
            case p `lookup` additionalParams of
                Just x -> return x
                Nothing -> do
                    let err' = "parameter " ++ show p ++
                            " in the Model Template not set"
                    tryJust err' $ p `lookup` dict

readModelTemplate :: FilePath -> Double -> [Double] -> Script ModelTemplate
readModelTemplate path theta timeSteps = do
    c <- scriptIO $ T.readFile path
    (names, discoveryRates, events, constraints) <- tryRight $
        A.parseOnly (parseModelTemplate <* A.endOfInput) c
    return $
        ModelTemplate names theta timeSteps discoveryRates events constraints

parseModelTemplate :: A.Parser ([String], [(BranchSpec, ParamSpec)],
    [EventTemplate], [ConstraintTemplate])
parseModelTemplate = do
    params <- parseParams
    discoveryRates <- A.many' parseDiscoveryRate
    events <- parseEvents
    constraints <- parseConstraints
    return (params, discoveryRates, events, constraints)

parseParams :: A.Parser [String]
parseParams = do
    names <- A.sepBy parseParamName (A.char ',')
    A.endOfLine
    return names

parseParamName :: A.Parser String
parseParamName = unpack <$> A.takeWhile (\c -> isAlphaNum c || c == '_')

parseDiscoveryRate :: A.Parser (BranchSpec, ParamSpec)
parseDiscoveryRate = do
    _ <- A.char 'D'
    _ <- A.space
    k <- parseEitherBranch
    _ <- A.char ','
    d <- parseEitherParam
    A.endOfLine
    return (k, d) 

parseEvents :: A.Parser [EventTemplate]
parseEvents = A.many' (parsePopSizeEvent <|> parseJoinEvent <|> parseSplitEvent <|> 
                       parseJoinPopSizeEvent <|> parseGrowthRateEvent <|> parseMigrationRateEvent)

parsePopSizeEvent :: A.Parser EventTemplate
parsePopSizeEvent = do
    _ <- A.char 'P'
    _ <- A.space
    t <- parseEitherParam
    _ <- A.char ','
    k <- parseEitherBranch
    _ <- A.char ','
    n <- parseEitherParam
    A.endOfLine
    return $ PopSizeEventTemplate t k n

parseEitherParam :: A.Parser ParamSpec
parseEitherParam = (Left <$> A.double) <|> (Right <$> (A.char '<' *> parseParamName <* A.char '>')) 

parseEitherBranch :: A.Parser BranchSpec
parseEitherBranch = (Left <$> A.decimal) <|> (Right <$> (A.char '"' *> parseBranchName <* A.char '"'))
  where
    parseBranchName = parseParamName

parseJoinEvent :: A.Parser EventTemplate
parseJoinEvent = do
    _ <- A.char 'J'
    _ <- A.space
    t <- parseEitherParam
    _ <- A.char ','
    k <- parseEitherBranch
    _ <- A.char ','
    l <- parseEitherBranch
    A.endOfLine
    return $ JoinEventTemplate t k l

parseSplitEvent :: A.Parser EventTemplate
parseSplitEvent = do
    _ <- A.char 'S'
    _ <- A.space
    t <- parseEitherParam
    _ <- A.char ','
    k <- parseEitherBranch
    _ <- A.char ','
    l <- parseEitherBranch
    _ <- A.char ','
    m <- parseEitherParam
    A.endOfLine
    return $ SplitEventTemplate t k l m

parseJoinPopSizeEvent :: A.Parser EventTemplate
parseJoinPopSizeEvent = do
    _ <- A.char 'K'
    _ <- A.space
    t <- parseEitherParam
    _ <- A.char ','
    k <- parseEitherBranch
    _ <- A.char ','
    l <- parseEitherBranch
    _ <- A.char ','
    n <- parseEitherParam
    A.endOfLine
    return $ JoinPopSizeEventTemplate t k l n

parseGrowthRateEvent :: A.Parser EventTemplate
parseGrowthRateEvent = do
    _ <- A.char 'R'
    _ <- A.space
    t <- parseEitherParam
    _ <- A.char ','
    k <- parseEitherBranch
    _ <- A.char ','
    n <- parseEitherParam
    A.endOfLine
    return $ GrowthRateEventTemplate t k n

parseMigrationRateEvent :: A.Parser EventTemplate
parseMigrationRateEvent = do
    _ <- A.char 'M'
    _ <- A.space
    t <- parseEitherParam
    _ <- A.char ','
    k <- parseEitherBranch
    _ <- A.char ','
    l <- parseEitherBranch
    _ <- A.char ','
    n <- parseEitherParam
    A.endOfLine
    return $ MigrationRateEventTemplate t k l n

parseConstraints :: A.Parser [ConstraintTemplate]
parseConstraints = A.many' $ (A.try parseSmallerConstraint <|> parseGreaterConstraint)
  where
    parseSmallerConstraint = do
        _ <- A.char 'C'
        _ <- A.space
        name1 <- parseEitherParam
        _ <- A.char '<'
        name2 <- parseEitherParam
        A.endOfLine
        return $ SmallerConstraintTemplate name1 name2
    parseGreaterConstraint = do
        _ <- A.char 'C'
        _ <- A.space
        name1 <- parseEitherParam
        _ <- A.char '>'
        name2 <- parseEitherParam
        A.endOfLine
        return $ GreaterConstraintTemplate name1 name2

instantiateModel :: ModelTemplate -> V.Vector Double -> [String] ->
    Either String ModelSpec
instantiateModel (ModelTemplate pNames theta timeSteps drates ets cts) params
        branchNames = do
    let params' = V.toList params
    dr <- do
        indexValuePairs <-
            mapM (instantiateDiscoveryRates pNames params' branchNames) drates
        return . V.toList $ V.replicate (length pNames) 1.0 V.// indexValuePairs
    events <- mapM (instantiateEvent pNames params' branchNames) ets
    mapM_ (validateConstraint pNames params') cts
    return $ ModelSpec timeSteps theta dr (concat events)

instantiateDiscoveryRates :: [String] -> [Double] -> [String] ->
    (BranchSpec, ParamSpec) -> Either String (Int, Double)
instantiateDiscoveryRates pnames params branchNames (k, r) = do
    k' <- substituteBranch branchNames k
    r' <- substituteParam pnames params r
    return (k', r')

instantiateEvent :: [String] -> [Double] -> [String] -> EventTemplate -> Either String [ModelEvent]
instantiateEvent pnames params branchNames et = do
    case et of
        PopSizeEventTemplate t k n -> do
            t' <- getEitherParam t
            k' <- getEitherBranch k
            n' <- getEitherParam n
            return [ModelEvent t' (SetPopSize k' n')]
        JoinEventTemplate t k l -> do
            t' <- getEitherParam t
            k' <- getEitherBranch k
            l' <- getEitherBranch l
            return [ModelEvent t' (Join k' l')]
        SplitEventTemplate t k l m -> do
            t' <- getEitherParam t
            k' <- getEitherBranch k
            l' <- getEitherBranch l
            m' <- getEitherParam m
            return [ModelEvent t' (Split k' l' m')]
        JoinPopSizeEventTemplate t k l n -> do
            t' <- getEitherParam t
            k' <- getEitherBranch k
            l' <- getEitherBranch l
            n' <- getEitherParam n
            return [ModelEvent t' (Join k' l'), ModelEvent t' (SetPopSize k' n')]
        GrowthRateEventTemplate t k r -> do
            t' <- getEitherParam t
            k' <- getEitherBranch k
            r' <- getEitherParam r
            return [ModelEvent t' (SetGrowthRate k' r')]
        MigrationRateEventTemplate t k l r -> do
            t' <- getEitherParam t
            k' <- getEitherBranch k
            l' <- getEitherBranch l
            r' <- getEitherParam r
            return [ModelEvent t' (SetMigration k' l' r')]
  where
    getEitherParam = substituteParam pnames params
    getEitherBranch k = substituteBranch branchNames k

substituteBranch :: [String] -> BranchSpec -> Either String Int
substituteBranch branchNames branchSpec = case branchSpec of
    Left k' -> return k'
    Right n -> justErr ("did not find branch name " ++ n) $
        lookup n (zip branchNames [0..])

substituteParam :: [String] -> [Double] -> ParamSpec -> Either String Double
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

getModelSpec :: ModelDesc -> [String] -> Double -> Int -> Script ModelSpec
getModelSpec modelDesc branchNames theta lingen =
    case modelDesc of
        ModelDescTemplate path paramsDesc additionalEvents -> do
            template <- readModelTemplate path theta times
            x' <- getInitialParams template paramsDesc
            modelSpec <- tryRight $ instantiateModel template x' branchNames
            let e = mEvents modelSpec
            return $ modelSpec {mEvents = e ++ additionalEvents}
        ModelDescDirect discoveryRates events -> do
            indexValuePairs <- forM discoveryRates $ \(branchName, rate) -> do
                k <- tryRight $ substituteBranch branchNames (Right branchName)
                return (k, rate)
            let dr = V.toList $
                    V.replicate (length branchNames) 1.0 V.// indexValuePairs 
            return $ ModelSpec times theta dr events
  where
    times = getTimeSteps 20000 lingen 20.0
