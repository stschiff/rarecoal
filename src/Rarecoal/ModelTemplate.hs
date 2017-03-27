module Rarecoal.ModelTemplate (getInitialParams, ModelTemplate(..),
                               readModelTemplate,
                               instantiateModel, ModelDesc(..),
                               getModelSpec, EventTemplate(..),
                               BranchSpec, makeFixedParamsTemplate,
                               reportGhostPops, ConstraintTemplate(..)) where

import           Rarecoal.Core        (EventType (..), ModelEvent (..),
                                       ModelSpec (..), getTimeSteps,
                                       getNrOfPops)

import           Control.Applicative  ((<|>))
import           Control.Error        (Script, assertErr, justErr, scriptIO,
                                       tryRight, errLn)
import           Control.Monad        (forM, when)
import           Control.Monad.Except (throwError)
import qualified Data.Attoparsec.Text as A
import           Data.Char            (isAlphaNum)
-- import Debug.Trace (trace)
import           Data.List            (elemIndex)
import           Data.Text            (unpack)
import qualified Data.Text.IO         as T
import qualified Data.Vector.Unboxed  as V
import           System.Log.Logger    (infoM)

data ModelTemplate = ModelTemplate {
    mtParams              :: [String],
    mtTheta               :: Double,
    mtTimeSteps           :: [Double],
    mtDiscoveryRate       :: [(BranchSpec, ParamSpec)],
    mtPopSizeReg          :: Double,
    mtEventTemplates      :: [EventTemplate],
    mtConstraintTemplates :: [ConstraintTemplate]
} deriving (Eq, Show)

type ParamSpec = Either Double String
type BranchSpec = Either Int String

data EventTemplate =
    JoinEventTemplate ParamSpec BranchSpec BranchSpec |
    SplitEventTemplate ParamSpec BranchSpec BranchSpec ParamSpec |
    PopSizeEventTemplate ParamSpec BranchSpec ParamSpec |
    JoinPopSizeEventTemplate ParamSpec BranchSpec BranchSpec ParamSpec
    deriving (Eq, Show)

data ConstraintTemplate =
    SmallerConstraintTemplate ParamSpec ParamSpec |
    GreaterConstraintTemplate ParamSpec ParamSpec
    deriving (Eq, Show)

data ModelDesc = ModelDesc {
    mdDiscoveryRates :: [(String, Double)],
    mdReg :: Double,
    mdTheta :: Double,
    mdLinGen :: Int,
    mdEvents :: [ModelEvent],
    mdMaybeTemplate :: Maybe (FilePath, Maybe FilePath, [(String, Double)])
}

-- type ParamsDesc = Either (FilePath, [(String, Double)]) [Double]

getInitialParams :: ModelTemplate -> Maybe FilePath -> [(String, Double)] ->
    Script (V.Vector Double)
getInitialParams modelTemplate maybeInputFile paramsList =
    case maybeInputFile of
        Just paramsFile -> do
            l <- lines <$> (scriptIO . readFile $ paramsFile)
            ret <- if (head . head $ l) == '#'
                then -- aha, have rarecoal mcmc output file
                    loadFromDict
                        [(k, read $ v !! 2) | (k : v) <- map words . drop 3 $ l]
                else -- aha, have rarecoal maxl output file
                    loadFromDict
                        [(k, read v) | [k, v] <- map words l]
            scriptIO . infoM "rarecoal" $ "initial parameters: " ++ show ret
            return ret
        Nothing -> loadFromDict []
  where
    loadFromDict :: [(String, Double)] -> Script (V.Vector Double)
    loadFromDict dict = V.fromList <$> mapM (paramLookup dict) (mtParams modelTemplate)
    paramLookup :: [(String, Double)] -> String -> Script Double
    paramLookup dict p =
        case p `lookup` paramsList of
            Just x -> return x
            Nothing -> do
                case p `lookup` dict of
                    Just v -> return v
                    Nothing -> do
                        if head p == 'p'
                        then do
                            scriptIO $ errLn $ "assuming " ++ p ++
                                " denotes a population size. Initializing to 1"
                            return 1.0
                        else
                            if take 3 p == "adm"
                            then do
                                scriptIO $ errLn $ "assuming " ++ p ++
                                    " denotes an admixture rate. Initializing to 0.05"
                                return 0.05
                            else
                                throwError $ "Don't know how to initialize parameter " ++ p ++
                                    ". Please provide initial value via -X " ++ p ++ "=?"
                            
readModelTemplate :: FilePath -> Double -> [Double] -> Double ->
    Script ModelTemplate
readModelTemplate path theta timeSteps reg = do
    c <- scriptIO $ T.readFile path
    (names, discoveryRates, events, constraints) <- tryRight $
        A.parseOnly (parseModelTemplate <* A.endOfInput) c
    scriptIO $ errLn "found the following parameters in the model template:"
    scriptIO $ mapM_ errLn names
    return $
        ModelTemplate names theta timeSteps discoveryRates reg events
            constraints

reportGhostPops :: ModelTemplate -> [String] -> V.Vector Double -> Script ()
reportGhostPops modelTemplate branchNames x = do
    modelSpec <- tryRight $ instantiateModel modelTemplate x branchNames
    nrPops <- tryRight $ getNrOfPops (mEvents modelSpec)
    when (length branchNames /= nrPops) . scriptIO $
        errLn ("found " ++ show (nrPops - length branchNames) ++
        " ghost populations")

parseModelTemplate :: A.Parser ([String], [(BranchSpec, ParamSpec)],
    [EventTemplate], [ConstraintTemplate])
parseModelTemplate = do
    discoveryRates <- A.many' parseDiscoveryRate
    events <- parseEvents
    constraints <- parseConstraints
    let names = pullOutAllParamNames discoveryRates events
    return (names, discoveryRates, events, constraints)

pullOutAllParamNames :: [(BranchSpec, ParamSpec)] -> [EventTemplate] -> [String]
pullOutAllParamNames dRates eTemplates = go [] dRates eTemplates
  where
    go res (dR:dRs) ets = case dR of
        (_, Left _) -> go res dRs ets
        (_, Right n) -> go (n:res) dRs ets
    go res [] (et:ets) = case et of
        JoinEventTemplate (Left _) _ _                     -> go res [] ets
        JoinEventTemplate (Right n) _ _                    -> go (n:res) [] ets
        SplitEventTemplate (Left _) _ _ (Left _)           -> go res [] ets
        SplitEventTemplate (Left _) _ _ (Right n)          -> go (n:res) [] ets
        SplitEventTemplate (Right n) _ _ (Left _)          -> go (n:res) [] ets
        SplitEventTemplate (Right n1) _ _ (Right n2)       -> go (n1:n2:res) [] ets
        PopSizeEventTemplate (Left _) _ (Left _)           -> go res [] ets 
        PopSizeEventTemplate (Left _) _ (Right n)          -> go (n:res) [] ets 
        PopSizeEventTemplate (Right n) _ (Left _)          -> go (n:res) [] ets 
        PopSizeEventTemplate (Right n1) _ (Right n2)       -> go (n1:n2:res) [] ets 
        JoinPopSizeEventTemplate (Left _) _ _ (Left _)     -> go res [] ets
        JoinPopSizeEventTemplate (Left _) _ _ (Right n)    -> go (n:res) [] ets
        JoinPopSizeEventTemplate (Right n) _ _ (Left _)    -> go (n:res) [] ets
        JoinPopSizeEventTemplate (Right n1) _ _ (Right n2) -> go (n1:n2:res) [] ets
    go res [] [] = res

parseDiscoveryRate :: A.Parser (BranchSpec, ParamSpec)
parseDiscoveryRate = do
    _ <- A.char 'D'
    _ <- A.space
    k <- parseEitherBranch
    _ <- A.char ','
    d <- parseEitherParam
    A.endOfLine
    return (k, d)

parseParamName :: A.Parser String
parseParamName = unpack <$> A.takeWhile (\c -> isAlphaNum c || c == '_')

parseEvents :: A.Parser [EventTemplate]
parseEvents = A.many' (parsePopSizeEvent <|> parseJoinEvent <|>
    parseSplitEvent <|> parseJoinPopSizeEvent)

parsePopSizeEvent :: A.Parser EventTemplate
parsePopSizeEvent = PopSizeEventTemplate <$> (A.char 'P' *> A.space *>
    parseEitherParam) <* A.char ',' <*> parseEitherBranch <* A.char ',' <*>
    parseEitherParam <* A.endOfLine

parseEitherParam :: A.Parser ParamSpec
parseEitherParam = (Left <$> A.double) <|>
    (Right <$> (A.char '<' *> parseParamName <* A.char '>'))

parseEitherBranch :: A.Parser BranchSpec
parseEitherBranch = (Left <$> A.decimal) <|>
    (Right <$> (A.char '"' *> parseBranchName <* A.char '"'))
  where
    parseBranchName = parseParamName

parseJoinEvent :: A.Parser EventTemplate
parseJoinEvent = JoinEventTemplate <$> (A.char 'J' *> A.space *>
    parseEitherParam) <* A.char ',' <*> parseEitherBranch <* A.char ',' <*>
    parseEitherBranch <* A.endOfLine

parseSplitEvent :: A.Parser EventTemplate
parseSplitEvent = SplitEventTemplate <$> (A.char 'S' *> A.space *>
    parseEitherParam) <* A.char ',' <*> parseEitherBranch <* A.char ',' <*>
    parseEitherBranch <* A.char ',' <*> parseEitherParam <* A.endOfLine

parseJoinPopSizeEvent :: A.Parser EventTemplate
parseJoinPopSizeEvent = JoinPopSizeEventTemplate <$> (A.char 'K' *> A.space *>
    parseEitherParam) <* A.char ',' <*> parseEitherBranch <* A.char ',' <*>
    parseEitherBranch <* A.char ',' <*> parseEitherParam <* A.endOfLine

parseConstraints :: A.Parser [ConstraintTemplate]
parseConstraints = A.many' (A.try parseSmallerConstraint <|>
    parseGreaterConstraint)
  where
    parseSmallerConstraint = SmallerConstraintTemplate <$> (A.char 'C' *>
        A.space *> parseEitherParam) <* A.char '<' <*> parseEitherParam <*
        A.endOfLine
    parseGreaterConstraint = GreaterConstraintTemplate <$> (A.char 'C' *>
        A.space *> parseEitherParam) <* A.char '>' <*> parseEitherParam <*
        A.endOfLine

instantiateModel :: ModelTemplate -> V.Vector Double -> [String] ->
    Either String ModelSpec
instantiateModel mt params branchNames = do
    let (ModelTemplate pNames theta timeSteps drates reg ets cts) = mt
        params' = V.toList params
    events <- concat <$> mapM (instantiateEvent pNames params' branchNames) ets
    nrPops <- getNrOfPops events
    dr <- do
        indexValuePairs <-
            mapM (instantiateDiscoveryRates pNames params' branchNames) drates
        return . V.toList $ V.replicate nrPops 1.0 V.//
            indexValuePairs
    mapM_ (validateConstraint pNames params') cts
    return $ ModelSpec timeSteps theta dr reg events

instantiateDiscoveryRates :: [String] -> [Double] -> [String] ->
    (BranchSpec, ParamSpec) -> Either String (Int, Double)
instantiateDiscoveryRates pnames params branchNames (k, r) = do
    k' <- substituteBranch branchNames k
    r' <- substituteParam pnames params r
    return (k', r')

instantiateEvent :: [String] -> [Double] -> [String] -> EventTemplate ->
    Either String [ModelEvent]
instantiateEvent pnames params branchNames et =
    case et of
        PopSizeEventTemplate t k n -> do
            (t', k', n') <- (,,) <$> getEitherParam t <*> getEitherBranch k <*>
                getEitherParam n
            return [ModelEvent t' (SetPopSize k' n')]
        JoinEventTemplate t k l -> do
            (t', k', l') <- (,,) <$> getEitherParam t <*> getEitherBranch k <*>
                getEitherBranch l
            return [ModelEvent t' (Join k' l')]
        SplitEventTemplate t k l m -> do
            (t', k', l', m') <- (,,,) <$> getEitherParam t <*>
                getEitherBranch k <*> getEitherBranch l <*> getEitherParam m
            return [ModelEvent t' (Split k' l' m')]
        JoinPopSizeEventTemplate t k l n -> do
            (t', k', l', n') <- (,,,) <$> getEitherParam t <*>
                getEitherBranch k <*> getEitherBranch l <*> getEitherParam n
            return [ModelEvent t' (Join k' l'),
                ModelEvent t' (SetPopSize k' n')]
  where
    getEitherParam = substituteParam pnames params
    getEitherBranch = substituteBranch branchNames

substituteBranch :: [String] -> BranchSpec -> Either String Int
substituteBranch branchNames branchSpec = case branchSpec of
    Left k' -> return k'
    Right n -> justErr ("did not find branch name " ++ n) $
        elemIndex n branchNames

substituteParam :: [String] -> [Double] -> ParamSpec -> Either String Double
substituteParam _ _ (Left val) = Right val
substituteParam pnames params (Right param) = do
    when (length params /= length pnames) $ Left ("number of parameter values \
        \does not match number of parameters in template. Values: " ++ show params ++ ", parameters: " ++ show pnames)
    case foundParam of
        Nothing -> Left $ "Error in Template: could not find parameter \
            \named " ++ param
        Just val -> Right val
  where
    foundParam = lookup param (zip pnames params)

validateConstraint :: [String] -> [Double] -> ConstraintTemplate ->
    Either String ()
validateConstraint pNames params ct =
    case ct of
        SmallerConstraintTemplate maybeParam1 maybeParam2 -> do
            p1 <- substituteParam pNames params maybeParam1
            p2 <- substituteParam pNames params maybeParam2
            assertErr ("Constrained failed: " ++ show p1 ++ " < " ++ show p2)
                (p1 < p2)
        GreaterConstraintTemplate maybeParam1 maybeParam2 -> do
            p1 <- substituteParam pNames params maybeParam1
            p2 <- substituteParam pNames params maybeParam2
            assertErr ("Constrained failed: " ++ show p1 ++ " > " ++ show p2)
                (p1 > p2)

getModelSpec :: ModelDesc -> [String] -> Script ModelSpec
getModelSpec modelDesc branchNames = do
    let ModelDesc discoveryRates reg theta lingen events maybeTemplate =
            modelDesc
    indexValuePairs <- forM discoveryRates $ \(branchName, rate) -> do
        k <- tryRight $ substituteBranch branchNames (Right branchName)
        return (k, rate)
    (events', dr') <- case maybeTemplate of
        Nothing -> do
            nrPops <- tryRight $ getNrOfPops events
            let dr = V.toList $
                    V.replicate nrPops 1.0 V.// indexValuePairs
            return (events, dr)
        Just (mtF, iF, params) -> do
            template <- readModelTemplate mtF theta (times lingen) reg
            x' <- getInitialParams template iF params
            modelSpec <-
                tryRight $ instantiateModel template x' branchNames
            let e = mEvents modelSpec
                d = V.fromList $ mDiscoveryRates modelSpec
                d' = V.toList $ d V.// indexValuePairs
            return (e ++ events, d')
    let modelSpec = ModelSpec (times lingen) theta dr' reg events'
    nrPops <- tryRight $ getNrOfPops (mEvents modelSpec)
    when (nrPops /= length branchNames) . scriptIO $
        errLn ("found " ++ show (nrPops - length branchNames) ++
            " ghost populations")
    return modelSpec
  where
    times lingen = getTimeSteps 20000 lingen 20.0

makeFixedParamsTemplate :: ModelTemplate -> [String] -> V.Vector Double ->
    Either String ModelTemplate
makeFixedParamsTemplate modelTemplate fixedParams initialValues = do
    let p = mtParams modelTemplate
        et = mtEventTemplates modelTemplate
        ct = mtConstraintTemplates modelTemplate
    newEventTemplates <- mapM convertEventTemplate et
    newConstraintTemplates <- mapM convertConstraintTemplate ct
    let newParams = filter (`notElem` fixedParams) p
    return $ modelTemplate {
        mtParams = newParams,
        mtEventTemplates = newEventTemplates,
        mtConstraintTemplates = newConstraintTemplates
    }
  where
    convertEventTemplate et =
        case et of
            PopSizeEventTemplate t k n -> do
                newT <- maybeFixParam t
                newN <- maybeFixParam n
                return $ PopSizeEventTemplate newT k newN
            JoinEventTemplate t k l -> do
                newT <- maybeFixParam t
                return $ JoinEventTemplate newT k l
            SplitEventTemplate t k l m -> do
                newT <- maybeFixParam t
                newM <- maybeFixParam m
                return $ SplitEventTemplate newT k l newM
            JoinPopSizeEventTemplate t k l n -> do
                newT <- maybeFixParam t
                newN <- maybeFixParam n
                return $ JoinPopSizeEventTemplate newT k l newN
    maybeFixParam paramSpec =
        case paramSpec of
            Left _ -> return paramSpec
            Right pname ->
                if   pname `elem` fixedParams
                then Left <$> substituteParam (mtParams modelTemplate)
                    (V.toList initialValues) paramSpec
                else return paramSpec
    convertConstraintTemplate ct =
        case ct of
            SmallerConstraintTemplate ps1 ps2 ->
                SmallerConstraintTemplate <$> maybeFixParam ps1 <*>
                    maybeFixParam ps2
            GreaterConstraintTemplate ps1 ps2 ->
                GreaterConstraintTemplate <$> maybeFixParam ps1 <*>
                    maybeFixParam ps2
