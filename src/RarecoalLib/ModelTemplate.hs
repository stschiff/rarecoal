{-# LANGUAGE OverloadedStrings #-}
module RarecoalLib.ModelTemplate (ModelOptions(..), ParamOptions(..),
    ModelTemplate(..), MTEvent(..), MTConstraint(..), ParamSpec(..), getModelTemplate,
    instantiateModel, makeParameterDict, getRegularizationPenalty, getParamNames,
    fillParameterDictWithDefaults, ConstraintOperator(..))
where

import           RarecoalLib.Core              (EventType (..), ModelEvent (..),
                                             ModelSpec (..))
import           RarecoalLib.StateSpace        (getRegularizationPenalty)
import           RarecoalLib.Utils             (GeneralOptions (..), ModelBranch,
                                             RarecoalException (..),
                                             getTimeSteps)

import           Control.Applicative        ((<|>))
import           Control.Exception          (throwIO)
import           Control.Monad              (forM, when)
import           Data.Char                  (isAlphaNum)
-- import Debug.Trace (trace)
import           Data.List                  (elemIndex, nub)
import           System.IO                  (hPutStrLn, stderr)
import qualified Text.Parsec                as P
import qualified Text.Parsec.Char           as PC
import           Text.Parsec.Number         (floating)
import           Text.Parsec.String         (Parser)

data ModelOptions = ModelByFile FilePath
    | ModelByText String

data ParamOptions = ParamOptions
    { optMaybeParamInputFile :: Maybe FilePath
    , optParameterSettings   :: [(String, Double)]
    }

data ModelTemplate = ModelTemplate
    { mtBranchNames :: [ModelBranch]
    , mtEvents      :: [MTEvent]
    , mtConstraints :: [MTConstraint]
    }
    deriving (Show, Eq)

data MTEvent = MTPopSizeChange ParamSpec ModelBranch ParamSpec
    | MTJoin ParamSpec ModelBranch ModelBranch
    | MTJoinPopSizeChange ParamSpec ModelBranch ModelBranch ParamSpec
    | MTSplit ParamSpec ModelBranch ModelBranch ParamSpec
    | MTFreeze ParamSpec ModelBranch Bool
    deriving (Eq, Show)

data MTConstraint = MTConstraint ParamSpec ParamSpec ConstraintOperator
    deriving (Eq, Show)
data ParamSpec = ParamFixed Double
    | ParamVariable String
    deriving (Eq, Show)

data ConstraintOperator = ConstraintOpGreater
    | ConstraintOpSmaller
    deriving (Eq, Show)

getModelTemplate :: ModelOptions -> IO ModelTemplate
getModelTemplate mo = do
    mt <- case mo of
        ModelByFile fp -> do
            input' <- readFile fp
            case P.parse modelTemplateP "" input' of
                Left e  -> throwIO $ RarecoalModeltemplateException (show e)
                Right v -> return v
        ModelByText input -> case P.parse modelTemplateP "" input of
            Left e  -> throwIO $ RarecoalModeltemplateException (show e)
            Right v -> return v
    reportModelTemplate mt
    return mt
  where
    reportModelTemplate mt@(ModelTemplate n e c) = do
        let paramNames = getParamNames mt
        hPutStrLn stderr ("loaded model template with branch names " ++ show n ++ ", events " ++ show e ++
            ", constraints " ++ show c ++ " and parameters " ++ show paramNames)

modelTemplateP :: Parser ModelTemplate
modelTemplateP = ModelTemplate <$> parseBranchNames <*> parseEvents <*>
    (parseConstraints <|> pure [])

parseBranchNames :: Parser [String]
parseBranchNames = PC.string "BRANCHES" *> PC.spaces *> PC.char '=' *> PC.spaces *> PC.char '[' *>
    PC.spaces *> (branchNameP `P.sepBy1` (PC.char ',' *> PC.spaces)) <* PC.spaces <* PC.char ']' <*
    PC.spaces

branchNameP :: Parser String
branchNameP = P.many1 (PC.satisfy (\c -> isAlphaNum c || c == '_'))

parseEvents :: Parser [MTEvent]
parseEvents = PC.string "EVENTS" *> PC.spaces *> PC.char '=' *> PC.spaces *> PC.char '[' *>
    PC.spaces *> (eventP `P.sepBy` (PC.char ';' *> PC.spaces)) <* PC.spaces <* PC.char ']' <*
    PC.spaces

eventP :: Parser MTEvent
eventP = P.try popSizeChangeCmdP <|>
         P.try joinCmdP <|>
         P.try joinPopSizeCmdP <|>
         P.try splitCmdP <|>
         freezeCmdP

parseConstraints :: Parser [MTConstraint]
parseConstraints = PC.string "CONSTRAINTS" *> PC.spaces *> PC.char '=' *> PC.spaces *>
    PC.char '[' *> PC.spaces *> (constraintCmdP `P.sepBy` (PC.char ';' *> PC.spaces)) <*
    PC.spaces <* PC.char ']' <* PC.spaces

popSizeChangeCmdP :: Parser MTEvent
popSizeChangeCmdP = MTPopSizeChange <$> (PC.char 'P' *> spaces1 *> paramSpecP <* spaces1) <*>
    (branchNameP <* spaces1) <*> paramSpecP

paramSpecP :: Parser ParamSpec
paramSpecP = P.try (ParamFixed <$> floating) <|>
             (ParamVariable <$> variableP)

variableP :: Parser String
variableP = PC.char '<' *> P.many1 (PC.satisfy (\c -> isAlphaNum c || c == '_')) <* PC.char '>'

spaces1 :: Parser ()
spaces1 = P.skipMany1 PC.space

joinCmdP :: Parser MTEvent
joinCmdP = MTJoin <$> (PC.char 'J' *> spaces1 *> paramSpecP <* spaces1) <*>
    (branchNameP <* spaces1) <*> branchNameP

joinPopSizeCmdP :: Parser MTEvent
joinPopSizeCmdP = MTJoinPopSizeChange <$> (PC.char 'K' *> spaces1 *> paramSpecP <* spaces1) <*>
    (branchNameP <* spaces1) <*> (branchNameP <* spaces1) <*> paramSpecP

splitCmdP :: Parser MTEvent
splitCmdP = MTSplit <$> (PC.char 'S' *> spaces1 *> paramSpecP <* spaces1) <*>
    (branchNameP <* spaces1) <*> (branchNameP <* spaces1) <*> paramSpecP

freezeCmdP :: Parser MTEvent
freezeCmdP = MTFreeze <$> (PC.char 'F' *> spaces1 *> paramSpecP <* spaces1) <*>
    (branchNameP <* spaces1) <*> boolP
  where
    boolP = P.try (read <$> PC.string "True") <|> (read <$> PC.string "False")

constraintCmdP :: Parser MTConstraint
constraintCmdP = P.try smallerConstraintP <|> greaterConstraintP
  where
    smallerConstraintP = MTConstraint <$> (PC.char 'C' *> spaces1 *> paramSpecP <* PC.spaces) <*>
        (PC.char '<' *> PC.spaces *> paramSpecP) <*> pure ConstraintOpSmaller
    greaterConstraintP = MTConstraint <$> (PC.char 'C' *> spaces1 *> paramSpecP <* PC.spaces) <*>
        (PC.char '>' *> PC.spaces *> paramSpecP) <*> pure ConstraintOpGreater

getParamNames :: ModelTemplate -> [String]
getParamNames mt = reverse . nub $ go [] (mtEvents mt)
  where
    go res [] = res
    go res (cmd:rest) = case cmd of
        MTPopSizeChange (ParamFixed _) _ (ParamFixed _) -> go res rest
        MTPopSizeChange (ParamVariable n) _ (ParamFixed _) -> go (n:res) rest
        MTPopSizeChange (ParamFixed _) _ (ParamVariable n) -> go (n:res) rest
        MTPopSizeChange (ParamVariable n1) _ (ParamVariable n2) ->
            go (n2:n1:res) rest
        MTJoin (ParamFixed _) _ _ -> go res rest
        MTJoin (ParamVariable n) _ _ -> go (n:res) rest
        MTJoinPopSizeChange (ParamFixed _) _ _ (ParamFixed _) -> go res rest
        MTJoinPopSizeChange (ParamVariable n) _ _ (ParamFixed _) ->
            go (n:res) rest
        MTJoinPopSizeChange (ParamFixed _) _ _ (ParamVariable n) ->
            go (n:res) rest
        MTJoinPopSizeChange (ParamVariable n1) _ _ (ParamVariable n2) ->
            go (n2:n1:res) rest
        MTSplit (ParamFixed _) _ _ (ParamFixed _) -> go res rest
        MTSplit (ParamVariable n) _ _ (ParamFixed _) ->
            go (n:res) rest
        MTSplit (ParamFixed _) _ _ (ParamVariable n) ->
            go (n:res) rest
        MTSplit (ParamVariable n1) _ _ (ParamVariable n2) ->
            go (n2:n1:res) rest
        MTFreeze (ParamVariable n) _ _ -> go (n:res) rest
        _ -> go res rest

instantiateModel :: GeneralOptions -> ModelTemplate -> [(String, Double)] -> Either String ModelSpec
instantiateModel opts (ModelTemplate branchNames _mtEvents _mtConstraints) paramsDict = do
    events <- reverse <$> getEvents branchNames _mtEvents []
    validateConstraints paramsDict _mtConstraints
    let timeSteps = getTimeSteps (optN0 opts) (optLinGen opts) (optTMax opts)
        nrPops = length branchNames
    return $ ModelSpec nrPops timeSteps (optTheta opts) [1.0 | _ <- [0..(nrPops - 1)]]
        (optRegPenalty opts) (optNoShortcut opts) events
  where
    getEvents _ [] res = return res
    getEvents bN (cmd:rest) res = case cmd of
        MTJoin tSpec toSpec fromSpec -> do
            t <- getParam paramsDict tSpec
            e <- Join <$> getModelBranchIndex bN toSpec <*>
                getModelBranchIndex bN fromSpec
            getEvents bN rest (ModelEvent t e:res)
        MTPopSizeChange tSpec bSpec pSpec -> do
            t <- getParam paramsDict tSpec
            e <- SetPopSize <$> getModelBranchIndex bN bSpec <*>
                getParam paramsDict pSpec
            getEvents bN rest (ModelEvent t e:res)
        MTJoinPopSizeChange tSpec toSpec fromSpec pSpec -> do
            t <- getParam paramsDict tSpec
            e1 <- Join <$> getModelBranchIndex bN toSpec <*>
                getModelBranchIndex bN fromSpec
            e2 <- SetPopSize <$> getModelBranchIndex bN toSpec <*>
                getParam paramsDict pSpec
            getEvents bN rest (ModelEvent t e2:ModelEvent t e1:res)
        MTSplit tSpec toSpec fromSpec rateSpec -> do
            t <- getParam paramsDict tSpec
            e <- Split <$> getModelBranchIndex bN toSpec <*>
                getModelBranchIndex bN fromSpec <*>
                getParam paramsDict rateSpec
            getEvents bN rest (ModelEvent t e:res)
        MTFreeze tSpec bSpec v -> do
            t <- getParam paramsDict tSpec
            e <- SetFreeze <$> getModelBranchIndex bN bSpec <*> pure v
            getEvents bN rest (ModelEvent t e:res)

getModelBranchIndex :: [ModelBranch] -> ModelBranch -> Either String Int
getModelBranchIndex branchNames branch =
    case elemIndex branch branchNames of
        Nothing -> Left ("did not find model-branch name " ++ show branch)
        Just v  -> Right v


getParam :: [(String, Double)] -> ParamSpec -> Either String Double
getParam _ (ParamFixed val) = return val
getParam paramsDict (ParamVariable n) =
    case n `lookup` paramsDict of
        Nothing -> Left ("Error in Template: could not find parameter " ++ show n)
        Just val -> return val

validateConstraints :: [(String, Double)] -> [MTConstraint] -> Either String ()
validateConstraints _ [] = return ()
validateConstraints paramsDict (c@(MTConstraint pSpec1 pSpec2 op):rest) = do
    p1 <- getParam paramsDict pSpec1
    p2 <- getParam paramsDict pSpec2
    case op of
        ConstraintOpGreater ->
            when (p1 <= p2) $
                Left ("constraint violated: " ++ show c)
        ConstraintOpSmaller ->
            when (p1 >= p2) $
                Left ("constraint violated: " ++ show c)
    validateConstraints paramsDict rest

makeParameterDict :: ParamOptions -> IO [(String, Double)]
makeParameterDict (ParamOptions maybeInputFile xSettings) =
    case maybeInputFile of
        Just paramsFile -> do
            l <- lines <$> readFile paramsFile
            if (head . head $ l) == '#'
            then -- aha, have rarecoal mcmc output file
                removeDuplicateKeys $
                    xSettings ++ [(k, read $ v !! 0) | (k : v) <- map words . drop 3 $ l]
            else -- aha, have rarecoal maxl output file
                removeDuplicateKeys $
                    xSettings ++ [(k, read v) | [k, v] <- map words l]
        Nothing -> removeDuplicateKeys xSettings
  where
    removeDuplicateKeys settings = go settings []
    go [] res = return res
    go ((key, val):rest) res = case key `lookup` res of
        Nothing -> do
            hPutStrLn stderr ("setting parameter " ++ show key ++ " = " ++ show val)
            go rest ((key, val):res)
        Just _ -> go rest res

fillParameterDictWithDefaults :: ModelTemplate -> [(String, Double)] -> IO [(String, Double)]
fillParameterDictWithDefaults mt paramsDict = do
    let paramNames = getParamNames mt
    forM paramNames $ \p ->
        case p `lookup` paramsDict of
            Just v -> return (p, v)
            Nothing | head p == 'p' -> do
                        hPutStrLn stderr $
                            "setting parameter " ++ p ++ " = 1.0 (default)"
                        return (p, 1.0)
                    | take 3 p == "adm" -> do
                        hPutStrLn stderr $ "setting parameter " ++ p ++ " = 0.05 (default)"
                        return (p, 0.05)
                    | otherwise ->
                        throwIO $ RarecoalModeltemplateException ("Don't know how to initialize \
                            \parameter " ++ p ++ ". Please provide initial \
                            \value via -X " ++ p ++ "=...")

