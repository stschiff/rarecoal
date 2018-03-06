{-# LANGUAGE OverloadedStrings #-}
module Rarecoal.ModelTemplate (ModelOptions(..), ParamOptions(..),
    ModelTemplate(..), MTEvent(..), MTConstraint(..), ParamSpec(..), getModelTemplate, 
    instantiateModel, makeParameterDict, getRegularizationPenalty, getParamNames,
    fillParameterDictWithDefaults, ConstraintOperator(..))
where

import           Rarecoal.Utils                       (getTimeSteps,
                                                       GeneralOptions (..),
                                                       Branch)
import           Rarecoal.Core              (getRegularizationPenalty,
    EventType(..), ModelSpec(..), ModelEvent (..))

import           Control.Applicative                  ((<|>))
import           Control.Error                        (Script, errLn,
                                                       justErr, scriptIO)
import           Control.Monad                        (forM, when)
import           Control.Monad.Trans.Except           (throwE)
import           Data.Char                            (isAlphaNum)
-- import Debug.Trace (trace)
import           Data.List                            (elemIndex, nub)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Text.Parsec                          as P
import qualified Text.Parsec.Char                     as PC
import           Text.Parsec.Number                   (floating, sign)
import           Text.Parsec.Text                     (Parser)
import           Turtle                               (format, w, (%), g)

data ModelOptions = ModelByFile FilePath | ModelByText T.Text

data ParamOptions = ParamOptions {
    optMaybeParamInputFile :: Maybe FilePath,
    optParameterSettings   :: [(T.Text, Double)]
}

data ModelTemplate = ModelTemplate {
    mtBranchNames :: [T.Text],
    mtEvents :: [MTEvent],
    mtConstraints :: [MTConstraint]
} deriving (Show, Eq)

data MTEvent = MTPopSizeChange ParamSpec Branch ParamSpec
             | MTJoin ParamSpec Branch Branch
             | MTJoinPopSizeChange ParamSpec Branch Branch ParamSpec
             | MTSplit ParamSpec Branch Branch ParamSpec
             | MTFreeze ParamSpec Branch Bool deriving (Eq, Show)

data MTConstraint = MTConstraint ParamSpec ParamSpec ConstraintOperator deriving (Eq, Show)               
data ParamSpec = ParamFixed Double | ParamVariable T.Text deriving (Eq, Show)

data ConstraintOperator = ConstraintOpGreater | ConstraintOpSmaller deriving (Eq, Show)

getModelTemplate :: ModelOptions -> Script ModelTemplate
getModelTemplate mo = do
    mt <- case mo of
        ModelByFile fp -> do
            input' <- scriptIO $ T.readFile fp
            case P.parse modelTemplateP "" input' of
                Left e  -> throwE (format w e)
                Right v -> return v
        ModelByText input -> case P.parse modelTemplateP "" input of
            Left e  -> throwE (format w e)
            Right v -> return v
    reportModelTemplate mt
    return mt
  where
    reportModelTemplate mt@(ModelTemplate n e c) = do
        let paramNames = getParamNames mt
        scriptIO . errLn $ format ("loaded model template with branch names "%w%", events "%w%
            ", constraints "%w%" and parameters "%w) n e c paramNames

modelTemplateP :: Parser ModelTemplate
modelTemplateP = ModelTemplate <$> parseBranchNames <*> parseEvents <*>
    (parseConstraints <|> pure [])

parseBranchNames :: Parser [T.Text]
parseBranchNames = PC.string "BRANCHES" *> PC.spaces *> PC.char '=' *> PC.spaces *> PC.char '[' *> 
    PC.spaces *> (branchNameP `P.sepBy1` (PC.char ',' *> PC.spaces)) <* PC.spaces <* PC.char ']' <* 
    PC.spaces

branchNameP :: Parser T.Text
branchNameP = T.pack <$> P.many1 PC.alphaNum

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
paramSpecP = P.try (ParamFixed <$> (sign <*> floating)) <|>
             (ParamVariable <$> variableP)

variableP :: Parser T.Text
variableP = T.pack <$> (PC.char '<' *> P.many1 (PC.satisfy (\c -> isAlphaNum c || c == '_')) <* PC.char '>')

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

getParamNames :: ModelTemplate -> [T.Text]
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

instantiateModel :: GeneralOptions -> ModelTemplate -> [(T.Text, Double)] ->
    Either T.Text ModelSpec
instantiateModel opts (ModelTemplate branchNames _mtEvents _mtConstraints) paramsDict = do
    events <- reverse <$> getEvents branchNames _mtEvents []
    validateConstraints paramsDict _mtConstraints
    let timeSteps = getTimeSteps (optN0 opts) (optLinGen opts) (optTMax opts)
    return $ ModelSpec timeSteps (optTheta opts) [1.0 | _ <- branchNames] (optRegPenalty opts) 
        (optNoShortcut opts) events
  where
    getEvents _ [] res = return res
    getEvents bN (cmd:rest) res = case cmd of
        MTJoin tSpec toSpec fromSpec -> do
            t <- getParam paramsDict tSpec
            e <- Join <$> getBranchIndex bN toSpec <*>
                getBranchIndex bN fromSpec
            getEvents bN rest (ModelEvent t e:res)
        MTPopSizeChange tSpec bSpec pSpec -> do
            t <- getParam paramsDict tSpec
            e <- SetPopSize <$> getBranchIndex bN bSpec <*>
                getParam paramsDict pSpec
            getEvents bN rest (ModelEvent t e:res)
        MTJoinPopSizeChange tSpec toSpec fromSpec pSpec -> do
            t <- getParam paramsDict tSpec
            e1 <- Join <$> getBranchIndex bN toSpec <*>
                getBranchIndex bN fromSpec
            e2 <- SetPopSize <$> getBranchIndex bN toSpec <*>
                getParam paramsDict pSpec
            getEvents bN rest (ModelEvent t e2:ModelEvent t e1:res)
        MTSplit tSpec toSpec fromSpec rateSpec -> do
            t <- getParam paramsDict tSpec
            e <- Split <$> getBranchIndex bN toSpec <*>
                getBranchIndex bN fromSpec <*>
                getParam paramsDict rateSpec
            getEvents bN rest (ModelEvent t e:res)
        MTFreeze tSpec bSpec v -> do
            t <- getParam paramsDict tSpec
            e <- SetFreeze <$> getBranchIndex bN bSpec <*> pure v
            getEvents bN rest (ModelEvent t e:res)

getBranchIndex :: [T.Text] -> Branch -> Either T.Text Int
getBranchIndex branchNames branch = justErr (format ("did not find branch name "%w) branch) $
    elemIndex branch branchNames

getParam :: [(T.Text, Double)] -> ParamSpec -> Either T.Text Double
getParam _ (ParamFixed val) = return val
getParam paramsDict (ParamVariable n) =
    case n `lookup` paramsDict of
        Nothing ->
            Left $ format ("Error in Template: could not find parameter "%w) n
        Just val -> return val

validateConstraints :: [(T.Text, Double)] -> [MTConstraint] -> Either T.Text ()
validateConstraints _ [] = return ()
validateConstraints paramsDict (c@(MTConstraint pSpec1 pSpec2 op):rest) = do
    p1 <- getParam paramsDict pSpec1
    p2 <- getParam paramsDict pSpec2
    case op of
        ConstraintOpGreater ->
            when (p1 <= p2) $
                Left (format ("constraint violated: "%w) c)
        ConstraintOpSmaller ->
            when (p1 >= p2) $
                Left (format ("constraint violated: "%w) c)
    validateConstraints paramsDict rest

makeParameterDict :: ParamOptions -> IO [(T.Text, Double)]
makeParameterDict (ParamOptions maybeInputFile xSettings) =
    case maybeInputFile of
        Just paramsFile -> do
            l <- lines <$> readFile paramsFile
            if (head . head $ l) == '#'
            then -- aha, have rarecoal mcmc output file
                removeDuplicateKeys $
                    xSettings ++ [(T.pack k, read $ v !! 2) | (k : v) <- map words . drop 3 $ l]
            else -- aha, have rarecoal maxl output file
                removeDuplicateKeys $
                    xSettings ++ [(T.pack k, read v) | [k, v] <- map words l]
        Nothing -> removeDuplicateKeys xSettings
  where
    removeDuplicateKeys settings = go settings []
    go [] res = return res
    go ((key, val):rest) res = case key `lookup` res of
        Nothing -> do
            errLn $ format ("setting parameter "%w%" = "%g) key val
            go rest ((key, val):res)
        Just _ -> go rest res

fillParameterDictWithDefaults :: ModelTemplate -> [(T.Text, Double)] ->
    Script [(T.Text, Double)]
fillParameterDictWithDefaults mt paramsDict = do
    let paramNames = getParamNames mt
    forM paramNames $ \p ->
        case p `lookup` paramsDict of
            Just v -> return (p, v)
            Nothing | T.head p == 'p' -> do
                        scriptIO . errLn $
                            format ("setting parameter "%w%" = 1.0 (default)") p
                        return (p, 1.0)
                    | T.take 3 p == "adm" -> do
                        scriptIO . errLn $ format ("setting parameter "%w%
                            " = 0.05 (default)") p
                        return (p, 0.05)
                    | otherwise ->
                        throwE $ format ("Don't know how to initialize \
                        \parameter "%w%". Please provide initial \
                        \value via -X "%w%"=...") p p

