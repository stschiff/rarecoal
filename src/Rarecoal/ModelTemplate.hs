{-# LANGUAGE OverloadedStrings #-}
module Rarecoal.ModelTemplate (ModelOptions(..), ParamOptions(..),
    ModelTemplate(..), getModelTemplate, instantiateModel,
    makeParameterDict, getRegularizationPenalty, getParamNames,
    fillParameterDictWithDefaults, loadHistogram, minFunc, penalty,
    makeInitialPoint, getNrAndNamesOfBranches, validateBranchNameCongruency)
    where

import           Rarecoal.Formats.RareAlleleHistogram (RareAlleleHistogram (..),
                                                       readHistogram)
import           Rarecoal.Utils                       (filterConditionOn,
                                                       filterExcludePatterns,
                                                       filterGlobalMinAf,
                                                       filterMaxAf,
                                                       getTimeSteps,
                                                       GeneralOptions (..),
                                                       HistogramOptions (..))
import           Rarecoal.Core              (getRegularizationPenalty,
    EventType(..), ModelSpec(..), ModelEvent (..))

import           Control.Applicative                  ((<|>))
import           Control.Error                        (Script, assertErr, errLn,
                                                       justErr, scriptIO,
                                                       tryRight)
import           Control.Monad                        (forM, forM_, when, (>=>))
import           Control.Monad.Trans.Except           (throwE)
import           Data.Char                            (isAlphaNum)
-- import Debug.Trace (trace)
import           Data.List                            (elemIndex, nub)
import           Data.Maybe                           (catMaybes)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import qualified Data.Vector.Unboxed                  as V
import qualified Text.Parsec                          as P
import qualified Text.Parsec.Char                     as PC
import           Text.Parsec.Number                   (floating, int, sign)
import           Text.Parsec.Text                     (Parser)
import           Turtle                               (format, w, (%), g)

data ModelOptions = ModelOptions {
    optMaybeModelTemplateFile :: Maybe FilePath,
    optModelTemplateString    :: T.Text
}

data ParamOptions = ParamOptions {
    optMaybeParamInputFile :: Maybe FilePath,
    optParameterSettings   :: [(String, Double)]
}

newtype ModelTemplate = ModelTemplate [MTCommand]

data MTCommand = MTBranchNames [String]
               | MTDiscoveryRate BranchSpec ParamSpec
               | MTPopSizeChange ParamSpec BranchSpec ParamSpec
               | MTJoin ParamSpec BranchSpec BranchSpec
               | MTJoinPopSizeChange ParamSpec BranchSpec BranchSpec ParamSpec
               | MTSplit ParamSpec BranchSpec BranchSpec ParamSpec
               | MTFreeze ParamSpec BranchSpec Bool
               | MTConstraint ParamSpec ParamSpec ConstraintOperator
               deriving (Show)

data ParamSpec = ParamFixed Double | ParamVariable String deriving (Eq, Show)
data BranchSpec = BranchByIndex Int | BranchByName String deriving (Eq, Show)

data ConstraintOperator = ConstraintOpGreater | ConstraintOpSmaller deriving (Show)

getModelTemplate :: ModelOptions -> Script ModelTemplate
getModelTemplate (ModelOptions maybeFP input) = do
    ModelTemplate cmds1 <- case maybeFP of
        Just fp -> do
            input' <- scriptIO $ T.readFile fp
            case P.parse modelTemplateP "" input' of
                Left e  -> throwE (format w e)
                Right v -> return v
        Nothing -> return $ ModelTemplate []
    ModelTemplate cmds2 <- case P.parse modelTemplateP "" input of
        Left e  -> throwE (format w e)
        Right v -> return v
    let mt = ModelTemplate (cmds1 ++ cmds2)
    reportModelTemplate mt
    return mt
  where
    reportModelTemplate mt = do
        modelBranchNames <- tryRight $ getModelBranchNames mt
        let paramNames = getParamNames mt
        scriptIO . errLn $ format ("loaded model template with branch names "%w%" and model parameters "%w) modelBranchNames paramNames

modelTemplateP :: Parser ModelTemplate
modelTemplateP =
    ModelTemplate <$> P.sepBy commandP (PC.char ';' *> PC.spaces)

commandP :: Parser MTCommand
commandP = P.try branchNameCmdP <|>
           P.try discoveryRateCmdP <|>
           P.try popSizeChangeCmdP <|>
           P.try joinCmdP <|>
           P.try joinPopSizeCmdP <|>
           P.try splitCmdP <|>
           P.try freezeCmdP <|>
           constraintCmdP

branchNameCmdP :: Parser MTCommand
branchNameCmdP =
    MTBranchNames <$> (PC.string "branchNames" *> spaces1 *>
        (branchNameP `P.sepBy1` PC.char ','))

spaces1 :: Parser ()
spaces1 = P.skipMany1 PC.space

branchNameP :: Parser String
branchNameP = P.many1 PC.upper

discoveryRateCmdP :: Parser MTCommand
discoveryRateCmdP =
    MTDiscoveryRate <$>
    (PC.string "discoveryRate" *> spaces1 *> PC.string "branch=" *>
    branchSpecP <* spaces1) <*>
    (PC.string "rate=" *> paramSpecP)

branchSpecP :: Parser BranchSpec
branchSpecP = P.try (BranchByName <$> branchNameP) <|>
              (BranchByIndex <$> int)

paramSpecP :: Parser ParamSpec
paramSpecP = P.try (ParamFixed <$> (sign <*> floating)) <|>
             (ParamVariable <$> variableP)

variableP :: Parser String
variableP = PC.char '<' *>
    P.many1 (PC.satisfy (\c -> isAlphaNum c || c == '_')) <* P.char '>'

popSizeChangeCmdP :: Parser MTCommand
popSizeChangeCmdP =
    MTPopSizeChange <$>
    (PC.string "popSizeChange" *> spaces1 *> PC.string "time=" *> paramSpecP <*
    spaces1) <*>
    (PC.string "branch=" *> branchSpecP <* spaces1) <*>
    (PC.string "size=" *> paramSpecP)

joinCmdP :: Parser MTCommand
joinCmdP =
    MTJoin <$>
    (PC.string "join" *> spaces1 *> PC.string "time=" *> paramSpecP <*
    spaces1) <*>
    (PC.string "to=" *> branchSpecP <* spaces1) <*>
    (PC.string "from=" *> branchSpecP)

joinPopSizeCmdP :: Parser MTCommand
joinPopSizeCmdP =
    MTJoinPopSizeChange <$>
    (PC.string "join" *> spaces1 *> PC.string "time=" *> paramSpecP <*
    spaces1) <*>
    (PC.string "to=" *> branchSpecP <* spaces1) <*>
    (PC.string "from=" *> branchSpecP <* spaces1) <*>
    (PC.string "size=" *> paramSpecP)

splitCmdP :: Parser MTCommand
splitCmdP =
    MTSplit <$>
    (PC.string "split" *> spaces1 *> PC.string "time=" *> paramSpecP <*
    spaces1) <*>
    (PC.string "to=" *> branchSpecP <* spaces1) <*>
    (PC.string "from=" *> branchSpecP <* spaces1) <*>
    (PC.string "rate=" *> paramSpecP)

freezeCmdP :: Parser MTCommand
freezeCmdP =
    MTFreeze <$>
    (PC.string "freeze" *> spaces1 *> PC.string "time=" *> paramSpecP <*
    spaces1) <*>
    (PC.string "branch=" *> branchSpecP <* spaces1) <*>
    (PC.string "freeze=" *> boolP)
  where
    boolP = P.try (read <$> PC.string "True") <|> (read <$> PC.string "False")

constraintCmdP :: Parser MTCommand
constraintCmdP = P.try smallerConstraintP <|> greaterConstraintP
  where
    smallerConstraintP =
        MTConstraint <$>
        (PC.string "constraint" *> spaces1 *> paramSpecP <* PC.spaces) <*>
        (PC.char '<' *> PC.spaces *> paramSpecP) <*> pure ConstraintOpSmaller
    greaterConstraintP =
        MTConstraint <$>
        (PC.string "constraint" *> spaces1 *> paramSpecP <* PC.spaces) <*>
        (PC.char '>' *> PC.spaces *> paramSpecP) <*> pure ConstraintOpGreater

getParamNames :: ModelTemplate -> [String]
getParamNames (ModelTemplate commands) = reverse . nub $ go [] commands
  where
    go res [] = res
    go res (cmd:rest) = case cmd of
        MTDiscoveryRate _ (ParamFixed _) -> go res rest
        MTDiscoveryRate _ (ParamVariable n) -> go (n:res) rest
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
        MTConstraint (ParamVariable n) (ParamFixed _) _ -> go (n:res) rest
        MTConstraint (ParamFixed _) (ParamVariable n) _ -> go (n:res) rest
        MTConstraint (ParamVariable n1) (ParamVariable n2) _ ->
            go (n2:n1:res) rest
        _ -> go res rest

getModelBranchNames :: ModelTemplate -> Either T.Text [String]
getModelBranchNames (ModelTemplate mtCommands) = do
    let branchNameCommand = [b | MTBranchNames b <- mtCommands]
    case branchNameCommand of
        []      -> makeDefaultBranchNames
        [names] -> return names
        _       -> Left "Error: More than one branchName command."
  where
    makeDefaultBranchNames = do
        nrBranches <- length . nub <$> go [] mtCommands
        return ["Branch" ++ show i | i <- [0..nrBranches-1]]
    go res [] = return res
    go res (cmd:rest) = case cmd of
      MTDiscoveryRate (BranchByIndex i) _ -> go (i:res) rest
      MTPopSizeChange _ (BranchByIndex i) _ -> go (i:res) rest
      MTJoin _ (BranchByIndex i) (BranchByIndex j) -> go (j:i:res) rest
      MTJoinPopSizeChange _ (BranchByIndex i) (BranchByIndex j) _ ->
        go (j:i:res) rest
      MTSplit _ (BranchByIndex i) (BranchByIndex j) _ -> go (j:i:res) rest
      MTFreeze _ (BranchByIndex i) _ -> go (i:res) rest
      MTConstraint{} -> go res rest
      MTBranchNames _ -> go res rest
      c -> Left $ format ("illegal use of named branch in command "%w%
        " without branchName declaration") c

instantiateModel :: GeneralOptions -> ModelTemplate -> [(String, Double)] ->
    Either T.Text ModelSpec
instantiateModel opts mt@(ModelTemplate mtCommands) paramsDict = do
    branchNames <- getModelBranchNames mt
    discoveryRates <- modifyDiscoveryRates branchNames mtCommands
        (V.replicate (length branchNames) 1.0)
    events <- reverse <$> getEvents branchNames mtCommands []
    validateConstraints paramsDict mtCommands
    let timeSteps = getTimeSteps (optN0 opts) (optLinGen opts) (optTMax opts)
    return $ ModelSpec timeSteps (optTheta opts) (V.toList discoveryRates)
        (optRegPenalty opts) (optNoShortcut opts) events
  where
    modifyDiscoveryRates _ [] res = return res
    modifyDiscoveryRates branchNames (cmd:rest) res = case cmd of
        MTDiscoveryRate branchSpec paramSpec -> do
            branchIndex <- getBranchIndex branchNames branchSpec
            param <- getParam paramsDict paramSpec
            return $ res V.// [(branchIndex, param)]
        _ -> modifyDiscoveryRates branchNames rest res
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
        _ -> getEvents bN rest res

getBranchIndex :: [String] -> BranchSpec -> Either T.Text Int
getBranchIndex branchNames branchSpec = case branchSpec of
    BranchByIndex k' -> return k'
    BranchByName n -> justErr (format ("did not find branch name "%w) n) $
        elemIndex n branchNames

getParam :: [(String, Double)] -> ParamSpec -> Either T.Text Double
getParam _ (ParamFixed val) = return val
getParam paramsDict (ParamVariable n) =
    case n `lookup` paramsDict of
        Nothing ->
            Left $ format ("Error in Template: could not find parameter "%w) n
        Just val -> return val

validateConstraints :: [(String, Double)] -> [MTCommand] ->
    Either T.Text ()
validateConstraints _ [] = return ()
validateConstraints paramsDict (cmd:rest) =
    case cmd of
        MTConstraint pSpec1 pSpec2 op -> do
            p1 <- getParam paramsDict pSpec1
            p2 <- getParam paramsDict pSpec2
            case op of
                ConstraintOpGreater ->
                    when (p1 <= p2) $
                        Left (format ("constraint violated: "%w) cmd)
                ConstraintOpSmaller ->
                    when (p1 >= p2) $
                        Left (format ("constraint violated: "%w) cmd)
            validateConstraints paramsDict rest
        _ -> validateConstraints paramsDict rest

makeParameterDict :: ParamOptions -> IO [(String, Double)]
makeParameterDict (ParamOptions maybeInputFile xSettings) =
    case maybeInputFile of
        Just paramsFile -> do
            l <- lines <$> readFile paramsFile
            if (head . head $ l) == '#'
            then -- aha, have rarecoal mcmc output file
                removeDuplicateKeys $
                    xSettings ++ [(k, read $ v !! 2) | (k : v) <- map words . drop 3 $ l]
            else -- aha, have rarecoal maxl output file
                removeDuplicateKeys $
                    xSettings ++ [(k, read v) | [k, v] <- map words l]
        Nothing -> removeDuplicateKeys xSettings
  where
    removeDuplicateKeys settings = go settings []
    go [] res = return res
    go ((key, val):rest) res = case key `lookup` res of
        Nothing -> do
            errLn $ format ("setting parameter "%w%" = "%g) key val
            go rest ((key, val):res)
        Just _ -> go rest res

fillParameterDictWithDefaults :: ModelTemplate -> [(String, Double)] ->
    Script [(String, Double)]
fillParameterDictWithDefaults mt paramsDict = do
    let paramNames = getParamNames mt
    fmap catMaybes . forM paramNames $ \p ->
        case p `lookup` paramsDict of
            Just _ -> return Nothing
            Nothing | head p == 'p' -> do
                        scriptIO . errLn $
                            format ("setting parameter "%w%" = 1.0 (default)") p
                        return $ Just (p, 1.0)
                    | take 3 p == "adm" -> do
                        scriptIO . errLn $ format ("setting parameter "%w%
                            " = 0.05 (d efault)") p
                        return $ Just (p, 0.05)
                    | otherwise ->
                        throwE $ format ("Don't know how to initialize \
                        \parameter "%w%". Please provide initial \
                        \value via -X "%w%"=...") p p

loadHistogram :: HistogramOptions -> ModelTemplate -> Script RareAlleleHistogram
loadHistogram histOpts modelTemplate = do
    let HistogramOptions path minAf maxAf conditionOn excludePatterns = histOpts
    hist <- readHistogram path
    validateBranchNameCongruency modelTemplate (raNames hist)
    tryRight $ (filterMaxAf maxAf >=> filterGlobalMinAf minAf >=>
        filterConditionOn conditionOn >=> filterExcludePatterns excludePatterns)
        hist
  where
    validateBranchNameCongruency modelTemplate histBranchNames = do
        modelBranchNames <- tryRight $ getModelBranchNames modelTemplate
        forM_ histBranchNames $ \histName ->
            when (histName `notElem` modelBranchNames) $
                throwE (format ("histogram branch "%w%
                    " not found in model branches ("%w%")") histName
                    modelBranchNames)
        forM_ modelBranchNames $ \modelName ->
            when (modelName `notElem` histBranchNames) $
                scriptIO . errLn $ format ("found unsampled ghost branch: "%w)
                modelName

penalty :: Double
penalty = 1.0e20

makeInitialPoint :: ModelTemplate -> [(String, Double)] ->
    Either T.Text (V.Vector Double)
makeInitialPoint modelTemplate modelParams = do
    let paramNames = getParamNames modelTemplate
    vals <- forM paramNames $ \n ->
        case n `lookup` modelParams of
            Just x  -> return x
            Nothing -> Left $ format ("did not find parameter "%w) n
    return $ V.fromList vals
