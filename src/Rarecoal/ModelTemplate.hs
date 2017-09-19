module Rarecoal.ModelTemplate (ModelOptions(..), ParamOptions(..),
    ModelTemplate(..), getModelTemplate, instantiateModel,
    makeParameterVector) where

import           Rarecoal.Core        (EventType (..), ModelEvent (..),
                                       ModelSpec (..), getTimeSteps,
                                       getNrOfPops)
import           Rarecoal.Utils       (GeneralOptions(..))

import           Control.Applicative  ((<|>))
import           Control.Error        (Script, assertErr, justErr, scriptIO,
                                       tryRight, errLn)
import           Control.Monad        (forM, when)
import           Control.Monad.Except (throwError)
import           Data.Char            (isAlphaNum)
-- import Debug.Trace (trace)
import           Data.List            (elemIndex, nub)
import           Data.Text            (unpack)
import qualified Data.Text.IO         as T
import qualified Data.Vector.Unboxed  as V
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC
import           Text.Parsec.Text (Parser)
import           Text.Parsec.Number (sign, int, floating)
import           System.Log.Logger    (infoM)

data ModelOptions = ModelOptions {
    optMaybeModelTemplateFile :: Maybe ParamOptions,
    optModelTemplateString :: Text
}

data ParamOptions = ParamOptions {
    optMaybeParamInputFile :: Maybe FilePath,
    optParameterSettings :: [(String, Double)]
}

data ModelTemplate = ModelTemplate [MTCommand]

data MTCommand = MTBranchNames [String]
               | MTDiscoveryRate BranchSpec ParamSpec
               | MTPopSizeChange ParamSpec BranchSpec ParamSpec
               | MTJoin ParamSpec BranchSpec BranchSpec
               | MTJoinPopSizeChange ParamSpec BranchSpec BranchSpec ParamSpec
               | MTSplit ParamSpec BranchSpec BranchSpec ParamSpec
               | MTFreeze ParamSpec BranchSpec Bool
               | MTConstraint ParamSpec ParamSpec ConstraintOperator

data ParamSpec = ParamFixed Double | ParamVariable String deriving (Eq, Show)
data BranchSpec = BranchByIndex Int | BranchByName String deriving (Eq, Show)

data ConstraintOperator = ConstraintOpGreater | ConstraintOpSmaller

getModelTemplate :: ModelOptions -> Either Text ModelTemplate
getModelTemplate (ModelOptions maybeFP input) = do
    ModelTemplate cmds1 <- case maybeFP of
        Just fp -> do
            input' <- scriptIO $ T.readFile fp
            case parse modelTemplateParser "" input' of
                ParserError e -> Left (show e)
                Right v -> Right v
        Nothing -> return $ ModelTemplate []
    ModelTemplate cmds2 <- case parse modelTemplateParser "" input of
        ParserError e -> Left (show e)
        Right v -> Right v
    return $ ModelTemplate (cmds1 ++ cmds2)

modelTemplateP :: Parser ModelTemplate
modelTemplateP =
    ModelTemplate <$> P.sepBy commandP (PC.char ';' *> PC.spaces)

commandP :: Parser MTCommand
commandP = try branchNameCmdP <|>
           try discoveryRateCmdP <|>
           try popSizeChangeCmdP <|>
           try joinCmdP <|>
           try joinPopSizeCmdP <|>
           try splitCmdP <|>
           try freezeCmdP <|>
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
branchSpecP = try (BranchByName <$> branchNameP) <|>
              (BranchByIndex . read <$> int)

paramSpecP :: Parser ParamSpec
paramSpecP = try (ParamFixed <$> (sign <*> floating)) <|>
             (ParamVariable <$> variableP)

variableP :: Parser String
variableP = PC.char '<' *>
    P.many1 (PC.satisfy (\c -> isAlphaNum c || c == '_')) <* '>'

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
    boolP = try (read <$> PC.string "True") <|> (read <$> PC.string "False")

constraintCmdP :: Parser MTCommand
constraintCmdP = try smallerConstraintP <|> greaterConstraintP
  where
    smallerConstraintP =
        MTConstraint <$>
        (PC.string "constraint" *> spaces1 *> paramSpecP <* PC.spaces) <*>
        (PC.char '<' *> PC.spaces *> paramSpec) <*> pure ConstraintOpSmaller
    greaterConstraintP =
        MTConstraint <$>
        (PC.string "constraint" *> spaces1 *> paramSpecP <* PC.spaces) <*>
        (PC.char '>' *> PC.spaces *> paramSpec) <*> pure ConstraintOpGreater

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

getNrAndNamesOfBranches :: ModelTemplate -> Either String (Int, [String])
getNrAndNamesOfBranches (ModelTemplate mtCommands) = do
    let branchNameCommand = [b | MTBranchNames b <- mtCommands]
    branchNames <- case branchNameCommand of
        [] -> return []
        [names] -> return names
        _ -> Left "Error: More than one branchName command."
    nrBranches <- case branchNames of
        [] -> length . nub <$> go [] mtCommands
        n -> return $ length n
    return (nrBranches, branchNames)
  where
    go res [] = return res
    go res (cmd:rest) = case cmd of
      MTDiscoveryRate (BranchByIndex i) _ -> go (i:res) rest
      MTPopSizeChange _ (BranchByIndex i) _ -> go (i:res) rest
      MTJoin _ (BranchByIndex i) (BranchByIndex j) -> go (j:i:res) rest
      MTJoinPopSizeChange _ (BranchByIndex i) (BranchByIndex j) _ ->
        go (j:i:res) rest
      MTSplit _ (BranchByIndex i) (BranchByIndex j) _ -> go (j:i:res) rest
      MTFreeze _ (BranchByIndex i) Bool -> go (i:res) rest
      MTConstraint _ _ _ -> go res rest
      MTBranchNames _ -> go res rest
      c -> Left $ "illegal branch name in " ++ show c ++
          " without branchName declaration"

instantiateModel :: GeneralOptions -> ModelTemplate -> [(String, Double)] ->
    Either String ModelSpec
instantiateModel opts mt@(ModelTemplate mtCommands) paramsDict = do
    (nrBranches, branchNames) <- getNrAndNamesOfBranches mt
    discoveryRates <- modifyDiscoveryRates branchNames mtCommands
        (V.replicate nrBranches 1.0)
    events <- reverse <$> getEvents branchNames mtCommands []
    validateConstraints mtCommands
    let timeSteps = getTimeSteps (optN0 opts) (optLinGen opts) (optTMax opts)
    return $ ModelSpec timeSteps (optTheta opts) discoveryRates
        (optRegPenalty opts) events
  where
    modifyDiscoveryRates _ _ [] res = return res
    modifyDiscoveryRates branchNames (cmd:rest) res = case cmd of
        MTDiscoveryRate branchSpec paramSpec -> do
            branchIndex <- getBranchIndex branchNames branchSpec
            param <- getParam paramsDict paramSpec
            return $ res V.// (branchIndex, param)
        _ -> modifyDiscoveryRates rest res
    getEvents _ [] res = return res
    getEvents bN (cmd:rest) res = case cmd of
        MTJoin tSpec toSpec fromSpec -> do
            t <- getParam paramsDict tSpec
            e <- Join <$> getBranchIndex bN toSpec <*>
                getBranchIndex bN fromSpec
            return (ModelEvent t e:res)
        MTPopSizeChange tSpec bSpec pSpec -> do
            t <- getParam paramsDict tSpec
            e <- SetPopSize <$> getBranchIndex bN bSpec <*>
                getParam paramsDict pSpec
            return (ModelEvent t e:res)
        MTJoinPopSizeChange tSpec toSpec fromSpec pSpec -> do
            t <- getParam paramsDict tSpec
            e1 <- Join <$> getBranchIndex bN toSpec <*>
                getBranchIndex bN fromSpec
            e2 <- SetPopSize <$> getBranchIndex bN toSpec <*>
                getParam paramsDict pSpec
            return (ModelEvent t e2:ModelEvent t e1:res)
        MTSplit tSpec toSpec fromSpec rateSpec -> do
            t <- getParam paramsDict tSpec
            e <- Split <$> getBranchIndex bN toSpec <*>
                getBranchIndex bN fromSpec <*>
                getParam paramsDict rateSpec
            return (ModelEvent t e:res)
        MTFreeze tSpec bSpec v -> do
            t <- getParam paramsDict tSpec
            e <- SetFreeze <$> getBranchIndex bN bSpec <*> pure v
            return (ModelEvent t e:res)

getBranchIndex :: [String] -> BranchSpec -> Either String Int
getBranchIndex branchNames branchSpec = case branchSpec of
    BranchByIndex k' -> return k'
    BranchByName n -> justErr ("did not find branch name " ++ n) $
        elemIndex n branchNames

getParam :: [(String, Double)] -> ParamSpec -> Either String Double
getParam _ (ParamFixed val) = return val
getParam paramsDict (ParamVariable n) = do
    case n `lookup` paramsDict of
        Nothing -> Left $ "Error in Template: could not find parameter " ++ show n
        Just val -> return val

validateConstraints :: [(String, Double)] -> [MTCommands] ->
    Either String ()
validateConstraints _ [] = return ()
validateConstraints paramsDict (cmd:rest) =
    case cmd of
        MTConstraint pSpec1 pSpec2 op -> do
            p1 <- getParam paramsDict pSpec1
            p2 <- getParam paramsDict pSpec2
            case op of
                ConstraintOpGreater ->
                    when (p1 <= p2) $ Left "constraint violated: " ++ cmd
                ConstraintOpSmaller ->
                    when (p1 >= p2) $ Left "constraint violated: " ++ cmd
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
            errLn $ "setting parameter " ++ show key ++ " = " ++ show val
            go rest ((key, val):res)
        Just _ -> go rest res

fillParameterDictWithDefaults :: ModelTemplate -> [(String, Double)] ->
    Script [(String, Double)]
fillParameterDictWithDefaults mt paramsDict = do
    paramNames <- getParamNames mt
    additionalParams <- fmap catMaybes . forM paramNames $ \p ->
        case p `lookup` paramsDict of
            Just _ -> return Nothing
            Nothing -> do
                if head p == 'p'
                then do
                    scriptIO . errLn $ "setting parameter " ++ show p ++
                        " = 1.0 (default)"
                    return $ Just (p, 1.0)
                else if take 3 p == "adm"
                    then do
                        scriptIO . errLn $ "setting parameter " ++ show p ++
                            " = 0.05 (d efault)"
                        return $ Just (p, 0.05)
                    else
                        throwError $ "Don't know how to initialize \
                        \parameter " ++ p ++ ". Please provide initial \
                        \value via -X " ++ p ++ "=..."
