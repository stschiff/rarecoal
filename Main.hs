import Data.List.Split (splitOn)
import Control.Applicative (many, (<|>))
import qualified Options.Applicative as OP
import Data.Monoid ((<>))
import Logl (runLogl, LoglOpt(..))
import Maxl (MaxlOpt(..), runMaxl)
import Mcmc (McmcOpt(..), runMcmc)
import View (ViewOpt(..), runView)
import Prob (ProbOpt(..), runProb)
import Find (FindOpt(..), runFind)
import System.Log.Logger (updateGlobalLogger, setLevel, Priority(..), infoM)
import Data.Time.Clock (getCurrentTime)
import Core (ModelEvent(..), EventType(..))
import Control.Error.Script (runScript, scriptIO)
import Data.Int (Int64)
-- import Debug.Trace (trace)
import RareAlleleHistogram (SitePattern(..))
import ModelTemplate (InitialParams(..))

data Options = Options Command

data Command = CmdView ViewOpt | CmdProb ProbOpt | CmdLogl LoglOpt | CmdMaxl MaxlOpt | CmdMcmc McmcOpt | CmdFind FindOpt

main :: IO ()
main = run =<< OP.execParser (parseOptions `withInfo` "Rarecoal: Implementation of the Rarecoal algorithm")

run :: Options -> IO ()
run (Options cmdOpts) = runScript $ do
    scriptIO $ updateGlobalLogger "rarecoal" (setLevel INFO)
    currentT <- scriptIO getCurrentTime
    scriptIO $ infoM "rarecoal" $ "Starting at " ++ show currentT
    case cmdOpts of
        CmdView opts -> runView opts
        CmdProb opts -> runProb opts
        CmdLogl opts -> runLogl opts
        CmdMaxl opts -> runMaxl opts
        CmdMcmc opts -> runMcmc opts
        CmdFind opts -> runFind opts
    currentTafter <- scriptIO getCurrentTime
    scriptIO $ infoM "rarecoal" $ "Finished at " ++ show currentTafter

parseOptions :: OP.Parser Options
parseOptions = Options <$> parseCommand

parseCommand :: OP.Parser Command
parseCommand = OP.subparser $
    OP.command "view" (parseView `withInfo` "View the input file") <>
    OP.command "prob" (parseProb `withInfo` "Compute probability for a given configuration") <>
    OP.command "logl" (parseLogl `withInfo` "Compute the likelihood of the given model for the given data set") <>
    OP.command "maxl" (parseMaxl `withInfo` "Maximize the likelihood of the model given the data set") <>
    OP.command "mcmc" (parseMcmc `withInfo` "Run MCMC on the model and the data") <>
    OP.command "find" (parseFind `withInfo` "Explore where a branch joins the tree")

parseView :: OP.Parser Command
parseView = CmdView <$> parseViewOpt

parseViewOpt :: OP.Parser ViewOpt
parseViewOpt = ViewOpt <$> parseIndices <*> parseCombineIndices <*> parseMaxAf <*> parseNrCalledSites
                       <*> parseGlobal <*> parseHistPath

parseIndices :: OP.Parser [Int]
parseIndices = OP.option OP.auto $ OP.short 'I' <> OP.long "indices"
                                                <> OP.metavar "[i1,i2,...]"
                                                <> OP.value [] <> OP.showDefault
                                                <> OP.help "Read only these indices from the input file"

parseCombineIndices :: OP.Parser [Int]
parseCombineIndices = OP.option OP.auto $ OP.long "combine"
                                                <> OP.metavar "[i1,i2,...]"
                                                <> OP.value [] <> OP.showDefault
                                                <> OP.help "combine these indices into one"

parseMinAf :: OP.Parser Int
parseMinAf = OP.option OP.auto $ OP.long "minAf" <> OP.metavar "<INT>"
                                                        <> OP.help "minimal allele frequency" <> OP.value 1
                                                        <> OP.showDefault

parseMaxAf :: OP.Parser Int
parseMaxAf = OP.option OP.auto $ OP.short 'm' <> OP.long "max_af"
                                              <> OP.metavar "INT"
                                              <> OP.value 0 <> OP.showDefault
                                              <> OP.help "set the maximum allele frequency [leave 0 to read maxAf from histogram]"

parseNrCalledSites :: OP.Parser Int64
parseNrCalledSites = OP.option OP.auto $ OP.short 'N' <> OP.long "nr_called_sites"
                                                      <> OP.metavar "INT"
                                                      <> OP.value 0 <> OP.showDefault
                                                      <> OP.help "set the nr of called sites"

parseGlobal :: OP.Parser Bool
parseGlobal = OP.switch (OP.long "globalMax" <> OP.short 'g' <> OP.help "constrain global allele frequency")

parseHistPath :: OP.Parser FilePath
parseHistPath = OP.strOption $ OP.short 'i' <> OP.long "input"
                                                <> OP.metavar "<Input File>" <> OP.help "Input File, use - for stdin"

withInfo :: OP.Parser a -> String -> OP.ParserInfo a
withInfo opts desc = OP.info (OP.helper <*> opts) $ OP.progDesc desc

parseProb :: OP.Parser Command
parseProb = CmdProb <$> parseProbOpt

parseProbOpt :: OP.Parser ProbOpt
parseProbOpt = ProbOpt <$> parseTheta <*> parseTemplateFilePath <*> parseParams <*> parseModelEvents <*> parseLinGen
                       <*> OP.argument OP.auto (OP.metavar "NVec")
                       <*> OP.argument OP.auto (OP.metavar "MVec")

parseTheta :: OP.Parser Double
parseTheta = OP.option OP.auto $ OP.short 't' <> OP.long "theta"
                                              <> OP.metavar "DOUBLE"
                                              <> OP.value 0.0005 <> OP.showDefault
                                              <> OP.help "set the scaled mutation rate"

parseTemplateFilePath :: OP.Parser FilePath
parseTemplateFilePath = OP.strOption $ OP.short 'T' <> OP.long "template" <> OP.metavar "<Input Template File>" <> OP.value "/dev/null"
                                                              <> OP.help "Specify that the model should be read from a template file"

parseParams :: OP.Parser InitialParams
parseParams = (InitialParamsList <$> parseInitialParamsList) <|> (InitialParamsFile <$> parseInitialParamsFile)

parseInitialParamsList :: OP.Parser [Double]
parseInitialParamsList = OP.option OP.auto $ OP.short 'x' <> OP.long "params" <>
                         OP.metavar "[p1,p2,...]" <>
                         OP.help "initial parameters for the template"

parseInitialParamsFile :: OP.Parser FilePath
parseInitialParamsFile = OP.strOption $ OP.long "paramsFile" <>
                         OP.metavar "<FILE>" <>
                         OP.help "file with initial parameters, can be maxl- or mcmc-output"

parseModelEvents :: OP.Parser [ModelEvent]
parseModelEvents = many parseEvent

parseEvent :: OP.Parser ModelEvent
parseEvent = parseJoin <|> parseSetP <|> parseSetR <|> parseSetM

parseJoin :: OP.Parser ModelEvent
parseJoin = OP.option (OP.str >>= readJoin) $ OP.short 'j' <> OP.long "join"
                                              <> OP.metavar "t,k,l"
                                              <> OP.help "Join at time t from l into k. Can be given multiple times"
  where
    readJoin s = do
        let [t, k, l] = splitOn "," s
        return $ ModelEvent (read t) (Join (read k) (read l))

parseSetP :: OP.Parser ModelEvent
parseSetP = OP.option (OP.str >>= readSetP) $ OP.short 'p' <> OP.long "popSize"
                                              <> OP.metavar "t,k,p"
                                              <> OP.help "At time t, set population size in k to p, and set growth rate to 0"
  where
    readSetP s = do
        let [t, k, p] = splitOn "," s
        return $ ModelEvent (read t) (SetPopSize (read k) (read p))

parseSetR :: OP.Parser ModelEvent
parseSetR = OP.option (OP.str >>= readSetR) $ OP.short 'r' <> OP.long "growthRate"
                                              <> OP.metavar "t,k,r"
                                              <> OP.help "At time t, set growth rate in k to r"
  where
    readSetR s = do
        let [t, k, r] = splitOn "," s
        return $ ModelEvent (read t) (SetGrowthRate (read k) (read r))

parseSetM :: OP.Parser ModelEvent
parseSetM = OP.option (OP.str >>= readSetM) $ OP.long "mig" <> OP.metavar "t,k,l,m"
                                              <> OP.help "At time t, set migration rate m from l to k"
  where
    readSetM s = do
        let [t, k, l, m] = splitOn "," s
        return $ ModelEvent (read t) (SetMigration (read k) (read l) (read m))

parseLinGen :: OP.Parser Int
parseLinGen = OP.option OP.auto $ OP.long "lingen"
                                              <> OP.metavar "INT"
                                              <> OP.value 400 <> OP.showDefault
                                              <> OP.help "set the number of linear generations in discretization"

parseLogl :: OP.Parser Command
parseLogl = CmdLogl <$> parseLoglOpt

parseLoglOpt :: OP.Parser LoglOpt
parseLoglOpt = LoglOpt <$> parseSpectrumPath <*> parseTheta <*> parseTemplateFilePath <*> parseParams
                                 <*> parseModelEvents <*> parseLinGen <*> parseMinAf <*> parseMaxAf <*> parseConditioning
                                 <*> parseNrCalledSites <*> parseIndices <*> parseHistPath

parseSpectrumPath :: OP.Parser FilePath
parseSpectrumPath = OP.strOption $ OP.short 's' <> OP.long "spectrumFile"
                                                <> OP.metavar "<Output Spectrum File>"
                                                <> OP.value "/dev/null" <> OP.showDefault
                                                <> OP.help "Output the allele frequencies to file"


parseMaxl :: OP.Parser Command
parseMaxl = CmdMaxl <$> parseMaxlOpt

parseMaxlOpt :: OP.Parser MaxlOpt
parseMaxlOpt = MaxlOpt <$> parseTheta <*> parseTemplateFilePath <*> parseParams <*> parseMaxCycles <*> parseNrRestarts
                       <*> parseTraceFilePath  <*> parseMinAf <*> parseMaxAf <*> parseConditioning
                       <*> parseNrCalledSites <*> parseLinGen <*> parseIndices <*> parseHistPath
  where
    parseMaxCycles = OP.option OP.auto $ OP.short 'c' <> OP.long "maxCycles"
                                                      <> OP.metavar "<NR_MAX_CYCLES>"
                                                      <> OP.value 10000 <> OP.showDefault
                                                      <> OP.help "Specifies the maximum number of cycles in the minimization routine"
    parseNrRestarts = OP.option OP.auto $ OP.long "nrRestarts"
                                                      <> OP.metavar "<NR_Restarts>"
                                                      <> OP.value 5 <> OP.showDefault
                                                      <> OP.help "Specifies the number of restarts of the minimization routine"

parseTraceFilePath :: OP.Parser FilePath
parseTraceFilePath = OP.strOption $ OP.short 'f' <> OP.long "traceFile" <> OP.metavar "<FILE>"
                                                            <> OP.value "/dev/null" <> OP.showDefault
                                                            <> OP.help "The file to write the trace"

parseMcmc :: OP.Parser Command
parseMcmc = CmdMcmc <$> parseMcmcOpt

parseMcmcOpt :: OP.Parser McmcOpt
parseMcmcOpt = McmcOpt <$> parseTheta <*> parseTemplateFilePath <*> parseParams
                       <*> parseNrCycles <*> parseTraceFilePath <*> parseMinAf
                       <*> parseMaxAf <*> parseConditioning <*> parseNrCalledSites <*> parseLinGen
                       <*> parseIndices
                       <*> parseHistPath <*> parseRandomSeed <*> parseBranchAges
  where
    parseRandomSeed = OP.option OP.auto $ OP.short 'S' <> OP.long "seed" <> OP.metavar "<INT>" <> OP.value 0 <> 
                      OP.showDefault <>
                      OP.help "Random Seed, set to zero to determine the seed from the machine clock"
    parseNrCycles = OP.option OP.auto $ OP.long "cycles" <> OP.short 'c' <> OP.value 1000 <> OP.metavar "<INT>"
                                                               <> OP.help "nr of MCMC cycles" <> OP.showDefault
    parseBranchAges = OP.option OP.auto $ OP.long "branchAges" <> OP.short 'b' <> OP.metavar "<LIST>"
                                                               <> OP.help "scaled ages of samples" <> OP.value []
                                                               <> OP.showDefault

parseConditioning :: OP.Parser [Int]
parseConditioning = OP.option OP.auto $ OP.long "conditionOn" <> OP.metavar "<List>"
                                        <> OP.help "condition on those populations having positive counts"
                                        <> OP.value [] <> OP.showDefault

parseFind :: OP.Parser Command
parseFind = CmdFind <$> parseFindOpt

parseFindOpt = FindOpt <$> parseQueryIndex <*> parseEvalFile <*> parseBranchAge <*> parseDeltaTime <*> parseMaxTime
                       <*> parseTheta
                       <*> parseTemplateFilePath <*> parseParams <*> parseModelEvents <*> parseMinAf <*> parseMaxAf
                       <*> parseConditioning <*> parseNrCalledSites <*> parseLinGen <*> parseIndices
                       <*> parseIgnoreList <*> parseHistPath <*> parseNoShortcut
  where
    parseQueryIndex = OP.option OP.auto $ OP.short 'q' <> OP.long "queryIndex" <> OP.metavar "<INT>"
                                                       <> OP.help "index of query branch"
    parseEvalFile = OP.strOption $ OP.short 'f' <> OP.long "evalFile" <> OP.metavar "<FILE>" <>
                                   OP.help "file to write the trace to"
    parseBranchAge = OP.option OP.auto $ OP.short 'b' <> OP.long "branchAge" <> OP.metavar "<Double>"
                                                       <> OP.help "sampling age of query branch"
    parseDeltaTime = OP.option OP.auto $ OP.long "deltaTime" <> OP.metavar "<Double>" <> OP.showDefault
                                                      <> OP.help "length of time intervals" <> OP.value 0.0005
    parseMaxTime = OP.option OP.auto $ OP.long "maxTime" <> OP.metavar "<Double>" <> OP.showDefault
                                                           <> OP.help "maximum time" <> OP.value 0.025
    parseIgnoreList = OP.option (OP.str >>= readIgnoreList) $ OP.long "exclude" <> OP.metavar "<list of lists>"
                                                            <> OP.help "ignore patterns" <> OP.value []
                                                            <> OP.showDefault
    parseNoShortcut = OP.switch $ OP.long "noShortcut" <>
                                  OP.help "do not use shortcut if all lineages are in one population"
    readIgnoreList s = do
        let ll = read s :: [[Int]]
        return $ map Pattern ll

