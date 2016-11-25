import Find (FindOpt(..), runFind)
import Logl (runLogl, LoglOpt(..))
import Maxl (MaxlOpt(..), runMaxl)
import Mcmc (McmcOpt(..), runMcmc)
import Prob (ProbOpt(..), runProb)
import FitTable (FitTableOpt(..), runFitTable)
import SimCommand (SimCommandOpt(..), runSimCommand)
import Rarecoal.Core (ModelEvent(..), EventType(..))
import Rarecoal.ModelTemplate(ModelDesc(..), ParamsDesc)
import Rarecoal.RareAlleleHistogram (SitePattern(..))

import Control.Applicative (many, (<|>))
import Control.Error.Script (runScript, scriptIO)
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime)
import qualified Options.Applicative as OP
import System.Log.Logger (updateGlobalLogger, setLevel, Priority(..), infoM)

data Options = Options Command

data Command =
    CmdProb ProbOpt |
    CmdLogl LoglOpt |
    CmdMaxl MaxlOpt |
    CmdMcmc McmcOpt |
    CmdFind FindOpt |
    CmdFitTable FitTableOpt |
    CmdSimCommand SimCommandOpt

main :: IO ()
main = run =<< OP.execParser (parseOptions `withInfo` "Version 1.2.1. This \
    \software implements the Rarecoal algorithm, as described in \
    \doc/rarecoal.pdf. Type -h for getting help")

withInfo :: OP.Parser a -> String -> OP.ParserInfo a
withInfo opts desc = OP.info (OP.helper <*> opts) $ OP.progDesc desc

run :: Options -> IO ()
run (Options cmdOpts) = runScript $ do
    scriptIO $ updateGlobalLogger "rarecoal" (setLevel INFO)
    currentT <- scriptIO getCurrentTime
    scriptIO $ infoM "rarecoal" $ "Starting at " ++ show currentT
    case cmdOpts of
        CmdProb opts -> runProb opts
        CmdLogl opts -> runLogl opts
        CmdMaxl opts -> runMaxl opts
        CmdMcmc opts -> runMcmc opts
        CmdFind opts -> runFind opts
        CmdFitTable opts -> runFitTable opts
        CmdSimCommand opts -> runSimCommand opts
    currentTafter <- scriptIO getCurrentTime
    scriptIO $ infoM "rarecoal" $ "Finished at " ++ show currentTafter

parseOptions :: OP.Parser Options
parseOptions = Options <$> parseCommand

parseCommand :: OP.Parser Command
parseCommand = OP.subparser $
    OP.command "prob" (parseProb `withInfo` "Compute probability for a given \
        \configuration") <>
    OP.command "logl" (parseLogl `withInfo` "Compute the likelihood of the \
        \given model for the given data set") <>
    OP.command "maxl" (parseMaxl `withInfo` "Maximize the likelihood of the \
        \model given the data set") <>
    OP.command "mcmc" (parseMcmc `withInfo` "Run MCMC on the model and the \
        \data") <>
    OP.command "find" (parseFind `withInfo` "Explore where a branch joins the \
        \tree") <>
    OP.command "fitTable" (parseFitTable `withInfo` "Print a table for \
        \plotting fits") <>
    OP.command "simCommand" (parseSimCommand `withInfo`
        "print a simulation command line from a model")

parseProb :: OP.Parser Command
parseProb = CmdProb <$> parseProbOpt

parseProbOpt :: OP.Parser ProbOpt
parseProbOpt = ProbOpt <$> parseTheta <*> parseModelDesc <*> parseLinGen <*>
    parseBranchnames <*> parseNVec <*> parseKVec
  where
    parseNVec = OP.argument OP.auto (OP.metavar "[N1,N2,...]" <> OP.help "number of samples, \ 
                       \comma-separated, without spaces, and surrounded by square brackets, e.g. \ 
                       \[100,100] for two populations with a 100 haploid samples each")
    parseKVec = OP.argument OP.auto (OP.metavar "[k1,k2,...]" <> OP.help "number of derived \ 
                       \alleles in each population, same format as for NVec, e.g. [1,2] for allele \
                        \count 3 shared with one sample from the first and two from the second \ 
                        \population.")
    parseBranchnames = OP.option (splitOn "," <$> OP.str) (OP.help "string of branch names" <> OP.long "branchnames")

parseTheta :: OP.Parser Double
parseTheta = OP.option OP.auto $ OP.short 't' <> OP.long "theta" <> OP.hidden <> OP.metavar "FLOAT"
                    <> OP.value 0.0005 <> OP.showDefault
                    <> OP.help "set the scaled mutation rate. This is only used for scaling and \ 
                                \should rarely be changed from the default."

parseModelDesc :: OP.Parser ModelDesc
parseModelDesc = parseModelDescTemplate <|> parseModelDescDirect
  where
    parseModelDescDirect = ModelDescDirect <$> many parseDiscoveryRates <*> parseModelEvents

parseDiscoveryRates :: OP.Parser (String, Double)
parseDiscoveryRates = OP.option (readKeyValuePair <$> OP.str) $
    OP.long "discoveryRate" <> OP.metavar "POP=VAL" <>
    OP.help "set the discovery rate for a specific branch. By default, \
    \all discovery rates are 1, you can use this option to lower them for \
    \a specific sample/population."
  where
    readKeyValuePair s =
        let [key, valS] = splitOn "=" s
        in  (key, read valS)

parseModelDescTemplate :: OP.Parser ModelDesc
parseModelDescTemplate = ModelDescTemplate <$> parseTemplateFilePath <*>
    parseParamsDesc <*> many parseDiscoveryRates <*> parseModelEvents

parseTemplateFilePath :: OP.Parser FilePath
parseTemplateFilePath = OP.strOption $ OP.short 'T' <> OP.long "template" <>
                                       OP.metavar "FILE" <>
                                       OP.help "the model template file"

parseParamsDesc :: OP.Parser ParamsDesc
parseParamsDesc = (Left <$> parseInitialParamsFromFile) <|> (Right <$> parseInitialParamsList)

parseInitialParamsFromFile :: OP.Parser (FilePath, [(String, Double)])
parseInitialParamsFromFile = (,) <$> parseInitialParamsFile <*> parseAdditionalParams

parseInitialParamsFile :: OP.Parser FilePath
parseInitialParamsFile = OP.strOption $ OP.long "paramsFile" <> OP.metavar "FILE" <> 
                         OP.help "read the initial parameters from a file, which can be the main \ 
                         \output file from either the maxl or the mcmc commands. This is useful for continuing an optimization from a previous run, or for starting an MCMC run from the maximum found \
                         \by maxl"

parseAdditionalParams :: OP.Parser [(String, Double)]
parseAdditionalParams = many parseAdditionalParam
  where
    parseAdditionalParam = OP.option (parseOptionString <$> OP.str) $ OP.long "setParam" <> 
                           OP.short 'X' <>
                           OP.hidden <> OP.help "set parameter in addition to parameters read \
                           \from the given parameter file. If the parameter is also found in \
                           \the file, it is overwritten. Should be given in format PARAM=VALUE. \
                           \Can be given multiple times."
    parseOptionString s =
        let fields = splitOn "=" s
        in if length fields /= 2
           then error ("could not parse parameter option " ++ s)
           else
               let [param, valueS] = fields
               in  (param, read valueS)

parseInitialParamsList :: OP.Parser [Double]
parseInitialParamsList = OP.option OP.auto $ OP.short 'x' <> OP.long "params" <>
                         OP.metavar "[x1,x2,...]" <> 
                         OP.help "initial parameters for the template, given as comma-separated \ 
                         \list without spaces and surrounded by square-brackets, e.g. \ 
                         \[1.0,1.0,0.1,0,1] for four parameters. The number given here must match \ 
                         \the number of parameters in the model template file."

parseModelEvents :: OP.Parser [ModelEvent]
parseModelEvents = many parseEvent

parseEvent :: OP.Parser ModelEvent
parseEvent = parseJoin <|> parseSplit <|> parseSetP <|> parseSetR <|> parseSetM <|> parseSetFreeze

parseJoin :: OP.Parser ModelEvent
parseJoin = OP.option (OP.str >>= readJoin) $ OP.short 'j' <> OP.long "join" <> OP.hidden
                                              <> OP.metavar "FLOAT,INT,INT"
                                              <> OP.help "Specify a join event at time t from l \ 
                                              \into k. Can be given multiple times. Ignored if a \ 
                                              \model template file is given. Example: -j 0.1,0,1 \ 
                                              \specifies a join from branch 1 into branch 0 at \
                                              \time 0.1.  You can give this option multiple times"
  where
    readJoin s = do
        let [t, k, l] = splitOn "," s
        return $ ModelEvent (read t) (Join (read k) (read l))

parseSplit :: OP.Parser ModelEvent
parseSplit = OP.option (OP.str >>= readSplit) $ OP.long "split" <> OP.metavar "FLOAT,INT,INT,FLOAT"
                                              <> OP.help "Specify a split event at time t from l \ 
                                              \into k with probability m. Can be given multiple \
                                              \times. Ignored if a \ 
                                              \model template file is given. Example: -s \ 
                                              \0.1,0,1,0.2 \ 
                                              \specifies a split from branch 1 into branch 0 at \
                                              \time 0.1 with probability 0.2. You can give this \ 
                                              \option multiple times" <> OP.hidden
  where
    readSplit s = do
        let [t, k, l, m] = splitOn "," s
        return $ ModelEvent (read t) (Split (read k) (read l) (read m))

parseSetP :: OP.Parser ModelEvent
parseSetP = OP.option (OP.str >>= readSetP) $ OP.short 'p' <> OP.long "popSize"
                    <> OP.metavar "FLOAT,INT,FLOAT" <> OP.hidden
                    <> OP.help "Specify a population size change event at time t in branch k to \ 
                    \population size p. Can be given multiple times. Ignored if a model template \ 
                    \file is given. Example: -p 0.1,0,2.2 specifies that at time 0.1 the \
                    \population size in branch 0 should be set to 2.2. You can give this option \ 
                    \multiple times"
  where
    readSetP s = do
        let [t, k, p] = splitOn "," s
        return $ ModelEvent (read t) (SetPopSize (read k) (read p))

parseSetR :: OP.Parser ModelEvent
parseSetR = OP.option (OP.str >>= readSetR) $ OP.short 'r' <> OP.internal <> OP.long "growthRate"
                                              <> OP.metavar "FLAOT,INT,FLOAT"
                                              <> OP.help "At time t, set growth rate in k to r"
  where
    readSetR s = do
        let [t, k, r] = splitOn "," s
        return $ ModelEvent (read t) (SetGrowthRate (read k) (read r))

parseSetM :: OP.Parser ModelEvent
parseSetM = OP.option (OP.str >>= readSetM) $ OP.long "mig" <> OP.metavar "FLAOT,INT,INT,FLOAT"
                                              <> OP.internal
                                              <> OP.help "At time t, set migration rate m from l to k"
  where
    readSetM s = do
        let [t, k, l, m] = splitOn "," s
        return $ ModelEvent (read t) (SetMigration (read k) (read l) (read m))

parseSetFreeze :: OP.Parser ModelEvent
parseSetFreeze = OP.option (OP.str >>= readSetFreeze) $ OP.long "freeze" <> OP.metavar "FLOAT,INT,BOOL"
                                              <> OP.help "At time t, set or unset freeze at branch k"
                                              <> OP.hidden
  where
    readSetFreeze s = do
        let [t, k, b] = splitOn "," s
        return $ ModelEvent (read t) (SetFreeze (read k) (read b))

parseLinGen :: OP.Parser Int
parseLinGen = OP.option OP.auto $ OP.long "lingen" <> OP.hidden
                            <> OP.metavar "INT"
                            <> OP.value 400 <> OP.showDefault
                            <> OP.help "set the number of approximately linearly spaced time \ 
                            \intervals in the  discretization. This is described in \ 
                            \doc/rarecoal.pdf. You can use this parameter to speed up the \ 
                            \computation at the cost of accuracy. For example, set this to 50 and \ 
                            \everything will run much faster but be less correct. It is useful for \
                            \example to quickly search for a rough estimate of a maximum using \ 
                            \maxl, and then rerunning with the finer standard discretization (400)."

parseLogl :: OP.Parser Command
parseLogl = CmdLogl <$> parseLoglOpt

parseLoglOpt :: OP.Parser LoglOpt
parseLoglOpt = LoglOpt <$> parseTheta <*> parseModelDesc <*> 
                           parseLinGen <*> parseMinAf <*> parseMaxAf <*> 
                           parseConditioning <*> parseHistPath <*> 
                           parseNrThreads

parseMinAf :: OP.Parser Int
parseMinAf = OP.option OP.auto $ OP.long "minAf" <> OP.metavar "INT" <> OP.hidden
                                                 <> OP.help "minimal allele count" <> OP.value 1
                                                 <> OP.showDefault

parseMaxAf :: OP.Parser Int
parseMaxAf = OP.option OP.auto $ OP.short 'm' <> OP.long "max_af"
                                              <> OP.metavar "INT"
                                              <> OP.value 0
                                              <> OP.help "maximum allele count"

parseHistPath :: OP.Parser FilePath
parseHistPath = OP.strOption $ OP.short 'i' <> OP.long "input"
                    <> OP.metavar "FILE" <> OP.help "Input Histogram File, use - for stdin"

parseNrThreads :: OP.Parser Int
parseNrThreads = OP.option OP.auto $ OP.long "nrThreads" <> OP.metavar "INT" <> OP.value 0 <>
                 OP.help "set number of threads to use. By default this is set to the number of \ 
                 \processors on your system." <> OP.hidden


parseMaxl :: OP.Parser Command
parseMaxl = CmdMaxl <$> parseMaxlOpt

parseMaxlOpt :: OP.Parser MaxlOpt
parseMaxlOpt = MaxlOpt <$> parseTheta <*> parseTemplateFilePath <*> parseModelEvents <*>
                          parseParamsDesc
                       <*> parseMaxCycles <*> parseNrRestarts
                       <*> parseTraceFilePath  <*> parseMinAf <*> parseMaxAf <*> parseConditioning
                       <*> parseLinGen <*> parseHistPath <*> parseNrThreads
  where
    parseMaxCycles = OP.option OP.auto $ OP.short 'c' <> OP.long "maxCycles"
                    <> OP.metavar "INT" <> OP.hidden
                    <> OP.value 10000 <> OP.showDefault
                    <> OP.help "Specifies the maximum number of cycles in the minimization routine"
    parseNrRestarts = OP.option OP.auto $ OP.long "nrRestarts"
                      <> OP.metavar "INT" <> OP.hidden
                      <> OP.value 5 <> OP.showDefault
                      <> OP.help "Specifies the number of restarts of the minimization routine. \ 
                      \Each restart will start from the end point of the previous maximization. \ 
                      \From experience, five restarts are typically enough to get close to the \
                      \true maximum."

parseTraceFilePath :: OP.Parser FilePath
parseTraceFilePath = OP.strOption $ OP.short 'f' <> OP.long "traceFile" <> OP.metavar "FILE"
                               <> OP.value "/dev/null"
                               <> OP.help "The file to write the trace of the maximization or the \
                               \MCMC. Can be useful to check whether parameters are converging."

parseMcmc :: OP.Parser Command
parseMcmc = CmdMcmc <$> parseMcmcOpt

parseMcmcOpt :: OP.Parser McmcOpt
parseMcmcOpt = McmcOpt <$> parseTheta <*> parseTemplateFilePath <*> parseModelEvents <*>
                           parseParamsDesc <*> 
                           parseNrCycles <*> parseTraceFilePath <*> 
                           parseMinAf <*> parseMaxAf <*> parseConditioning <*>
                           parseLinGen <*> parseHistPath <*>
                           parseRandomSeed <*> parseNrThreads
  where
    parseRandomSeed = OP.option OP.auto $ OP.short 'S' <> OP.long "seed" <> OP.metavar "INT" <> 
                      OP.value 0 <> OP.hidden <>
                      OP.showDefault <>
                      OP.help "Random seed, use zero to determine the seed from the machine clock"
    parseNrCycles = OP.option OP.auto $ OP.long "cycles" <> OP.short 'c' <> OP.value 1000 <> 
                            OP.metavar "INT" <> OP.help "nr of MCMC cycles after Burnin. The \ 
                            \burnin phase is determined automatically by requiring the likelihood \ 
                            \to still systematicallyl increase. Once the likelihood stabilizes, \
                            \the number of cycles given here are performed. Note that a cycle \
                            \here means that every parameter is tried." <>
                            OP.showDefault <> OP.hidden
    --parseBranchAges = OP.option OP.auto $ OP.long "branchAges" <> OP.short 'b' <>
                                --OP.metavar "[FLOAT,FLOAT,...]" <> OP.help "scaled ages of samples, to input \ 
                                -- \ancient samples. This should be a comma-separated list without \ 
                                -- \spaces and surrounded by square-brackets. The length of the \
                                -- \list must equal the number of populations in the histogram \
                                -- \file." <> OP.value [] <> OP.hidden

parseConditioning :: OP.Parser [Int]
parseConditioning = OP.option OP.auto $ OP.long "conditionOn" <> OP.metavar "[INT,INT,...]"
                                        <> OP.help "a comma-separated list without spaces and \ 
                                        \surrounded by square-brackets. Gives all branches (as \
                                        \0-based indices) in which you require at least one \
                                        \derived allele for the computation of the likelihood. \
                                        \This is useful for mapping ancient samples onto a tree" <> 
                                        OP.value [] <> OP.hidden

parseFind :: OP.Parser Command
parseFind = CmdFind <$> parseFindOpt

parseFindOpt :: OP.Parser FindOpt
parseFindOpt = FindOpt <$> parseQueryBranch <*> parseEvalFile <*> parseBranchAge <*>
                           parseDeltaTime <*> parseMaxTime <*> parseTheta <*>
                           parseModelDesc <*> parseMinAf <*>
                           parseMaxAf <*> parseConditioning <*> 
                           parseLinGen <*> parseIgnoreList <*> parseHistPath <*>
                           parseNoShortcut <*> parseNrThreads
  where
    parseQueryBranch = (Left <$> parseQueryIndex) <|> (Right <$> parseQueryName)
    -- parseBranchPopSize = OP.option OP.auto $ OP.long "branchPopSize" <> OP.metavar "FLOAT"
      --  <> OP.help "branch population size" <> OP.value 1 <> OP.showDefault
    parseQueryIndex = OP.option OP.auto $ OP.short 'q' <> OP.long "queryIndex" <> OP.metavar "INT"
                                                       <> OP.help "0-based index of query branch"
    parseQueryName = OP.strOption $ OP.short 'n' <> OP.long "queryName" <> OP.metavar "STRING"
                                                       <> OP.help "branch name to query"
    parseEvalFile = OP.strOption $ OP.short 'f' <> OP.long "evalFile" <> OP.metavar "FILE" <>
                                   OP.help "file to write the list of computed likelihoods to"
    parseBranchAge = OP.option OP.auto $ OP.short 'b' <> OP.long "branchAge" <>
                        OP.metavar "FLOAT" <> OP.help "sampling age of the query sample"
    parseDeltaTime = OP.option OP.auto $ OP.long "deltaTime" <> OP.metavar "<Double>" <> 
                            OP.showDefault <> OP.help "time between the points" <>
                            OP.value 0.0005 <> OP.hidden
    parseMaxTime = OP.option OP.auto $ OP.long "maxTime" <> OP.metavar "<Double>" <>
                         OP.showDefault <> OP.help "maximum time" <> OP.value 0.025 <> OP.hidden
    parseIgnoreList = OP.option (OP.str >>= readIgnoreList) $ OP.long "exclude" <>
                            OP.metavar "[[INT,INT,...],[INT,INT,...],...]" <>
                            OP.help "a comma-separated list of \ 
                            \lists, without spaces, with patterns to exclude from the likelihood \ 
                            \computation" <> OP.value [] <> OP.internal
    parseNoShortcut = OP.switch $ OP.long "noShortcut" <> OP.internal <>
                                  OP.help "do not use shortcut if all lineages are in one \ 
                                           \population"
    readIgnoreList s = do
        let ll = read s :: [[Int]]
        return $ map Pattern ll

parseFitTable :: OP.Parser Command
parseFitTable = CmdFitTable <$> parseFitTableOpt

parseFitTableOpt :: OP.Parser FitTableOpt
parseFitTableOpt = FitTableOpt <$> parseModelDesc <*> parseTheta <*> parseLinGen <*> parseMaxAf <*>
                                parseHistPath

parseSimCommand :: OP.Parser Command
parseSimCommand = CmdSimCommand <$> parseSimCommandOpts

parseSimCommandOpts :: OP.Parser SimCommandOpt
parseSimCommandOpts = SimCommandOpt <$> parseModelDesc <*> parseBranchNames <*> parseNrHaps <*> 
                                        parseTheta <*> parseRho <*> parseChromLength
  where
    parseBranchNames = OP.option (splitOn "," <$> OP.str) $ OP.long "branchNames" <>
                       OP.short 'b' <> OP.metavar "NAME1,NAME2,..." <>
                       OP.help ("specify the branch names as a comma-separated string. This is \
                                 \needed because the model template may contain branch names \
                                 \instead of numbers.")
    parseNrHaps = OP.option (map read . splitOn "," <$> OP.str) $ OP.long "nrHaps" <>
                    OP.short 'n' <>
                    OP.metavar "N1,N2,..." <>
                            OP.help ("specify the number of chromosomes needed per population, as \
                            \a comma-separated list of integers")
                            
    parseRho = OP.option OP.auto $ OP.short 'r' <> OP.long "rho" <> OP.hidden <>
                 OP.metavar "FLOAT" <> OP.value 0.0004 <> OP.showDefault <> OP.help "set the \
                 \scaled recombination rate."
    parseChromLength = OP.option OP.auto $ OP.short 'L' <> OP.long "chromLength" <>
                       OP.metavar "INT" <> OP.value 100000000 <>
                       OP.help "specify the length of the simulated chromosomes in basepairs"
