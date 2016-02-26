import Find (FindOpt(..), runFind)
import Logl (runLogl, LoglOpt(..))
import Maxl (MaxlOpt(..), runMaxl)
import Mcmc (McmcOpt(..), runMcmc)
import Prob (ProbOpt(..), runProb)
import Rarecoal.Core (ModelEvent(..), EventType(..))
import Rarecoal.RareAlleleHistogram (SitePattern(..))
import View (ViewOpt(..), runView)

import Control.Applicative (many, (<|>))
import Control.Error.Script (runScript, scriptIO)
import Data.Int (Int64)
import Data.List.Split (splitOn)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime)
import qualified Options.Applicative as OP
import System.Log.Logger (updateGlobalLogger, setLevel, Priority(..), infoM)

data Options = Options Command

data Command = CmdView ViewOpt | CmdProb ProbOpt | CmdLogl LoglOpt | CmdMaxl MaxlOpt |
               CmdMcmc McmcOpt | CmdFind FindOpt

main :: IO ()
main = run =<< OP.execParser (parseOptions `withInfo` "This software implementats the Rarecoal \ 
                              \algorithm, as described in doc/rarecoal.pdf. Type -h for getting \
                              \help")

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
    OP.command "view" (parseView `withInfo` "View the input file up to some frequency") <>
    OP.command "prob" (parseProb `withInfo` "Compute probability for a given configuration") <>
    OP.command "logl" (parseLogl `withInfo`
                       "Compute the likelihood of the given model for the given data set") <>
    OP.command "maxl" (parseMaxl `withInfo`
                       "Maximize the likelihood of the model given the data set") <>
    OP.command "mcmc" (parseMcmc `withInfo` "Run MCMC on the model and the data") <>
    OP.command "find" (parseFind `withInfo` "Explore where a branch joins the tree")

parseView :: OP.Parser Command
parseView = CmdView <$> parseViewOpt

parseViewOpt :: OP.Parser ViewOpt
parseViewOpt = ViewOpt <$> parseMaxAf <*> parseNrCalledSites <*> parseHistPath

parseMinAf :: OP.Parser Int
parseMinAf = OP.option OP.auto $ OP.long "minAf" <> OP.metavar "<INT>"
                                                 <> OP.help "minimal allele count" <> OP.value 1
                                                 <> OP.showDefault

parseMaxAf :: OP.Parser Int
parseMaxAf = OP.option OP.auto $ OP.short 'm' <> OP.long "max_af"
                                              <> OP.metavar "INT"
                                              <> OP.value 0
                                              <> OP.help "maximum allele count"

parseNrCalledSites :: OP.Parser Int64
parseNrCalledSites = OP.option OP.auto $ OP.short 'N' <> OP.long "nr_called_sites"
                              <> OP.metavar "INT"
                              <> OP.value 0
                              <> OP.help "set the nr of called sites. This will affect only the \ 
                                          \number of non-variant sites, mainly added for legacy \ 
                                          \purposes for histograms without a non-variant pattern."

parseHistPath :: OP.Parser FilePath
parseHistPath = OP.strOption $ OP.short 'i' <> OP.long "input"
                    <> OP.metavar "<Input File>" <> OP.help "Input Histogram File, use - for stdin"

withInfo :: OP.Parser a -> String -> OP.ParserInfo a
withInfo opts desc = OP.info (OP.helper <*> opts) $ OP.progDesc desc

parseProb :: OP.Parser Command
parseProb = CmdProb <$> parseProbOpt

parseProbOpt :: OP.Parser ProbOpt
parseProbOpt = ProbOpt <$> parseTheta <*> parseTemplateFilePath <*> parseInitialParamsFile
                       <*> parseInitialParamsList <*> parseModelEvents <*> parseLinGen
                       <*> OP.argument OP.auto (OP.metavar "NVec" <> OP.help "number of samples, \ 
                       \comma-separated, without spaces, and surrounded by square brackets, e.g. \ 
                       \[100,100] for two populations with a 100 haploid samples each")
                       <*> OP.argument OP.auto (OP.metavar "MVec" <> OP.help "number of derived \ 
                       \alleles in each population, same format as for NVec, e.g. [1,2] for allele \
                        \count 3 shared with one sample from the first and two from the second \ 
                        \population.")

parseTheta :: OP.Parser Double
parseTheta = OP.option OP.auto $ OP.short 't' <> OP.long "theta"
                    <> OP.metavar "DOUBLE"
                    <> OP.value 0.0005 <> OP.showDefault
                    <> OP.help "set the scaled mutation rate. This is only used for scaling and \ 
                                \should rarely be changed from the default."

parseTemplateFilePath :: OP.Parser FilePath
parseTemplateFilePath = OP.strOption $ OP.short 'T' <> OP.long "template" <>
                                       OP.metavar "<Input Template File>" <>
                                       OP.value "/dev/null" <> 
                                       OP.help "the model template file"

parseInitialParamsList :: OP.Parser [Double]
parseInitialParamsList = OP.option OP.auto $ OP.short 'x' <> OP.long "params" <>
                         OP.metavar "[p1,p2,...]" <> OP.value [] <> 
                         OP.help "initial parameters for the template, given as comma-separated \ 
                         \list without spaces and surrounded by square-brackets, e.g. \ 
                         \[1.0,1.0,0.1,0,1] for four parameters. The number given here must match \ 
                         \the number of parameters in the model template file."

parseInitialParamsFile :: OP.Parser FilePath
parseInitialParamsFile = OP.strOption $ OP.long "paramsFile" <>
                         OP.metavar "<FILE>" <> OP.value "/dev/null" <> 
                         OP.help "read the initial parameters from a file, which can be the main \ 
                         \output file from either the maxl or the mcmc commands. This is useful for continuing an optimization from a previous run, or for starting an MCMC run from the maximum found \
                         \by maxl"

parseModelEvents :: OP.Parser [ModelEvent]
parseModelEvents = many parseEvent

parseEvent :: OP.Parser ModelEvent
parseEvent = parseJoin <|> parseSplit <|> parseSetP <|> parseSetR <|> parseSetM

parseJoin :: OP.Parser ModelEvent
parseJoin = OP.option (OP.str >>= readJoin) $ OP.short 'j' <> OP.long "join"
                                              <> OP.metavar "t,k,l"
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
parseSplit = OP.option (OP.str >>= readSplit) $ OP.long "split" <> OP.metavar "t,k,l,m"
                                              <> OP.help "Specify a split event at time t from l \ 
                                              \into k with probability m. Can be given multiple \
                                              \times. Ignored if a \ 
                                              \model template file is given. Example: -s \ 
                                              \0.1,0,1,0.2 \ 
                                              \specifies a split from branch 1 into branch 0 at \
                                              \time 0.1 with probability 0.2. You can give this \ 
                                              \option multiple times"
  where
    readSplit s = do
        let [t, k, l, m] = splitOn "," s
        return $ ModelEvent (read t) (Split (read k) (read l) (read m))

parseSetP :: OP.Parser ModelEvent
parseSetP = OP.option (OP.str >>= readSetP) $ OP.short 'p' <> OP.long "popSize"
                    <> OP.metavar "t,k,p"
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
                                              <> OP.metavar "t,k,r"
                                              <> OP.help "At time t, set growth rate in k to r"
  where
    readSetR s = do
        let [t, k, r] = splitOn "," s
        return $ ModelEvent (read t) (SetGrowthRate (read k) (read r))

parseSetM :: OP.Parser ModelEvent
parseSetM = OP.option (OP.str >>= readSetM) $ OP.long "mig" <> OP.metavar "t,k,l,m" <> OP.internal
                                              <> OP.help "At time t, set migration rate m from l to k"
  where
    readSetM s = do
        let [t, k, l, m] = splitOn "," s
        return $ ModelEvent (read t) (SetMigration (read k) (read l) (read m))

parseLinGen :: OP.Parser Int
parseLinGen = OP.option OP.auto $ OP.long "lingen"
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
parseLoglOpt = LoglOpt <$> parseSpectrumPath <*> parseTheta <*> parseTemplateFilePath <*> 
                           parseInitialParamsFile <*> parseInitialParamsList <*>
                           parseModelEvents <*> parseLinGen <*> parseMinAf <*> parseMaxAf <*> 
                           parseConditioning <*> parseNrCalledSites <*> parseHistPath <*> 
                           parseNrThreads

parseSpectrumPath :: OP.Parser FilePath
parseSpectrumPath = OP.strOption $ OP.short 's' <> OP.long "spectrumFile"
                            <> OP.metavar "<Output Spectrum File>"
                            <> OP.value "/dev/null"
                            <> OP.help "Write the model probabilities for each pattern in the \ 
                            \Histogram to a file."

parseNrThreads :: OP.Parser Int
parseNrThreads = OP.option OP.auto $ OP.long "nrThreads" <> OP.metavar "<INT>" <> OP.value 0 <>
                 OP.help "set number of threads to use. By default this is set to the number of \ 
                 \processors on your system."


parseMaxl :: OP.Parser Command
parseMaxl = CmdMaxl <$> parseMaxlOpt

parseMaxlOpt :: OP.Parser MaxlOpt
parseMaxlOpt = MaxlOpt <$> parseTheta <*> parseTemplateFilePath <*> parseInitialParamsFile
                       <*> parseInitialParamsList <*> parseMaxCycles <*> parseNrRestarts
                       <*> parseTraceFilePath  <*> parseMinAf <*> parseMaxAf <*> parseConditioning
                       <*> parseNrCalledSites <*> parseLinGen <*> parseHistPath <*> parseNrThreads
  where
    parseMaxCycles = OP.option OP.auto $ OP.short 'c' <> OP.long "maxCycles"
                    <> OP.metavar "<NR_MAX_CYCLES>"
                    <> OP.value 10000 <> OP.showDefault
                    <> OP.help "Specifies the maximum number of cycles in the minimization routine"
    parseNrRestarts = OP.option OP.auto $ OP.long "nrRestarts"
                      <> OP.metavar "<NR_Restarts>"
                      <> OP.value 5 <> OP.showDefault
                      <> OP.help "Specifies the number of restarts of the minimization routine. \ 
                      \Each restart will start from the end point of the previous maximization. \ 
                      \From experience, five restarts are typically enough to get close to the \
                      \true maximum."

parseTraceFilePath :: OP.Parser FilePath
parseTraceFilePath = OP.strOption $ OP.short 'f' <> OP.long "traceFile" <> OP.metavar "<FILE>"
                               <> OP.value "/dev/null"
                               <> OP.help "The file to write the trace of the maximization or the \
                               \MCMC. Can be useful to check whether parameters are converging."

parseMcmc :: OP.Parser Command
parseMcmc = CmdMcmc <$> parseMcmcOpt

parseMcmcOpt :: OP.Parser McmcOpt
parseMcmcOpt = McmcOpt <$> parseTheta <*> parseTemplateFilePath <*> parseInitialParamsFile <*> 
                           parseInitialParamsList <*> parseNrCycles <*> parseTraceFilePath <*> 
                           parseMinAf <*> parseMaxAf <*> parseConditioning <*>
                           parseNrCalledSites <*> parseLinGen <*> parseHistPath <*>
                           parseRandomSeed <*> parseBranchAges <*> parseNrThreads
  where
    parseRandomSeed = OP.option OP.auto $ OP.short 'S' <> OP.long "seed" <> OP.metavar "<INT>" <> 
                      OP.value 0 <>
                      OP.showDefault <>
                      OP.help "Random seed, use zero to determine the seed from the machine clock"
    parseNrCycles = OP.option OP.auto $ OP.long "cycles" <> OP.short 'c' <> OP.value 1000 <> 
                            OP.metavar "<INT>" <> OP.help "nr of MCMC cycles after Burnin. The \ 
                            \burnin phase is determined automatically by requiring the likelihood \ 
                            \to still systematicallyl increase. Once the likelihood stabilizes, \
                            \the number of cycles given here are performed. Note that a cycle \
                            \here means that every parameter is tried." <>
                            OP.showDefault
    parseBranchAges = OP.option OP.auto $ OP.long "branchAges" <> OP.short 'b' <>
                                OP.metavar "<LIST>" <> OP.help "scaled ages of samples, to input \ 
                                \ancient samples. This should be a comma-separated list without \ 
                                \spaces and surrounded by square-brackets. The length of the \
                                \list must equal the number of populations in the histogram \
                                \file." <> OP.value []

parseConditioning :: OP.Parser [Int]
parseConditioning = OP.option OP.auto $ OP.long "conditionOn" <> OP.metavar "<List>"
                                        <> OP.help "a comma-separated list without spaces and \ 
                                        \surrounded by square-brackets. Gives all branches (as \
                                        \0-based indices) in which you require at least one \
                                        \derived allele for the computation of the likelihood. \
                                        \This is useful for mapping ancient samples onto a tree" <> 
                                        OP.value []

parseFind :: OP.Parser Command
parseFind = CmdFind <$> parseFindOpt

parseFindOpt :: OP.Parser FindOpt
parseFindOpt = FindOpt <$> parseQueryIndex <*> parseEvalFile <*> parseBranchAge <*>
                           parseDeltaTime <*> parseMaxTime <*> parseTheta <*>
                           parseTemplateFilePath <*> parseInitialParamsFile <*> 
                           parseInitialParamsList <*> parseModelEvents <*> parseMinAf <*>
                           parseMaxAf <*> parseConditioning <*> parseNrCalledSites <*>
                           parseLinGen <*> parseIgnoreList <*> parseHistPath <*>
                           parseNoShortcut <*> parseNrThreads
  where
    parseQueryIndex = OP.option OP.auto $ OP.short 'q' <> OP.long "queryIndex" <> OP.metavar "<INT>"
                                                       <> OP.help "0-based index of query branch"
    parseEvalFile = OP.strOption $ OP.short 'f' <> OP.long "evalFile" <> OP.metavar "<FILE>" <>
                                   OP.help "file to write the list of computed likelihoods to"
    parseBranchAge = OP.option OP.auto $ OP.short 'b' <> OP.long "branchAge" <>
                        OP.metavar "<Double>" <> OP.help "sampling age of the query sample"
    parseDeltaTime = OP.option OP.auto $ OP.long "deltaTime" <> OP.metavar "<Double>" <> 
                            OP.showDefault <> OP.help "time between the points" <> OP.value 0.0005
    parseMaxTime = OP.option OP.auto $ OP.long "maxTime" <> OP.metavar "<Double>" <>
                         OP.showDefault <> OP.help "maximum time" <> OP.value 0.025
    parseIgnoreList = OP.option (OP.str >>= readIgnoreList) $ OP.long "exclude" <>
                            OP.metavar "<list of lists>" <> OP.help "a comma-separated list of \ 
                            \lists, without spaces, with patterns to exclude from the likelihood \ 
                            \computation" <> OP.value [] <> OP.internal
    parseNoShortcut = OP.switch $ OP.long "noShortcut" <> OP.internal <>
                                  OP.help "do not use shortcut if all lineages are in one \ 
                                           \population"
    readIgnoreList s = do
        let ll = read s :: [[Int]]
        return $ map Pattern ll
