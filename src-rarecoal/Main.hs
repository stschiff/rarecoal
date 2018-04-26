{-# LANGUAGE OverloadedStrings #-}

import           Find                         (FindOpt (..), runFind)
import           FitTable                     (FitTableOpt (..), runFitTable)
import           Logl                         (LoglOpt (..), runLogl)
import           Maxl                         (MaxlOpt (..), runMaxl)
import           Mcmc                         (McmcOpt (..), runMcmc)
import           Prob                         (ProbOpt (..), runProb)
import           Rarecoal.ModelTemplate       (ModelOptions (..), ParamOptions(..))
import Rarecoal.Utils (GeneralOptions(..), HistogramOptions(..))
import           SimCommand                   (SimCommandOpt (..),
                                               runSimCommand)
import qualified Rarecoal.Core as Core
import qualified Rarecoal.Core2 as Core2

import           Control.Applicative          ((<|>))
import           Control.Error         (runScript, scriptIO, errLn)
import           Data.List.Split              (splitOn)
import           Data.Monoid                  ((<>))
import           Data.Time.Clock              (getCurrentTime)
import qualified Data.Text as T
import qualified Options.Applicative          as OP
import           Turtle                       (format, w, (%))

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
main = run =<< OP.execParser (parseOptions `withInfo` "Version 1.4.0. This \
    \software implements the Rarecoal algorithm, as described in \
    \doc/rarecoal.pdf. Type -h for getting help")

withInfo :: OP.Parser a -> String -> OP.ParserInfo a
withInfo opts desc = OP.info (OP.helper <*> opts) $ OP.progDesc desc

run :: Options -> IO ()
run (Options cmdOpts) = runScript $ do
    currentT <- scriptIO getCurrentTime
    scriptIO . errLn $ format ("Rarecoal starting at "%w) currentT
    case cmdOpts of
        CmdProb opts       -> runProb opts
        CmdLogl opts       -> runLogl opts
        CmdMaxl opts       -> runMaxl opts
        CmdMcmc opts       -> runMcmc opts
        CmdFind opts       -> runFind opts
        CmdFitTable opts   -> runFitTable opts
        CmdSimCommand opts -> runSimCommand opts
    currentTafter <- scriptIO getCurrentTime
    scriptIO . errLn $ format ("Rarecoal finished at "%w) currentTafter

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
parseProbOpt = ProbOpt <$> parseGeneralOpts <*> parseModelOpts <*>
    parseParamOpts <*> parseNVec <*> parseKVec
  where
    parseNVec = OP.argument OP.auto (OP.metavar "[N1,N2,...]" <>
        OP.help "number of samples, comma-separated, without spaces, and \
        \surrounded by square brackets, e.g. [100,100] for two populations \
        \with a 100 haploid samples each")
    parseKVec = OP.argument OP.auto (OP.metavar "[k1,k2,...]" <>
        OP.help "number of derived alleles in each population, same format as \
        \for NVec, e.g. [1,2] for allele count 3 shared with one sample from \
        \the first and two from the second population.")

parseGeneralOpts :: OP.Parser GeneralOptions
parseGeneralOpts = GeneralOptions <$> parseCoreFunc <*> parseTheta <*> parseNrThreads <*>
    parseNoShortcut <*> parseRegularization <*> parseN0 <*> parseLinGen <*> parseTmax
  where
    parseCoreFunc = (\c -> if c then Core2.getProb else Core.getProb) <$>
        OP.switch (OP.long "core2" <> OP.hidden <> OP.help "Use the new Core2 algorithm (in beta)")
    parseTheta = OP.option OP.auto $ OP.long "theta" <>
        OP.hidden <> OP.metavar "FLOAT" <> OP.value 0.0005 <>
        OP.showDefault <> OP.help "set the scaled mutation rate. This is only \
        \used for scaling and should rarely be changed from the default."
    parseNrThreads = OP.option OP.auto $ OP.long "nrThreads" <>
        OP.metavar "INT" <> OP.value 0 <> OP.help "set number of threads to \
        \use. By default this is set to the number of processors on your \
        \system." <> OP.hidden
    parseNoShortcut = OP.switch $ OP.long "noShortcut" <> OP.internal <>
        OP.help "do not use shortcut if all lineages are in one population"
    parseRegularization = OP.option OP.auto $ OP.long "regularization" <>
        OP.metavar "FLOAT" <> OP.help "set the regularization parameter for \
        \population size changes. This is a penalty factor for fold-change in \
        \population size. Set to 0 to switch off." <> OP.value 10.0 <>
        OP.showDefault <> OP.hidden
    parseN0 = OP.option OP.auto $ OP.long "n0" <> OP.internal <>
        OP.metavar "INT" <> OP.value 20000 <> OP.showDefault <>
        OP.help "this is an internal parameter important for the time \
        \patterning function."
    parseLinGen = OP.option OP.auto $ OP.long "lingen" <> OP.hidden <>
        OP.metavar "INT" <> OP.value 400 <> OP.showDefault <>
        OP.help "set the number of approximately linearly spaced time \
        \intervals in the discretization. This is described in \
        \doc/rarecoal.pdf. You can use this parameter to speed up the \
        \computation at the cost of accuracy. For example, set this to 50 and \
        \everything will run much faster but be less correct. It is useful for \
        \example to quickly search for a rough estimate of a maximum using \
        \maxl, and then rerunning with the finer standard discretization."
    parseTmax = OP.option OP.auto $ OP.long "tMax" <>
        OP.internal <> OP.metavar "FLOAT" <> OP.value 20.0 <> OP.showDefault <>
        OP.help "this is an internal parameter important for the time \
        \patterning function."

parseModelOpts :: OP.Parser ModelOptions
parseModelOpts = parseModelByFile <|> parseModelByText

parseModelByFile :: OP.Parser ModelOptions
parseModelByFile = ModelByFile <$> OP.strOption (OP.metavar "TEMPLATE_FILE" <>
    OP.help "file with model template commands" <> OP.short 'T' <> OP.long "modelTemplate")

parseModelByText :: OP.Parser ModelOptions
parseModelByText = ModelByText <$> OP.strOption (OP.metavar "MODEL_SPECS" <>
    OP.short 't' <> OP.help "model template passed directly via the command line.")

parseParamOpts :: OP.Parser ParamOptions
parseParamOpts = ParamOptions <$> parseMaybeParamInputFile <*> OP.many parseParamSetting
  where
    parseMaybeParamInputFile = OP.option (Just <$> OP.str) $
        OP.long "paramsFile" <> OP.short 'P' <> OP.metavar "FILE" <> OP.value Nothing <>
        OP.help "read the initial parameters from a file, which can be the \
        \main output file from either the maxl or the mcmc commands. This is \
        \useful for continuing an optimization from a previous run, or for \
        \starting an MCMC run from the maximum found by maxl"
    parseParamSetting = OP.option (parseOptionString <$> OP.str) $
        OP.long "setParam" <> OP.short 'X' <> OP.metavar "PARAMETER=VALUE" <> OP.help "set \
        \parameter. If parameters are also given via an input file, this \
        \setting takes precedence. Should be given in format PARAM=VALUE. \
        \Can be given multiple times."
    parseOptionString s =
        let fields = splitOn "=" s
        in if length fields /= 2
           then error ("could not parse parameter option " ++ s)
           else
               let [param, valueS] = fields
               in  (T.pack param, read valueS)

parseLogl :: OP.Parser Command
parseLogl = CmdLogl <$> parseLoglOpt

parseLoglOpt :: OP.Parser LoglOpt
parseLoglOpt = LoglOpt <$> parseGeneralOpts <*> parseModelOpts <*>
    parseParamOpts <*> parseHistOpts

parseHistOpts :: OP.Parser HistogramOptions
parseHistOpts = HistogramOptions <$> parseHistPath <*> parseMinAf <*>
    parseMaxAf <*> parseConditioning <*> parseExcludePattern <*> parseEffSiteReduction
  where
    parseHistPath = OP.strOption $ OP.short 'i' <> OP.long "histogram" <>
        OP.metavar "FILE" <> OP.help "Input Histogram File, use - for stdin"
    parseMinAf = OP.option OP.auto $ OP.long "minAf" <> OP.metavar "INT" <>
        OP.hidden <> OP.help "minimal allele count" <> OP.value 1 <>
        OP.showDefault
    parseMaxAf = OP.option OP.auto $ OP.short 'm' <> OP.long "max_af" <>
        OP.metavar "INT" <> OP.value 4 <> OP.showDefault <>
        OP.help "maximum allele count"
    parseConditioning = OP.option OP.auto $ OP.long "conditionOn" <>
        OP.hidden <> OP.metavar "[INT,INT,...]" <>
        OP.help "a comma-separated list without spaces and surrounded by \
        \square-brackets. Gives all branches (as 0-based indices) in which you \
        \require at least one derived allele for the computation of the \
        \likelihood. This is useful for mapping ancient samples onto a tree" <>
        OP.value [] <> OP.showDefault
    parseExcludePattern = OP.option OP.auto $ OP.long "excludePattern" <>
        OP.metavar "[INT,INT,...]" <> OP.help "a comma-separated list without \
        \spaces and surrounded by square-brackets. Gives a pattern to exclude \
        \from fitting. Can be given multiple times" <> OP.hidden <> OP.value [] <> OP.showDefault
    parseEffSiteReduction = OP.option OP.auto $ OP.long "effectiveSitesReduction" <>
        OP.metavar "DOUBLE" <> OP.value 1.0 <> OP.showDefault <> OP.help "a factor between 0 and 1 \
        \that reduces the number of sites in the histogram to reflect genetic linkage."

    

parseMaxl :: OP.Parser Command
parseMaxl = CmdMaxl <$> parseMaxlOpt

parseMaxlOpt :: OP.Parser MaxlOpt
parseMaxlOpt = MaxlOpt <$> parseGeneralOpts <*> parseModelOpts <*>
    parseParamOpts <*> parseHistOpts <*> parseMaxCycles <*> parseNrRestarts <*>
    parseOutPrefix <*> parsePowell <*> parsePowellTolerance
  where
    parseMaxCycles = OP.option OP.auto $ OP.long "maxCycles"
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
    parsePowell = OP.switch $ OP.long "powell" <> OP.help "Use Powell's method instead of the \
        \Nelder-Mead Simplex method for optimization (Default)"
    parsePowellTolerance = OP.option OP.auto $ OP.long "tolerance" <>
        OP.metavar "FLOAT" <> OP.help "absolute final tolerance for Powell's method" <>
        OP.value 10 <> OP.showDefault

parseOutPrefix :: OP.Parser FilePath
parseOutPrefix = OP.strOption $ OP.short 'o' <> OP.long "prefix" <>
    OP.metavar "PREFIX" <> OP.help "give the prefix for the output files."

-- parseFixedParams :: OP.Parser [String]
-- parseFixedParams = OP.option (splitOn "," <$> OP.str) $ OP.long "fixedParams" <>
--     OP.metavar "P1,P2,P3,..." <> OP.value [] <> OP.help "Give a list of \
--     \parameters, comma-separated without spaces. Those parameters will not be \
--     \estimated, but kept fixed to the initial values."

parseMcmc :: OP.Parser Command
parseMcmc = CmdMcmc <$> parseMcmcOpt

parseMcmcOpt :: OP.Parser McmcOpt
parseMcmcOpt = McmcOpt <$> parseGeneralOpts <*> parseModelOpts <*>
    parseParamOpts <*> parseHistOpts <*> parseNrCycles <*> parseOutPrefix <*>
    parseRandomSeed
  where
    parseRandomSeed = OP.option OP.auto $ OP.long "seed" <> OP.metavar "INT" <>
                      OP.value 0 <> OP.hidden <>
                      OP.showDefault <>
                      OP.help "Random seed, use zero to determine the seed from the machine clock"
    parseNrCycles = OP.option OP.auto $ OP.long "cycles" <> OP.value 1000 <>
        OP.hidden <> OP.metavar "INT" <>
        OP.help "nr of MCMC cycles after Burnin. The burnin phase is \
        \determined automatically by requiring the likelihood to still \
        \systematicallyl increase. Once the likelihood stabilizes, the number \
        \of cycles given here are performed. Note that a cycle here means that \
        \every parameter is tried." <> OP.showDefault

parseFind :: OP.Parser Command
parseFind = CmdFind <$> parseFindOpt

parseFindOpt :: OP.Parser FindOpt
parseFindOpt = FindOpt <$> parseGeneralOpts <*> parseModelOpts <*>
    parseParamOpts <*> parseHistOpts <*> parseQueryBranch <*> parseEvalFile <*>
    parseBranchAge <*> parseDeltaTime <*> parseMaxTime
  where
    parseQueryBranch = OP.strOption $ OP.short 'n' <> OP.long "queryName" <>
        OP.metavar "STRING" <> OP.help "branch name to query"
    parseEvalFile = OP.strOption $ OP.short 'f' <> OP.long "evalFile" <>
        OP.metavar "FILE" <> OP.help "file to write the list of computed \
        \likelihoods to"
    parseBranchAge = OP.option OP.auto $ OP.short 'b' <> OP.long "branchAge" <>
        OP.metavar "FLOAT" <> OP.help "sampling age of the query sample"
    parseDeltaTime = OP.option OP.auto $ OP.long "deltaTime" <>
        OP.metavar "FLOAT" <> OP.showDefault <>
        OP.help "time between the points" <> OP.value 0.0005 <> OP.hidden
    parseMaxTime = OP.option OP.auto $ OP.long "maxTime" <>
        OP.metavar "FLOAT" <> OP.showDefault <> OP.help "maximum time" <>
        OP.value 0.025 <> OP.hidden

parseFitTable :: OP.Parser Command
parseFitTable = CmdFitTable <$> parseFitTableOpt

parseFitTableOpt :: OP.Parser FitTableOpt
parseFitTableOpt = FitTableOpt <$> parseGeneralOpts <*> parseModelOpts <*>
    parseParamOpts <*> parseHistOpts <*> parseOutPrefix

parseSimCommand :: OP.Parser Command
parseSimCommand = CmdSimCommand <$> parseSimCommandOpts

parseSimCommandOpts :: OP.Parser SimCommandOpt
parseSimCommandOpts = SimCommandOpt <$> parseGeneralOpts <*> parseModelOpts <*>
    parseParamOpts <*> parseNrHaps <*> parseRho <*> parseChromLength
  where
    parseNrHaps = OP.option (map read . splitOn "," <$> OP.str) $
        OP.long "nrHaps" <> OP.short 'n' <> OP.metavar "N1,N2,..." <>
        OP.help "specify the number of chromosomes needed per population, as \
                            \a comma-separated list of integers"
    parseRho = OP.option OP.auto $ OP.short 'r' <> OP.long "rho" <>
        OP.hidden <> OP.metavar "FLOAT" <> OP.value 0.0004 <> OP.showDefault <>
        OP.help "set the scaled recombination rate."
    parseChromLength = OP.option OP.auto $ OP.short 'L' <>
        OP.long "chromLength" <> OP.metavar "INT" <> OP.value 100000000 <>
        OP.help "specify the length of the simulated chromosomes in basepairs"
