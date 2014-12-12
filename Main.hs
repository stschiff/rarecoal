import Data.List.Split (splitOn)
import Control.Applicative ((<$>), (<*>), many, (<|>))
import qualified Options.Applicative as OP
import Data.Monoid ((<>))
import Logl (runLogl, LoglOpt(..))
import Maxl (MaxlOpt(..), runMaxl)
import Mcmc (McmcOpt(..), runMcmc)
import View (ViewOpt(..), runView)
import Prob (ProbOpt(..), runProb)
import System.Log.Logger (updateGlobalLogger, setLevel, Priority(..), infoM)
import Data.Time.Clock (getCurrentTime)
import Core (ModelEvent(..), EventType(..))
import Control.Error.Script (runScript, scriptIO)
import Data.Int (Int64)

data Options = Options Command

data Command = CmdView ViewOpt | CmdProb ProbOpt | CmdLogl LoglOpt | CmdMaxl MaxlOpt | CmdMcmc McmcOpt 

main :: IO ()
main = run =<< OP.execParser (parseOptions `withInfo` "Rarecoal: Implementation of the Rarecoal algorithm")

run :: Options -> IO ()
run (Options cmdOpts) = runScript $ do
    scriptIO $ updateGlobalLogger "rarecoal" (setLevel INFO)
    currentT <- scriptIO $ getCurrentTime
    scriptIO $ infoM "rarecoal" $ "Starting at " ++ show currentT
    case cmdOpts of
        CmdView opts -> runView opts
        CmdProb opts -> runProb opts
        CmdLogl opts -> runLogl opts
        CmdMaxl opts -> runMaxl opts
        CmdMcmc opts -> runMcmc opts
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
    OP.command "mcmc" (parseMcmc `withInfo` "Run MCMC on the model and the data")

parseView :: OP.Parser Command
parseView = CmdView <$> parseViewOpt

parseViewOpt :: OP.Parser ViewOpt
parseViewOpt = ViewOpt <$> parseIndices <*> parseCombineIndices <*> parseMaxAf <*> parseNrCalledSites <*> parseHistPath

parseIndices :: OP.Parser [Int]
parseIndices = OP.option OP.auto $ OP.short 'I' <> OP.long "indices"
                                                <> OP.metavar "[i1,i2,...]"
                                                <> OP.value []
                                                <> OP.help "Read only these indices from the input file"

parseCombineIndices :: OP.Parser [Int]
parseCombineIndices = OP.option OP.auto $ OP.long "combine"
                                                <> OP.metavar "[i1,i2,...]"
                                                <> OP.value []
                                                <> OP.help "combine these indices into one"

parseMaxAf :: OP.Parser Int
parseMaxAf = OP.option OP.auto $ OP.short 'm' <> OP.long "max_af"
                                              <> OP.metavar "INT"
                                              <> OP.value 10
                                              <> OP.help "set the maximum allele frequency (default:10)"

parseNrCalledSites :: OP.Parser Int64
parseNrCalledSites = OP.option OP.auto $ OP.short 'N' <> OP.long "nr_called_sites"
                                                      <> OP.metavar "INT"
                                                      <> OP.value 0
                                                      <> OP.help "set the nr of called sites"

parseHistPath :: OP.Parser FilePath
parseHistPath = OP.option OP.str $ OP.short 'i' <> OP.long "input"
                                                <> OP.metavar "<Input File>" <> OP.help "Input File"

withInfo :: OP.Parser a -> String -> OP.ParserInfo a
withInfo opts desc = OP.info (OP.helper <*> opts) $ OP.progDesc desc

parseProb :: OP.Parser Command
parseProb = CmdProb <$> parseProbOpt

parseProbOpt :: OP.Parser ProbOpt
parseProbOpt = ProbOpt <$> parseTheta <*> parseTemplateFilePath <*> parseParams <*> parseModelEvents
                       <*> OP.argument OP.auto (OP.metavar "NVec")
                       <*> OP.argument OP.auto (OP.metavar "MVec")

parseTheta :: OP.Parser Double
parseTheta = OP.option OP.auto $ OP.short 't' <> OP.long "theta"
                                              <> OP.metavar "DOUBLE"
                                              <> OP.value 0.0005
                                              <> OP.help "set the scaled mutation rate [default:0.005]"

parseTemplateFilePath :: OP.Parser FilePath
parseTemplateFilePath = OP.option OP.str $ OP.short 'T' <> OP.long "template" <> OP.metavar "<Input Template File>" <> OP.value "/dev/null"
                                                              <> OP.help "Specify that the model should be read from a template file"

parseParams :: OP.Parser [Double]
parseParams = OP.option OP.auto $ OP.short 'x' <> OP.long "params"
                                               <> OP.metavar "[p1,p2,...]"
                                               <> OP.value []
                                               <> OP.help "initial parameters for the template"

parseModelEvents :: OP.Parser [ModelEvent]
parseModelEvents = many parseEvent

parseEvent :: OP.Parser ModelEvent
parseEvent = parseJoin <|> parseSetP <|> parseSetR

parseJoin :: OP.Parser ModelEvent
parseJoin = OP.option readJoin $ OP.short 'j' <> OP.long "join"
                                              <> OP.metavar "t,k,l"
                                              <> OP.help "Join at time t from l into k. Can be given multiple times"
  where
    readJoin s = do
        let [t, k, l] = splitOn "," s
        return $ ModelEvent (read t) (Join (read k) (read l))

parseSetP :: OP.Parser ModelEvent
parseSetP = OP.option readSetP $ OP.short 'p' <> OP.long "popSize"
                                              <> OP.metavar "t,k,p"
                                              <> OP.help "At time t, set population size in k to p, and set growth rate to 0"
  where
    readSetP s = do
        let [t, k, p] = splitOn "," s
        return $ ModelEvent (read t) (SetPopSize (read k) (read p))

parseSetR :: OP.Parser ModelEvent
parseSetR = OP.option readSetR $ OP.short 'r' <> OP.long "growthRate"
                                              <> OP.metavar "t,k,r"
                                              <> OP.help "At time t, set growth rate in k to r"
  where
    readSetR s = do
        let [t, k, r] = splitOn "," s
        return $ ModelEvent (read t) (SetGrowthRate (read k) (read r))

parseLogl :: OP.Parser Command
parseLogl = CmdLogl <$> parseLoglOpt

parseLoglOpt :: OP.Parser LoglOpt
parseLoglOpt = LoglOpt <$> parseSpectrumPath <*> parseTheta <*> parseTemplateFilePath <*> parseParams
                                 <*> parseModelEvents <*> parseMaxAf
                                 <*> parseNrCalledSites <*> parseHistPath

parseSpectrumPath :: OP.Parser FilePath
parseSpectrumPath = OP.option OP.str $ OP.short 's' <> OP.long "spectrumFile"
                                                <> OP.metavar "<Output Spectrum File>"
                                                <> OP.value "/dev/null"
                                                <> OP.help "Output the allele frequencies to file"


parseMaxl :: OP.Parser Command
parseMaxl = CmdMaxl <$> parseMaxlOpt

parseMaxlOpt :: OP.Parser MaxlOpt
parseMaxlOpt = MaxlOpt <$> parseTheta <*> parseTemplateFilePath <*> parseParams <*> parseMaxCycles
                       <*> parseTraceFilePath  <*> parseMaxAf
                       <*> parseNrCalledSites <*> parseHistPath
  where
    parseMaxCycles = OP.option OP.auto $ OP.short 'c' <> OP.long "maxCycles"
                                                      <> OP.metavar "<NR_MAX_CYCLES>"
                                                      <> OP.value 10000
                                                      <> OP.help "Specifies the maximum number of cycles in the minimization routine"

parseTraceFilePath :: OP.Parser FilePath
parseTraceFilePath = OP.option OP.str $ OP.short 'f' <> OP.long "traceFile" <> OP.metavar "<FILE>"
                                                            <> OP.value "/dev/null"
                                                            <> OP.help "The file to write the trace"

parseMcmc :: OP.Parser Command
parseMcmc = CmdMcmc <$> parseMcmcOpt

parseMcmcOpt :: OP.Parser McmcOpt
parseMcmcOpt = McmcOpt <$> parseTheta <*> parseTemplateFilePath <*> parseParams
                       <*> parseNrBurninCycles <*> parseNrMainCycles <*> parseTraceFilePath
                       <*> parseMaxAf <*> parseNrCalledSites
                       <*> parseHistPath <*> parseRandomSeed
  where
    parseRandomSeed = OP.option OP.auto $ OP.short 'S' <> OP.long "seed" <> OP.metavar "<INT>" <> OP.help "Random Seed"
    parseNrBurninCycles = OP.option OP.auto $ OP.long "burnin" <> OP.short 'b' <> OP.value 2000 <> OP.metavar "<INT>"
                                                               <> OP.help "nr of burnin cycles"
    parseNrMainCycles = OP.option OP.auto $ OP.long "cycles" <> OP.short 'c' <> OP.value 10000 <> OP.metavar "<INT>"
                                                               <> OP.help "nr of main MCMC cycles"

