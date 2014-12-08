import Data.List.Split (splitOn)
import Control.Monad (when)
import Control.Applicative ((<$>), (<*>), many, (<|>))
import qualified Options.Applicative as OP
import Data.Monoid ((<>))
import System.Exit (exitFailure)
import RareAlleleHistogram (loadHistogram, InputSpec(..))
import Logl (computeLikelihood, writeSpectrumFile)
import Maxl (maximizeLikelihood, reportMaxResult, reportTrace)
import Mcmc (runMcmc, reportMcmcResult, reportMcmcTrace, readMaxResult)
import System.Log.Logger (updateGlobalLogger, setLevel, Priority(..), errorM, infoM)
import Data.Time.Clock (getCurrentTime)
import ModelSpec (ModelSpec(..), readModelTemplate, ModelEvent(..), EventType(..), instantiateModel)
import Core (getProb, defaultTimes)
import Control.Monad.IO.Class (liftIO)
import Control.Error.Script (runScript)
import qualified Data.Vector.Unboxed as V

data Options = Options Command

data Command = CmdView InputSpec
             | CmdProb ModelOpt [Int] [Int]
             | CmdLogl FilePath ModelOpt InputSpec
             | CmdMaxl Double FilePath [Double] Int FilePath InputSpec
             | CmdMcmc Double FilePath FilePath Int FilePath InputSpec Int

data ModelOpt = ModelTemplateOpt Double FilePath [Double] | ModelSpecOpt ModelSpec

main :: IO ()
main = run =<< OP.execParser (parseOptions `withInfo` "Rarecoal: Implementation of the Rarecoal algorithm")

run :: Options -> IO ()
run (Options cmd) = do
    updateGlobalLogger "rarecoal" (setLevel INFO)
    currentT <- getCurrentTime
    infoM "rarecoal" $ "Starting at " ++ show currentT
    case cmd of
        CmdView inputSpec -> do
            hist <- loadHistogram inputSpec
            putStr $ show hist
        CmdProb modelOpt nVec mVec -> do
            modelSpec <- getModelSpec modelOpt
            case getProb modelSpec nVec mVec of
                Left err -> do
                    errorM "rarecoal" $ "Error: " ++ err
                    exitFailure
                Right result -> print result
        CmdLogl spectrumFile modelOpt inputSpec -> do
            modelSpec <- getModelSpec modelOpt
            hist <- loadHistogram inputSpec
            when (spectrumFile /= "/dev/null") $ writeSpectrumFile spectrumFile modelSpec hist
            case computeLikelihood modelSpec hist of
                Left err -> do
                    errorM "rarecoal" $ "Error: " ++ err
                    exitFailure
                Right result -> print result
        CmdMaxl theta mtPath params maxCycles path inputSpec -> do
            hist <- loadHistogram inputSpec
            modelTemplate <- readModelTemplate mtPath theta defaultTimes
            case maximizeLikelihood modelTemplate (V.fromList params) hist maxCycles of
                Left err -> errorM "rarecoal" $ "Error: " ++ err
                Right (s, p) -> do
                    reportMaxResult modelTemplate s 
                    reportTrace modelTemplate p path 
        CmdMcmc theta mtPath maxResultPath maxCycles path inputSpec seed -> runScript $ do
            hist <- liftIO $ loadHistogram inputSpec
            modelTemplate <- liftIO $ readModelTemplate mtPath theta defaultTimes
            initParams <- liftIO $ readMaxResult maxResultPath
            (res, trace) <- runMcmc modelTemplate (V.fromList initParams) hist maxCycles seed
            liftIO $ reportMcmcResult modelTemplate res 
            liftIO $ reportMcmcTrace modelTemplate trace path 
    currentTafter <- getCurrentTime
    infoM "rarecoal" $ "Finished at " ++ show currentTafter

getModelSpec :: ModelOpt -> IO ModelSpec
getModelSpec modelOpt =
    case modelOpt of
        ModelTemplateOpt theta path params -> do
            mt <- readModelTemplate path theta defaultTimes
            return $ instantiateModel mt (V.fromList params)
        ModelSpecOpt modelSpec -> return modelSpec

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
parseView = CmdView <$> parseInputSpec

parseInputSpec :: OP.Parser InputSpec
parseInputSpec = InputSpec <$> parseIndices <*> parseMaxAf <*> parseNrCalledSites
                           <*> OP.argument OP.str (OP.metavar "<Input Histogram File>")
  where
    parseIndices = OP.option OP.auto $ OP.short 'i' <> OP.long "indices"
                                                    <> OP.metavar "[i1,i2,...]"
                                                    <> OP.value []
                                                    <> OP.help "Read only these indices from the input file"
    parseMaxAf = OP.option OP.auto $ OP.short 'm' <> OP.long "max_af"
                                                  <> OP.metavar "INT"
                                                  <> OP.value 10
                                                  <> OP.help "set the maximum allele frequency (default:10)"
    parseNrCalledSites = OP.option OP.auto $ OP.short 'n' <> OP.long "nr_called_sites"
                                                          <> OP.metavar "INT"
                                                          <> OP.value 2064554803
                                                          <> OP.help "set the nr of called sites (default:2064554803)"

withInfo :: OP.Parser a -> String -> OP.ParserInfo a
withInfo opts desc = OP.info (OP.helper <*> opts) $ OP.progDesc desc

parseProb :: OP.Parser Command
parseProb = CmdProb <$> parseModelOpt
                    <*> OP.argument OP.auto (OP.metavar "NVec")
                    <*> OP.argument OP.auto (OP.metavar "MVec")

parseLogl :: OP.Parser Command
parseLogl = CmdLogl <$> parseSpectrumFile <*> parseModelOpt <*> parseInputSpec
  where
    parseSpectrumFile = OP.option OP.str $ OP.short 's' <> OP.long "spectrumFile"
                                                        <> OP.metavar "<Output Spectrum File>"
                                                        <> OP.value "/dev/null"
                                                        <> OP.help "Output the allele frequencies to file"

parseModelOpt :: OP.Parser ModelOpt
parseModelOpt = parseModelTemplateOpt <|> parseModelSpecOpt

parseModelTemplateOpt :: OP.Parser ModelOpt
parseModelTemplateOpt = ModelTemplateOpt <$> parseTheta <*> parseTemplateFilePath <*> parseParams

parseTheta :: OP.Parser Double
parseTheta = OP.option OP.auto $ OP.short 't' <> OP.long "theta"
                                              <> OP.metavar "DOUBLE"
                                              <> OP.value 0.0005
                                              <> OP.help "set the scaled mutation rate [default:0.005]"
parseTemplateFilePath :: OP.Parser FilePath
parseTemplateFilePath = OP.option OP.str $ OP.long "template" <> OP.metavar "<Input Template File>"
                                                              <> OP.help "Specify that the model should be read from a template file"

parseParams :: OP.Parser [Double]
parseParams = OP.option OP.auto $ OP.short 'x' <> OP.long "params"
                                               <> OP.metavar "[p1,p2,...]"
                                               <> OP.help "initial parameters for the template"

parseModelSpecOpt :: OP.Parser ModelOpt
parseModelSpecOpt = ModelSpecOpt <$> parseModelSpec 

parseModelSpec :: OP.Parser ModelSpec
parseModelSpec = ModelSpec defaultTimes <$> parseTheta <*> many parseEvent
  where
   parseEvent = parseJoin <|> parseSetP <|> parseSetR

parseJoin :: OP.Parser ModelEvent
parseJoin = OP.option readJoin $ OP.short 'j' <> OP.long "join"
                                              <> OP.metavar "t,k,l"
                                              <> OP.help "Join at time t from l into k. Can be given multiple times"
  where
    -- readJoin :: Monad m => String -> m ModelEvent
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

parseMaxl :: OP.Parser Command
parseMaxl = CmdMaxl <$> parseTheta <*> parseTemplateFilePath <*> parseParams <*> parseMaxCycles <*> parseTraceFilePath <*> parseInputSpec

parseMaxCycles :: OP.Parser Int
parseMaxCycles = OP.option OP.auto $ OP.short 'c' <> OP.long "maxCycles"
                                                  <> OP.metavar "<NR_MAX_CYCLES>"
                                                  <> OP.value 100
                                                  <> OP.help "Specifies the maximum number of cycles in the minimization routine"

parseTraceFilePath :: OP.Parser FilePath
parseTraceFilePath = OP.option OP.str $ OP.long "traceFile" <> OP.metavar "<FILE>"
                                                            <> OP.value "/dev/null"
                                                            <> OP.help "The file to write the trace"

parseMcmc :: OP.Parser Command
parseMcmc = CmdMcmc <$> parseTheta
                    <*> parseTemplateFilePath
                    <*> parseMaxResultFilePath
                    <*> parseMaxCycles
                    <*> parseTraceFilePath
                    <*> parseInputSpec
                    <*> parseSeed
  where
    parseMaxResultFilePath = OP.option OP.str $ OP.long "maxResultFile" <> OP.metavar "<FILE>"
                                                                        <> OP.help "The result of the maximization step containing the maximimum likelihood estimates"
    parseSeed = OP.option OP.auto $ OP.long "seed" <> OP.metavar "<INT>" <> OP.help "Random Seed"

