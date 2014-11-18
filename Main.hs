import Core (defaultTimes, getProb, ModelSpec(..), Join(..))
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Monoid (mempty)
import Control.Monad (liftM, when)
import Control.Applicative ((<$>), (<*>), pure)
import qualified Options.Applicative as OP
import Data.Monoid ((<>))
import System.Exit (exitFailure)
import RareAlleleHistogram (RareAlleleHistogram(..), loadHistogram, InputSpec(..))
import Logl (computeLikelihood)

data Options = Options {
    optCommand :: Command
}

data Command = CmdView InputSpec | CmdProb ModelSpec [Int] [Int] | CmdLogl ModelSpec InputSpec

main :: IO ()
main = run =<< OP.execParser (parseOptions `withInfo` "Rarecoal: Implementation of the Rarecoal algorithm")

run :: Options -> IO ()
run (Options cmd) = do
    case cmd of
        CmdView inputSpec -> do
            hist <- loadHistogram inputSpec
            putStr $ show hist
        CmdProb modelSpec nVec mVec -> do
            case getProb modelSpec nVec mVec of
                Left err -> do
                    putStrLn $ "Error: " ++ err
                    exitFailure
                Right result -> do
                    print result
        CmdLogl modelSpec inputSpec -> do
            hist <- loadHistogram inputSpec
            print $ computeLikelihood modelSpec hist
    
parseOptions :: OP.Parser Options
parseOptions = Options <$> parseCommand

withInfo :: OP.Parser a -> String -> OP.ParserInfo a
withInfo opts desc = OP.info (OP.helper <*> opts) $ OP.progDesc desc

parseCommand :: OP.Parser Command
parseCommand = OP.subparser $
    OP.command "view" (parseView `withInfo` "View the input file") <>
    OP.command "prob" (parseProb `withInfo` "Compute probability for a given configuration") <>
    OP.command "logl" (parseLogl `withInfo` "Compute the likelihood of the given model for the given data set")

parseView :: OP.Parser Command
parseView = CmdView <$> parseInputSpec

parseProb :: OP.Parser Command
parseProb = CmdProb <$> parseModelSpec
                    <*> OP.argument OP.auto (OP.metavar "NVec")
                    <*> OP.argument OP.auto (OP.metavar "MVec")

parseLogl :: OP.Parser Command
parseLogl = CmdLogl <$> parseModelSpec <*> parseInputSpec

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

parseModelSpec :: OP.Parser ModelSpec
parseModelSpec = ModelSpec defaultTimes <$> parseLambda <*> parseJoins <*> parseTheta
  where
    parseLambda = OP.option OP.auto $ OP.short 'p' <> OP.long "pop_size"
                                                   <> OP.metavar "[l1,l2,...]"
                                                   <> OP.value []
                                                   <> OP.help "Initial inverse population sizes"
    
    parseJoins = OP.option OP.auto $ OP.short 'j' <> OP.long "joins"
                                                  <> OP.metavar "[(t1,k1,l1,lambda1),(t2,k2,l2,lambda2),...]"
                                                  <> OP.value []
                                                  <> OP.help "Joins"

    parseTheta = OP.option OP.auto $ OP.short 't' <> OP.long "theta"
                                                  <> OP.metavar "DOUBLE"
                                                  <> OP.value 0.0005
                                                  <> OP.help "set the scaled mutation rate [default:0.005]"
