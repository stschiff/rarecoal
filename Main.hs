import Core (defaultTimes, getProb, Join)
import System.Environment (getArgs)
import Data.List.Split (splitOn)
import Data.Monoid (mempty)
import Control.Monad (liftM, when)
import Control.Applicative ((<$>), (<*>), pure)
import qualified Options.Applicative as OP
import Data.Monoid ((<>))
import System.Exit (exitFailure)

data Options = Options {
    optIndices :: [Int],
    optMaxAf :: Int,
    optLambda :: [Double],
    optJoins :: [Join],
    optCommand :: Command
}

data Command = View | Prob [Int] [Int]

main :: IO ()
main = run =<< OP.execParser (parseOptions `withInfo` "Rarecoal: Implementation of the Rarecoal algorithm")

parseOptions :: OP.Parser Options
parseOptions = Options <$> parseIndices <*> parseMaxAf <*> parseLambda <*> parseJoins <*> parseCommand

parseIndices :: OP.Parser [Int]
parseIndices = OP.option OP.auto $ OP.short 'i' <> OP.long "indices" <> OP.metavar "[i1,i2,...]"
                                        <> OP.value []
                                        <> OP.help "Read only these indices from the input file"

parseMaxAf :: OP.Parser Int
parseMaxAf = OP.option OP.auto $ OP.short 'm' <> OP.long "max_af" <> OP.metavar "INT"
                                        <> OP.value 10
                                        <> OP.help "set the maximum allele frequency"

parseLambda :: OP.Parser [Double]
parseLambda = OP.option OP.auto $ OP.short 'p' <> OP.long "pop_size" <> OP.metavar "[l1,l2,...]"
                                        <> OP.value []
                                        <> OP.help "Initial inverse population sizes"

parseJoins :: OP.Parser [Join]
parseJoins = OP.option OP.auto $ OP.short 'j' <> OP.long "joins" <> OP.metavar "[(t1,k1,l1,lambda1),(t2,k2,l2,lambda2),...]"
                                        <> OP.value []
                                        <> OP.help "Joins"

parseCommand :: OP.Parser Command
parseCommand = OP.subparser $
    OP.command "view" (parseView `withInfo` "View the input file") <>
    OP.command "prob" (parseProb `withInfo` "Compute probability for a given configuration")

parseView :: OP.Parser Command
parseView = pure View

withInfo :: OP.Parser a -> String -> OP.ParserInfo a
withInfo opts desc = OP.info (OP.helper <*> opts) $ OP.progDesc desc

parseProb :: OP.Parser Command
parseProb = Prob <$> OP.argument OP.auto (OP.metavar "NVec")
                 <*> OP.argument OP.auto (OP.metavar "MVec")

theta = 0.0005

run :: Options -> IO ()
run (Options indices max_af lambda joins cmd) = do
    case cmd of
        View -> print "View was selected"
        Prob nVec mVec -> do
            case getProb defaultTimes lambda joins theta nVec mVec of
                Left err -> do
                    putStrLn $ "Error: " ++ err
                    exitFailure
                Right result -> do
                    print result
    
    
    