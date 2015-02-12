module Maxl (minFunc, penalty, runMaxl, MaxlOpt(..)) where

import RareAlleleHistogram (RareAlleleHistogram, loadHistogram)
import Logl (computeLikelihood)
import Numeric.LinearAlgebra.Data (toRows, toList)
import Numeric.GSL.Minimization (minimize, MinimizeMethod(..))
import ModelTemplate (ModelTemplate(..), instantiateModel, readModelTemplate)
import Data.List (intercalate)
import Data.Int (Int64)
import Core (defaultTimes)
import qualified Data.Vector.Unboxed as V
import Control.Error (Script, scriptIO)
import Control.Error.Safe (assertErr)
import Control.Monad.Trans.Either (hoistEither)

data MaxlOpt = MaxlOpt {
   maTheta :: Double,
   maTemplatePath :: FilePath,
   maInitialParams :: [Double],
   maMaxCycles :: Int,
   maTracePath :: FilePath,
   maMaxAf :: Int,
   maNrCalledSites :: Int64,
   maIndices :: [Int],
   maHistPath :: FilePath
}

runMaxl :: MaxlOpt -> Script ()
runMaxl opts = do
    modelTemplate <- readModelTemplate (maTemplatePath opts) (maTheta opts) defaultTimes
    hist <- loadHistogram (maIndices opts) (maMaxAf opts) (maNrCalledSites opts) [] (maHistPath opts)
    _ <- hoistEither $ minFunc modelTemplate hist (V.fromList $ maInitialParams opts)
    let minFunc' = either (const penalty) id . minFunc modelTemplate hist . V.fromList
        stepWidths = [max 1.0e-4 $ abs (0.01 * p) | p <- maInitialParams opts]
        (minResult, trace) = minimize NMSimplex2 1.0e-8 (maMaxCycles opts) stepWidths minFunc' (maInitialParams opts)
        minScore = minFunc' minResult
        trace' = map toList $ toRows trace
    scriptIO $ reportMaxResult modelTemplate (V.fromList minResult) minScore
    scriptIO $ reportTrace modelTemplate trace' (maTracePath opts)

reportMaxResult :: ModelTemplate -> V.Vector Double -> Double -> IO ()
reportMaxResult modelTemplate result minScore = do
    putStrLn $ "Score\t" ++ show minScore
    putStr $ unlines $ zipWith (\p v -> p ++ "\t" ++ show v) (mtParams modelTemplate) (V.toList result)

reportTrace :: ModelTemplate -> [[Double]] -> FilePath -> IO ()
reportTrace modelTemplate trace path = do
    let header = intercalate "\t" $ ["Nr", "-Log-Likelihood", "Simplex size"] ++ mtParams modelTemplate
        body = unlines [intercalate "\t" [show val | val <- row] | row <- trace]
    writeFile path $ header ++ "\n" ++ body

minFunc :: ModelTemplate -> RareAlleleHistogram -> V.Vector Double -> Either String Double
minFunc modelTemplate hist params = do
    modelSpec <- instantiateModel modelTemplate params
    val <- computeLikelihood modelSpec hist
    assertErr ("likelihood infinite for params " ++ show params) $ not (isInfinite val)
    assertErr ("likelihood NaN for params " ++ show params) $ not (isNaN val)
    return (-val)

penalty :: Double
penalty = 1.0e20


