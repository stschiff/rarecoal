{-# LANGUAGE OverloadedStrings #-}

module Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..),
                            SitePattern(..),
                            filterMaxAf, loadHistogram, filterGlobalMinAf,
                            readHistogram,
                            readHistogramFromHandle, computeStandardOrder) where

import Rarecoal.Utils (computeAllConfigs)

import qualified Data.Map.Strict as Map
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import Control.Monad (when, (>=>))
import Data.Int (Int64)
import Control.Error (Script, scriptIO, tryRight, throwE)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import qualified Pipes.Text.IO as PT
import System.IO (Handle, openFile, IOMode(..), hClose)
import Control.Monad.Trans.State.Strict (evalStateT)
import Pipes.Attoparsec (parse)

data RareAlleleHistogram = RareAlleleHistogram {
    raNames :: [String],
    raNVec :: [Int],
    raMinAf :: Int,
    raMaxAf :: Int,
    raConditionOn :: [Int],
    raExcludePatterns :: [[Int]],
    raCounts :: Map.Map SitePattern Int64
}

data SitePattern = Pattern [Int] | Higher deriving (Eq, Ord)
instance Show SitePattern where
    show (Pattern nVec) = intercalate "," . map show $ nVec
    show Higher = "HIGHER"

readHistogram :: FilePath -> Script RareAlleleHistogram
readHistogram path = do
    h <- scriptIO $ openFile path ReadMode
    hist <- readHistogramFromHandle h
    scriptIO $ hClose h
    return hist

readHistogramFromHandle :: Handle -> Script RareAlleleHistogram
readHistogramFromHandle handle = do
    res <- evalStateT (parse parseHistogram) . PT.fromHandle $ handle
    case res of
        Nothing -> throwE "histogram file exhausted too early"
        Just (Left err) -> throwE $ "Histogram parsing error: " ++ show err
        Just (Right hist) -> return hist

parseHistogram :: A.Parser RareAlleleHistogram
parseHistogram = RareAlleleHistogram <$> (map T.unpack <$> parseNames) <*>
    parseNVec <*> pure 0 <*> parseMaxM <*> pure [] <*> pure [] <*> parseBody
  where
    parseNames = A.string "NAMES=" *> name `A.sepBy1` A.char ',' <* A.endOfLine
    name = A.takeWhile1 (\c -> isAlphaNum c || c == '_')
    parseNVec = A.string "N=" *> A.decimal `A.sepBy1` A.char ',' <* A.endOfLine
    parseMaxM = A.string "MAX_M=" *> A.decimal <* A.endOfLine

parseBody :: A.Parser (Map.Map SitePattern Int64)
parseBody = Map.fromList <$> A.many1 patternLine
  where
    patternLine = (,) <$> (parsePattern <|> parseHigher) <* A.space <*>
        parseLargeInt <* A.endOfLine
    parsePattern = Pattern <$> A.decimal `A.sepBy1` A.char ','
    parseHigher = A.string "HIGHER" *> pure Higher
    parseLargeInt = read <$> A.many1 A.digit

loadHistogram :: Int -> Int -> [Int] -> [[Int]] -> FilePath ->
    Script RareAlleleHistogram
loadHistogram minAf maxAf conditionOn excludePatterns path = do
    hist <- readHistogram path
    tryRight $ (filterMaxAf maxAf >=> filterGlobalMinAf minAf >=>
        filterConditionOn conditionOn >=> filterExcludePatterns excludePatterns)
        hist

filterConditionOn :: [Int] -> RareAlleleHistogram ->
    Either String RareAlleleHistogram
filterConditionOn indices hist =
    if null indices then return hist else do
        let newBody = Map.mapKeysWith (+) conditionPatternOn (raCounts hist)
        return $ hist {raCounts = newBody, raConditionOn = indices}
  where
    conditionPatternOn (Pattern pat) =
        if all (\i -> pat !! i > 0) indices then Pattern pat else Higher
    conditionPatternOn Higher = Higher

filterExcludePatterns :: [[Int]] -> RareAlleleHistogram ->
    Either String RareAlleleHistogram
filterExcludePatterns excludePatterns hist =
    if null excludePatterns then return hist else do
        let newBody = Map.mapKeysWith (+) pruneExcludePatterns (raCounts hist)
        return $ hist {raExcludePatterns = excludePatterns, raCounts = newBody}
  where
    pruneExcludePatterns (Pattern pat) =
        if pat `elem` excludePatterns then Higher else Pattern pat
    pruneExcludePatterns Higher = Higher

filterMaxAf :: Int -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterMaxAf maxAf' hist = do
    let maxAf = if maxAf' == 0 then raMaxAf hist else maxAf'
    when (maxAf > raMaxAf hist || maxAf < raMinAf hist) $ Left "illegal maxAF"
    if maxAf == raMaxAf hist then return hist else do
        let newBody = Map.mapKeysWith (+) (prunePatternFreq maxAf) (raCounts hist)
        return $ hist {raMaxAf = maxAf, raCounts = newBody}
  where
    prunePatternFreq maxM (Pattern pat) =
        if sum pat > maxM then Higher else Pattern pat
    prunePatternFreq _ Higher = Higher

filterGlobalMinAf :: Int -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterGlobalMinAf minAf hist = do
    when (minAf > raMaxAf hist || minAf < raMinAf hist) $ Left "illegal minAf"
    if minAf == raMinAf hist then return hist else do
        let newBody = Map.mapKeysWith (+) prunePatternMinTotalFreq (raCounts hist)
        return $ hist {raCounts = newBody, raMinAf = minAf}
  where
    prunePatternMinTotalFreq (Pattern pat) = if sum pat < minAf then Higher else Pattern pat
    prunePatternMinTotalFreq Higher = Higher

computeStandardOrder :: RareAlleleHistogram -> Either String [[Int]]
computeStandardOrder histogram =
    let nrPop = length $ raNVec histogram
        nVec = raNVec histogram
    in  Right . filter (\p -> sum p >= raMinAf histogram &&
            hasConditioning (raConditionOn histogram) p &&
            p `notElem` raExcludePatterns histogram) $
            computeAllConfigs nrPop (raMaxAf histogram) nVec
  where
    hasConditioning indices pat = all (\i -> pat !! i > 0) indices
