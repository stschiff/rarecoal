{-# LANGUAGE OverloadedStrings #-}

module Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..),
                            SitePattern(..),
                            filterMaxAf, loadHistogram, filterGlobalMinAf, readHistogram, 
                            readHistogramFromHandle) where

import qualified Data.Map.Strict as Map
import Data.Char (isAlphaNum)
import Data.List (intercalate)
import Control.Monad (when, (<=<))
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
parseHistogram = RareAlleleHistogram <$> (map T.unpack <$> parseNames) <*> parseNVec <*> pure 0 <*> 
                                         parseMaxM <*> pure [] <*> parseBody
  where
    parseNames = A.string "NAMES=" *> name `A.sepBy1` A.char ',' <* A.endOfLine
    name = A.takeWhile1 (\c -> isAlphaNum c || c == '_')
    parseNVec = A.string "N=" *> A.decimal `A.sepBy1` A.char ',' <* A.endOfLine
    parseMaxM = A.string "MAX_M=" *> A.decimal <* A.endOfLine

parseBody :: A.Parser (Map.Map SitePattern Int64)
parseBody = Map.fromList <$> A.many1 patternLine
  where
    patternLine = do
        pat <- parsePattern <|> parseHigher
        _ <- A.space
        num <- parseLargeInt
        _ <- A.endOfLine
        return (pat, num)
    parsePattern = Pattern <$> A.decimal `A.sepBy1` A.char ','
    parseHigher = A.string "HIGHER" *> pure Higher
    parseLargeInt = read <$> A.many1 A.digit

loadHistogram :: Int -> Int -> [Int] -> FilePath -> Script RareAlleleHistogram
loadHistogram minAf maxAf conditionOn path = do
    hist <- readHistogram path
    tryRight $ (filterConditionOn conditionOn <=< filterGlobalMinAf minAf <=< filterMaxAf maxAf) hist

filterConditionOn :: [Int] -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterConditionOn indices hist =
    if (null indices) then return hist else do
        let newBody = Map.filterWithKey conditionPatternOn (raCounts hist)
        return $ hist {raCounts = newBody, raConditionOn = indices}
  where
    conditionPatternOn Higher _ = False
    conditionPatternOn (Pattern pat) _ = all (\i -> pat !! i > 0) indices

filterMaxAf :: Int -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterMaxAf maxAf' hist = do
    let maxAf = if maxAf' == 0 then raMaxAf hist else maxAf'
    when (maxAf > raMaxAf hist || maxAf < raMinAf hist) $ Left "illegal maxAF"
    if maxAf == raMaxAf hist then return hist else do
        let newBody = Map.mapKeysWith (+) (prunePatternFreq maxAf) (raCounts hist)
        return $ hist {raMaxAf = maxAf, raCounts = newBody}

filterGlobalMinAf :: Int -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterGlobalMinAf minAf hist = do
    when (minAf > raMaxAf hist || minAf < raMinAf hist) $ Left "illegal minAf"
    if minAf == raMinAf hist then return hist else do
        let newBody = Map.mapKeysWith (+) (prunePatternMinTotalFreq minAf) (raCounts hist)
        return $ hist {raCounts = newBody, raMinAf = minAf}

prunePatternFreq :: Int -> SitePattern -> SitePattern
prunePatternFreq maxM (Pattern pattern) = if sum pattern > maxM then Higher else Pattern pattern
prunePatternFreq _ Higher = Higher

prunePatternMinTotalFreq :: Int -> SitePattern -> SitePattern
prunePatternMinTotalFreq minM (Pattern pattern) = if sum pattern < minM then Higher else Pattern pattern
prunePatternMinTotalFreq _ Higher = Higher

