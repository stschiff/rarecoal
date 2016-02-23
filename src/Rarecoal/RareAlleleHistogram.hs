{-# LANGUAGE OverloadedStrings #-}

module Rarecoal.RareAlleleHistogram (RareAlleleHistogram(..),
                            SitePattern(..),
                            addHistograms, filterMaxAf, setNrCalledSites,
                            loadHistogram, filterGlobalMinAf, readHistogram, showHistogram, readHistogramFromHandle) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate, sortBy)
import Control.Monad (when, (<=<))
import Data.Int (Int64)
import Control.Error (Script, scriptIO, assertErr, tryRight, throwE, justErr)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import qualified Pipes.Text.IO as PT
import System.IO (Handle, openFile, IOMode(..), hClose)
import Control.Monad.Trans.State.Strict (evalStateT)
import Pipes.Attoparsec (parse)

data RareAlleleHistogram = RareAlleleHistogram {
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

showHistogram :: RareAlleleHistogram -> Either String T.Text
showHistogram hist = do
    assertErr "can only print histogram with minAf=0 due to format-legacy" $ raMinAf hist == 0
    assertErr "can only print histogram with no conditioning due to format-legacy" $ length (raConditionOn hist) == 0
    let head1 = T.concat ["N=", T.pack . intercalate "," . map show . raNVec $ hist]
        head2 = T.concat ["MAX_M=", T.pack . show . raMaxAf $ hist]
        body = [T.intercalate " " [T.pack . show $ k, T.pack . show $ v] | (k, v) <- sorted]
    return $ T.unlines (head1:head2:body)
  where
    sorted = sortBy (\(_, v1) (_, v2)  -> compare v2 v1) $ Map.toList (raCounts hist)

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
        Nothing -> throwE "file exhausted too early"
        Just (Left err) -> throwE $ "Parser error: " ++ show err
        Just (Right hist) -> return hist
    
parseHistogram :: A.Parser RareAlleleHistogram
parseHistogram = RareAlleleHistogram <$> parseNVec <*> pure 0 <*> parseMaxM <*> pure [] <*> parseBody
  where
    parseNVec = A.string "N=" *> A.decimal `A.sepBy1` A.char ',' <* A.endOfLine
    parseMaxM = A.string "MAX_M=" *> A.decimal <* A.endOfLine
    parseBool = do
        s <- A.string "False" <|> A.string "True"
        if s == "False" then return False else return True

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

addHistograms :: RareAlleleHistogram -> RareAlleleHistogram -> Either String RareAlleleHistogram
addHistograms hist1 hist2 = do
    when (raNVec hist1 /= raNVec hist2) $ Left "histograms have different NVecs"
    when (raMaxAf hist1 /= raMaxAf hist2) $ Left "histograms have different maxAf"
    when (raConditionOn hist1 /= raConditionOn hist2) $ Left "histograms differ in conditioning"
    return $ hist1 {raCounts = Map.unionWith (+) (raCounts hist1) (raCounts hist2)}
        
loadHistogram :: Int -> Int -> [Int] -> Int64 -> FilePath -> Script RareAlleleHistogram
loadHistogram minAf maxAf conditionOn nrCalledSites path = do
    hist <- readHistogram path
    let f = if nrCalledSites > 0 then setNrCalledSites nrCalledSites else return
    tryRight $ (filterConditionOn conditionOn <=< filterGlobalMinAf minAf <=< filterMaxAf maxAf <=< f) hist

filterConditionOn :: [Int] -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterConditionOn indices hist =
    if (null indices) then return hist else do
        let newBody = Map.filterWithKey conditionPatternOn (raCounts hist)
        return $ hist {raCounts = newBody, raConditionOn = indices}
  where
    conditionPatternOn Higher _ = False
    conditionPatternOn (Pattern pat) _ = all (\i -> pat !! i > 0) indices

setNrCalledSites :: Int64 -> RareAlleleHistogram -> Either String RareAlleleHistogram
setNrCalledSites newNrCalledSites hist = do
    let zeroKey = Pattern $ replicate (length $ raNVec hist) 0
    nrZeroCalls <- justErr ("did not find key " ++ show zeroKey) $ zeroKey `Map.lookup` (raCounts hist)
    let nrCalledSites = Map.foldr (+) 0 $ raCounts hist
        nrNonZeroCalls = nrCalledSites - nrZeroCalls
        newZeroCalls = newNrCalledSites - nrNonZeroCalls
    when (newZeroCalls < 0) $ Left "Illegal nrCalledSites" 
    let newHistBody = Map.insert zeroKey newZeroCalls (raCounts hist)
    return $ hist {raCounts = newHistBody}

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

-- combineIndices :: [Int] -> RareAlleleHistogram -> Either String RareAlleleHistogram
-- combineIndices indices hist =
--     if null indices then
--         return hist
--     else do
--         when (not . null . raConditionOn $ hist) $ Left "Histogram cannot have conditioning for combining indices"
--         let newNvec = combineInPattern indices (raNVec hist)
--             newBody = Map.mapKeysWith (+) transformPattern (raCounts hist)
--         return $ hist {raNVec = newNvec, raCounts = newBody}
--   where
--     transformPattern Higher = Higher
--     transformPattern (Pattern pattern) =
--         let newPattern = combineInPattern indices pattern
--             maxAf = raMaxAf hist
--             tooHigh = sum newPattern > raMaxAf hist
--         in  if tooHigh then Higher else Pattern newPattern
--
-- combineInPattern :: [Int] -> [Int] -> [Int]
-- combineInPattern indices pattern =
--     let newCount = sum $ map (pattern!!) indices
--         restPattern = removeFromList pattern indices
--     in  insertIntoList restPattern (head indices) newCount

-- prunePatternIndices :: [Int] -> SitePattern -> SitePattern
-- prunePatternIndices indices (Pattern pattern) = Pattern $ selectFromList pattern indices
-- prunePatternIndices _ Higher = Higher

prunePatternFreq :: Int -> SitePattern -> SitePattern
prunePatternFreq maxM (Pattern pattern) = if sum pattern > maxM then Higher else Pattern pattern
prunePatternFreq _ Higher = Higher

prunePatternMinTotalFreq :: Int -> SitePattern -> SitePattern
prunePatternMinTotalFreq minM (Pattern pattern) = if sum pattern < minM then Higher else Pattern pattern
prunePatternMinTotalFreq _ Higher = Higher

-- selectFromList :: [a] -> [Int] -> [a]
-- selectFromList l [] = l
-- selectFromList l i = map (l!!) i
--
-- insertIntoList :: [a] -> Int -> a -> [a]
-- insertIntoList l i el =
--     let (l1,l2) = splitAt i l in l1 ++ [el] ++ l2
--
-- removeFromList :: [a] -> [Int] -> [a]
-- removeFromList l indices =
--     [el | (el, i) <- zip l [0..],  i `notElem` indices]

