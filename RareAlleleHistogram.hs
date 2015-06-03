{-# LANGUAGE OverloadedStrings #-}

module RareAlleleHistogram (RareAlleleHistogram(..),
                            SitePattern(..),
                            addHistograms, filterMaxAf, setNrCalledSites,
                            loadHistogram, reduceIndices, combineIndices,
                            filterGlobalMinAf, readHistogram, showHistogram, readHistogramFromHandle) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate, sortBy)
import Control.Monad (when, (<=<))
import Data.Int (Int64)
import Control.Error (Script, scriptIO, assertErr, tryRight, left)
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
    raGlobalMax :: Bool,
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
        head3 = T.concat ["GLOBAL_MAX=", T.pack . show . raGlobalMax $ hist]
        body = [T.intercalate " " [T.pack . show $ k, T.pack . show $ v] | (k, v) <- sorted]
    return $ T.unlines (head1:head2:head3:body)
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
        Nothing -> left "file exhausted too early"
        Just (Left err) -> left $ "Parser error: " ++ show err
        Just (Right hist) -> return hist
    
parseHistogram :: A.Parser RareAlleleHistogram
parseHistogram = RareAlleleHistogram <$> parseNVec <*> pure 0 <*> parseMaxM <*> parseIsGlobal <*> pure [] <*> parseBody
  where
    parseNVec = A.string "N=" *> A.decimal `A.sepBy1` A.char ',' <* A.endOfLine
    parseMaxM = A.string "MAX_M=" *> A.decimal <* A.endOfLine
    parseIsGlobal = A.string "GLOBAL_MAX=" *> parseBool <* A.endOfLine
    parseBool = do
        s <- A.string "False" <|> A.string "True"
        if s == "False" then return False else return True

parseBody :: A.Parser (Map.Map SitePattern Int64)
parseBody = Map.fromList <$> A.many1 patternLine
  where
    patternLine = do
        pat <- parsePattern <|> parseHigher
        A.space
        num <- parseLargeInt
        A.endOfLine
        return (pat, num)
    parsePattern = Pattern <$> A.decimal `A.sepBy1` A.char ','
    parseHigher = A.string "HIGHER" *> pure Higher
    parseLargeInt = read <$> A.many1 A.digit

addHistograms :: RareAlleleHistogram -> RareAlleleHistogram -> Either String RareAlleleHistogram
addHistograms hist1 hist2 = do
    when (raNVec hist1 /= raNVec hist2) $ Left "histograms have different NVecs"
    when (raMaxAf hist1 /= raMaxAf hist2) $ Left "histograms have different maxAf"
    when (raGlobalMax hist1 /= raGlobalMax hist2) $ Left "histograms different locality of maximum"
    when (raConditionOn hist1 /= raConditionOn hist2) $ Left "histograms differ in conditioning"
    return $ hist1 {raCounts = Map.unionWith (+) (raCounts hist1) (raCounts hist2)}
        
loadHistogram :: [Int] -> Int -> Int -> [Int] -> Int64 -> FilePath -> Script RareAlleleHistogram
loadHistogram indices minAf maxAf conditionOn nrCalledSites path = do
    hist <- readHistogram path
    let f = if nrCalledSites > 0 then setNrCalledSites nrCalledSites else return
    tryRight $ (f <=< filterConditionOn conditionOn <=< filterGlobalMinAf minAf <=< filterMaxAf True maxAf <=<
                      reduceIndices indices <=< return) hist

filterConditionOn :: [Int] -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterConditionOn indices hist =
    if (null indices) then return hist else do
        let newBody = Map.filterWithKey conditionPatternOn (raCounts hist)
        return $ hist {raCounts = newBody, raConditionOn = indices}
  where
    conditionPatternOn Higher _ = False
    conditionPatternOn (Pattern pat) _ = all (\i -> pat !! i > 0) indices

setNrCalledSites :: Int64 -> RareAlleleHistogram -> Either String RareAlleleHistogram
setNrCalledSites nrCalledSites hist = do
    let zeroKey = Pattern $ replicate (length $ raNVec hist) 0
        sum_ = Map.foldr (+) 0 $ raCounts hist
        add_ = nrCalledSites - sum_
    when (add_ < 0) $ Left "Illegal nrCalledSites" 
    let newHistBody = Map.insertWith (+) zeroKey add_ (raCounts hist)
    return $ hist {raCounts = newHistBody}

filterMaxAf :: Bool -> Int -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterMaxAf global maxAf' hist = do
    let maxAf = if maxAf' == 0 then raMaxAf hist else maxAf'
    when (maxAf > raMaxAf hist || maxAf < raMinAf hist) $ Left "illegal maxAF"
    when (not global && raGlobalMax hist) $ Left "cannot make maximum local"
    if maxAf == raMaxAf hist && global == raGlobalMax hist then return hist else do
        let newBody = Map.mapKeysWith (+) (prunePatternFreq global maxAf) (raCounts hist)
        return $ hist {raMaxAf = maxAf, raCounts = newBody, raGlobalMax = global}

filterGlobalMinAf :: Int -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterGlobalMinAf minAf hist = do
    when (minAf > raMaxAf hist || minAf < raMinAf hist) $ Left "illegal minAf"
    if minAf == raMinAf hist then return hist else do
        let newBody = Map.mapKeysWith (+) (prunePatternMinTotalFreq minAf) (raCounts hist)
        return $ hist {raCounts = newBody, raMinAf = minAf}

reduceIndices :: [Int] -> RareAlleleHistogram -> Either String RareAlleleHistogram
reduceIndices indices hist =
    if null indices || indices == [0..(length $ raNVec hist)] then return hist else do
        when (raGlobalMax hist && (length (raNVec hist) < length indices)) $ Left "Histogram cannot have global maxAF for this operation"
        when (not . null . raConditionOn $ hist) $ Left "Histogram cannot have conditioning for this operation"
        let newNvec = selectFromList (raNVec hist) indices
            newBody = Map.mapKeysWith (+) (prunePatternIndices indices) (raCounts hist)
        return $ hist {raNVec = newNvec, raCounts = newBody}

combineIndices :: [Int] -> RareAlleleHistogram -> Either String RareAlleleHistogram
combineIndices indices hist = 
    if null indices then
        return hist
    else do
        when (not . null . raConditionOn $ hist) $ Left "Histogram cannot have conditioning for combining indices"
        let newNvec = combineInPattern indices (raNVec hist)
            newBody = Map.mapKeysWith (+) transformPattern (raCounts hist)
        return $ hist {raNVec = newNvec, raCounts = newBody}
  where
    transformPattern Higher = Higher
    transformPattern (Pattern pattern) =
        let newPattern = combineInPattern indices pattern 
            maxAf = raMaxAf hist
            tooHigh = if raGlobalMax hist then
                          sum newPattern > raMaxAf hist
                      else 
                          any (>maxAf) newPattern
        in  if tooHigh then Higher else Pattern newPattern

combineInPattern :: [Int] -> [Int] -> [Int]
combineInPattern indices pattern =
    let newCount = sum $ map (pattern!!) indices
        restPattern = removeFromList pattern indices
    in  insertIntoList restPattern (head indices) newCount

prunePatternIndices :: [Int] -> SitePattern -> SitePattern
prunePatternIndices indices (Pattern pattern) = Pattern $ selectFromList pattern indices
prunePatternIndices _ Higher = Higher

prunePatternFreq :: Bool -> Int -> SitePattern -> SitePattern
prunePatternFreq global maxM (Pattern pattern) =
    if (global && sum pattern > maxM) || (not global && any (>maxM) pattern) then
        Higher
    else
        Pattern pattern
prunePatternFreq _ _ Higher = Higher

prunePatternMinTotalFreq :: Int -> SitePattern -> SitePattern
prunePatternMinTotalFreq minM (Pattern pattern) = if sum pattern < minM then Higher else Pattern pattern
prunePatternMinTotalFreq _ Higher = Higher

selectFromList :: [a] -> [Int] -> [a]
selectFromList l [] = l
selectFromList l i = map (l!!) i

insertIntoList :: [a] -> Int -> a -> [a]
insertIntoList l i el =
    let (l1,l2) = splitAt i l in l1 ++ [el] ++ l2

removeFromList :: [a] -> [Int] -> [a]
removeFromList l indices =
    [el | (el, i) <- zip l [0..],  i `notElem` indices]

