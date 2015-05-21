{-# LANGUAGE OverloadedStrings #-}

module RareAlleleHistogram (RareAlleleHistogram(..),
                            SitePattern(..),
                            addHistograms, filterMaxAf, setNrCalledSites,
                            loadHistogram, reduceIndices, combineIndices,
                            filterGlobalMinAf, parseHistogram, showHistogram) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate, sortBy)
import Control.Monad (when, (<=<))
import Data.Int (Int64)
import Control.Error (Script, scriptIO, justErr, assertErr, readErr, headErr, atErr, lastErr, tryRight)
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Applicative ((<$>))

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
    show (Pattern nVec) = commaSep nVec
    show Higher = "HIGHER"

-- instance Read SitePattern where
--     readsPrec _ s =
--         let val = if s == "HIGHER" then Higher else Pattern $ map read . splitOn "," $ s
--         in  [(val, "")]

commaSep :: Show a => [a] -> String
commaSep = intercalate "," . map show

showHistogram :: RareAlleleHistogram -> Either String B.ByteString
showHistogram hist = do
    assertErr "can only print histogram with minAf=0 due to format-legacy" $ raMinAf hist == 0
    assertErr "can only print histogram with no conditioning due to format-legacy" $ length (raConditionOn hist) == 0
    let head1 = B.concat ["N=", B.pack . commaSep . raNVec $ hist]
        head2 = B.concat ["MAX_M=", B.pack . show . raMaxAf $ hist]
        head3 = B.concat ["GLOBAL_MAX=", B.pack . show . raGlobalMax $ hist]
        body = [B.intercalate " " [B.pack . show $ k, B.pack . show $ v] | (k, v) <- sorted]
    return $ B.unlines (head1:head2:head3:body)
  where
    sorted = sortBy (\(_, v1) (_, v2)  -> compare v2 v1) $ Map.toList (raCounts hist)
    
-- simpleReadHistogram :: String -> RareAlleleHistogram
-- simpleReadHistogram s =
--     let lines_ = lines s
--         nVec = map read . splitOn "," $ drop 2 (head lines_)
--         maxM = read $ drop 6 (lines_ !! 1)
--         maxType = read $ drop 11 (lines_ !! 2)
--         body = map readBodyLine (filter (/="") $ drop 3 lines_)
--     in  RareAlleleHistogram nVec 0 maxM maxType (Map.fromList body)
--     where
--         readBodyLine :: String -> (SitePattern, Int64)
--         readBodyLine line =
--             let [patS, valS] = words line
--             in  (read patS, read valS)

-- parseHistogram :: String -> Either String RareAlleleHistogram
-- parseHistogram s = do
--     let lines_ = lines s
--     nVecLine <- headErr "empty parse" lines_
--     nVec <- mapM (readErr $ "error in line 1" ++ nVecLine) $ splitOn "," $ drop 2 nVecLine
--     maxMLine <- atErr "file too short" lines_ 1
--     maxM <- readErr "parse error in line 2" $ drop 6 maxMLine
--     maxTypeLine <- atErr "file too short" lines_ 2
--     maxType <- readErr "parse error in line 3" $ drop 11 maxTypeLine
--     body <- mapM readBodyLine (filter (/="") $ drop 3 lines_)
--     return $ RareAlleleHistogram nVec 0 maxM maxType [] (Map.fromList body)
--     where
--         readBodyLine :: String -> Either String (SitePattern, Int64)
--         readBodyLine line = do
--             let fields = words line
--             pat <- readErr "parse error" <=< headErr "parse error" $ fields
--             val <- readErr "parse error" <=< lastErr "parse error" $ fields
--             return (pat, val)

parseHistogram :: B.ByteString -> Either String RareAlleleHistogram
parseHistogram s = do
    let lines_ = B.lines s
    nVecLine <- headErr "empty parse" lines_
    nVec <- map fst <$> (justErr "error in line 1" . mapM B.readInt . B.split ',' . B.drop 2 $ nVecLine)
    maxMLine <- atErr "file too short" lines_ 1
    maxM <- fst <$> (justErr "parse error in line 2" . B.readInt . B.drop 6 $ maxMLine)
    maxTypeLine <- atErr "file too short" lines_ 2
    maxType <- readErr "parse error in line 3" . B.unpack . B.drop 11 $ maxTypeLine
    body <- mapM readBodyLine . filter ((>0) . B.length) . drop 3 $ lines_
    return $ RareAlleleHistogram nVec 0 maxM maxType [] (Map.fromList body)
    where
        readBodyLine :: B.ByteString -> Either String (SitePattern, Int64)
        readBodyLine line = do
            let fields = B.words line
            pat <- readPattern <=< headErr "parse error" $ fields
            val <- readErr "parse error" . B.unpack <=< lastErr "parse error" $ fields
            return (pat, val)
        readPattern :: B.ByteString -> Either String SitePattern
        readPattern w =
            if w == "HIGHER" then Right Higher
            else Pattern . map fst <$> (justErr "error parsing pattern" . mapM B.readInt . B.split ',' $ w)

addHistograms :: RareAlleleHistogram -> RareAlleleHistogram -> Either String RareAlleleHistogram
addHistograms hist1 hist2 = do
    when (raNVec hist1 /= raNVec hist2) $ Left "histograms have different NVecs"
    when (raMaxAf hist1 /= raMaxAf hist2) $ Left "histograms have different maxAf"
    when (raGlobalMax hist1 /= raGlobalMax hist2) $ Left "histograms different locality of maximum"
    when (raConditionOn hist1 /= raConditionOn hist2) $ Left "histograms differ in conditioning"
    return $ hist1 {raCounts = Map.unionWith (+) (raCounts hist1) (raCounts hist2)}
        
loadHistogram :: [Int] -> Int -> Int -> [Int] -> Int64 -> FilePath -> Script RareAlleleHistogram
loadHistogram indices minAf maxAf conditionOn nrCalledSites path = do
    s <- scriptIO $ B.readFile path
    hist <- tryRight $ parseHistogram s
    let f = if nrCalledSites > 0 then setNrCalledSites nrCalledSites else return
    tryRight $ (f <=< filterConditionOn conditionOn
                     <=< filterGlobalMinAf minAf
                     <=< filterMaxAf True maxAf
                     <=< reduceIndices indices
                     <=< return) hist

-- filterConditionOn :: [Int] -> RareAlleleHistogram -> Either String RareAlleleHistogram
-- filterConditionOn indices hist =
--     if (null indices) then return hist else do
--         let newBody = Map.mapKeysWith (+) conditionPatternOn (raCounts hist)
--         return $ hist {raCounts = newBody, raConditionOn = indices}
--   where
--     conditionPatternOn Higher = Higher
--     conditionPatternOn (Pattern pat) = if all (\i -> pat !! i > 0) indices then Pattern pat else Higher

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

