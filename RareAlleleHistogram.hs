module RareAlleleHistogram (RareAlleleHistogram(..),
                            SitePattern(..),
                            addHistograms, filterMaxAf, setNrCalledSites,
                            loadHistogram, reduceIndices, combineIndices,
                            filterGlobalMinAf, parseHistogram, showHistogram, simpleReadHistogram) where

import qualified Data.Map.Strict as Map
import Data.List (intercalate, sortBy)
import Data.List.Split (splitOn)
import Control.Monad (liftM, when, (<=<))
import System.Log.Logger (infoM)
import Data.Int (Int64)
import Debug.Trace (trace)
import Control.Error.Script (Script, scriptIO)
import Control.Error.Safe (assertErr, readErr, headErr, atErr, lastErr)
import Control.Monad.Trans.Either (hoistEither)

data RareAlleleHistogram = RareAlleleHistogram {
    raNVec :: [Int],
    raMinAf :: Int,
    raMaxAf :: Int,
    raGlobalMax :: Bool,
    raCounts :: Map.Map SitePattern Int64
}

showHistogram :: RareAlleleHistogram -> Either String String
showHistogram hist = do
    assertErr "cannot only print histogram with minAf=0 due to format-legacy" $ raMinAf hist == 0
    let head1 = "N=" ++ commaSep (raNVec hist)
        head2 = "MAX_M=" ++ show (raMaxAf hist)
        head3 = "GLOBAL_MAX=" ++ show (raGlobalMax hist)
        sorted = sortBy (\(_, v1) (_, v2)  -> compare v2 v1) $ Map.toList (raCounts hist)
        body = [show k ++ " " ++ show v | (k, v) <- sorted]
    return $ unlines (head1:head2:head3:body)

simpleReadHistogram :: String -> RareAlleleHistogram
simpleReadHistogram s =
    let lines_ = lines s
        nVec = map read . splitOn "," $ drop 2 (head lines_)
        maxM = read $ drop 6 (lines_ !! 1)
        maxType = read $ drop 11 (lines_ !! 2)
        body = map readBodyLine (filter (/="") $ drop 3 lines_)
    in  RareAlleleHistogram nVec 0 maxM maxType (Map.fromList body)
    where
        readBodyLine :: String -> (SitePattern, Int64)
        readBodyLine line =
            let [patS, valS] = words line
            in  (read patS, read valS)

parseHistogram :: String -> Either String RareAlleleHistogram
parseHistogram s = do
    let lines_ = lines s
    nVecLine <- headErr "empty parse" lines_
    nVec <- mapM (readErr $ "error in line 1" ++ nVecLine) $ splitOn "," $ drop 2 nVecLine
    maxMLine <- atErr "file too short" lines_ 1
    maxM <- readErr "parse error in line 2" $ drop 6 maxMLine
    maxTypeLine <- atErr "file too short" lines_ 2
    maxType <- readErr "parse error in line 3" $ drop 11 maxTypeLine
    body <- mapM readBodyLine (filter (/="") $ drop 3 lines_)
    -- let body = map readBodyLine (filter (/="") $ drop 3 lines_)
    return $ RareAlleleHistogram nVec 0 maxM maxType (Map.fromList body)
    where
        readBodyLine :: String -> Either String (SitePattern, Int64)
        readBodyLine line = do
            let fields = words line
            pat <- readErr "parse error" <=< headErr "parse error" $ fields
            val <- readErr "parse error" <=< lastErr "parse error" $ fields
            return (pat, val)
        -- readBodyLine :: String -> (SitePattern, Int64)
        -- readBodyLine line =
        --     let [patS, valS] = words line
        --     in  (read patS, read valS)

commaSep :: Show a => [a] -> String
commaSep = intercalate "," . map show

addHistograms :: RareAlleleHistogram -> RareAlleleHistogram -> Either String RareAlleleHistogram
addHistograms hist1 hist2 = do
    when (raNVec hist1 /= raNVec hist2 || raMaxAf hist1 /= raMaxAf hist2 ||
                      raGlobalMax hist1 /= raGlobalMax hist2) $ Left "histograms not compatible"
    return $ hist1 {raCounts = Map.unionWith (+) (raCounts hist1) (raCounts hist2)}
        
data SitePattern = Pattern [Int] | Higher deriving (Eq, Ord)
instance Show SitePattern where
    show (Pattern nVec) = commaSep nVec
    show Higher = "HIGHER"

instance Read SitePattern where
    readsPrec _ s =
        let val = if s == "HIGHER" then Higher else Pattern $ map read . splitOn "," $ s
        in  [(val, "")]

loadHistogram :: [Int] -> Int -> Int -> Int64 -> FilePath -> Script RareAlleleHistogram
loadHistogram indices minAf maxAf nrCalledSites path = do
    s <- scriptIO $ readFile path
    hist <- hoistEither $ parseHistogram s
    let f = if nrCalledSites > 0 then setNrCalledSites nrCalledSites else return
    hoistEither $ (f <=< filterGlobalMinAf minAf <=< filterMaxAf True maxAf <=< reduceIndices indices <=< return) hist

setNrCalledSites :: Int64 -> RareAlleleHistogram -> Either String RareAlleleHistogram
setNrCalledSites nrCalledSites hist = do
    let zeroKey = Pattern $ replicate (length $ raNVec hist) 0
        sum_ = Map.foldr (+) 0 $ raCounts hist
        add_ = nrCalledSites - sum_
    when (add_ < 0) $ Left "Illegal nrCalledSites" 
    let newHistBody = Map.insertWith (+) zeroKey add_ (raCounts hist)
    return $ hist {raCounts = newHistBody}

filterMaxAf :: Bool -> Int -> RareAlleleHistogram -> Either String RareAlleleHistogram
filterMaxAf global maxAf hist = do
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
        let newNvec = selectFromList (raNVec hist) indices
            newBody = Map.mapKeysWith (+) (prunePatternIndices indices) (raCounts hist)
        return $ hist {raNVec = newNvec, raCounts = newBody}

combineIndices :: [Int] -> RareAlleleHistogram -> RareAlleleHistogram
combineIndices indices hist = 
    if null indices then
        hist
    else
        let newNvec = combineInPattern indices (raNVec hist)
            newBody = Map.mapKeysWith (+) transformPattern (raCounts hist)
        in  hist {raNVec = newNvec, raCounts = newBody}
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

