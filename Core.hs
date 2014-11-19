module Core (defaultTimes, getProb, ModelSpec(..), Join) where

import Math.Combinatorics.Exact.Binomial (choose)
import Control.Monad.State (State, get, put, execState)
import Data.List (sortBy)
import Debug.Trace (trace)

type Join = (Double, Int, Int, Double) -- t, k, l, N

data ModelSpec = ModelSpec {
    msTimeSteps :: [Double],
    msPopSize :: [Double],
    msJoins :: [Join],
    msTheta :: Double
}

data CoalState = CoalState {
    csA :: [Double],
    csB :: [[Double]],
    csD :: Double
} deriving (Show)

data ModelCoalState = ModelCoalState {
    mcsT :: Double,
    mcsJoins :: [Join],
    mcsPopSize :: [Double],
    mcsCoalState :: CoalState
} deriving (Show)

getProb :: ModelSpec -> [Int] -> [Int] -> Either String Double
getProb (ModelSpec timeSteps popSize joins theta) nVec config = do
    let sortedJoins = sortBy (\(t1,_,_,_) (t2,_,_,_) -> t1 `compare` t2) joins
        popSize' = if length popSize > 0 then popSize else replicate (length nVec) 1.0
        initState = ModelCoalState 0.0 sortedJoins popSize' (makeInitCoalState nVec config)
        finalState = mcsCoalState $ execState (mapM_ singleStep timeSteps) initState
    checkLengths nVec config popSize'
    checkPopSizes popSize sortedJoins
    checkJoins sortedJoins (length nVec)
    return $ csD finalState * theta * fromIntegral (product $ zipWith choose nVec config)
  where
    checkLengths n m l =
        if length n /= length m || length n /= length l
            then Left "nVec, mVec, popsize have incompatible lengths"
            else Right ()
    checkPopSizes popSize joins =
        if any (\p -> p < 0.001 || p > 100.0) (popSize ++ map (\(_,_,_,p) -> p) joins)
            then Left "Population sizes must be between 0.001 and 100.0"
            else Right ()
    checkJoins [] nrPops = Right ()
    checkJoins ((_,k,l,_):rest) nrPops =
        if k == l || any (\(_,k',l',_) -> k' == l || l' == l) rest || k >= nrPops || l >= nrPops
            then Left "Illegal joins"
            else checkJoins rest nrPops
        
makeInitCoalState :: [Int] -> [Int] -> CoalState
makeInitCoalState nVec config = 
    let a = zipWith (\n m -> fromIntegral (n - m)) nVec config
        b = [take m [0, 0..] ++ [1.0] | m <- config]
    in  CoalState a b 0.0

singleStep :: Double -> State ModelCoalState ()
singleStep nextTime = do
    ModelCoalState t joins popSize coalState <- get
    let (t', k, l, n) = if null joins then (1.0/0.0, 0, 0, 0.0) else head joins
        ms = if t' < nextTime
            then let deltaT = t' - t
                     c = updateCoalState deltaT popSize coalState
                     c' = performJoin k l c
                     popSize' = update k n popSize
                     c'' = updateCoalState (nextTime - t') popSize' c'
                 in  ModelCoalState nextTime (tail joins) popSize' c''
        else let deltaT = nextTime - t
                 c = updateCoalState deltaT popSize coalState
             in  ModelCoalState nextTime joins popSize c
    put ms
    -- if nextTime < 0.0002 then trace (show nextTime ++ " " ++ show (mcsCoalState ms)) (put ms) else put ms


updateCoalState :: Double -> [Double] -> CoalState -> CoalState
updateCoalState deltaT popSize coalState =    
    let aNew = updateA deltaT popSize (csA coalState)
        bNew = updateB deltaT popSize (csA coalState) (csB coalState)
        dNew = updateD deltaT (csB coalState) (csD coalState)
    in  CoalState aNew bNew dNew

updateA :: Double -> [Double] -> [Double] -> [Double]
updateA deltaT = zipWith go
  where
    go popSizeK aK = aK * approx_exp(-0.5 * (aK - 1.0) * (1.0 / popSizeK) * deltaT)

updateB :: Double -> [Double] -> [Double] -> [[Double]] -> [[Double]]
updateB deltaT = zipWith3 (updateBk deltaT)

updateBk :: Double -> Double -> Double -> [Double] -> [Double] 
updateBk deltaT popSizeK aK bK = zipWith3 go [0..] bK (tail bK ++ [0.0])
  where
    go i bKi bKi' = bKi * approx_exp(-(i * (i - 1) / 2 * (1.0 / popSizeK) + i * aK * (1.0 / popSizeK)) * deltaT)
                              + bKi' * (1.0 - approx_exp(-(i * (i + 1)) / 2.0 * (1.0 / popSizeK) * deltaT))

updateD :: Double -> [[Double]] -> Double -> Double
updateD deltaT b d =
    let kn = length b in d + deltaT * sum (map go [0..(kn - 1)])
  where
    go k = if length (b!!k) == 1 then 0.0 else product (update k (b!!k!!1) $ map head b)

performJoin :: Int -> Int -> CoalState -> CoalState
performJoin k l state =
    let aVec = csA state
        bVec = csB state
        new_aK = aVec !! k + aVec !! l
        new_a  = update k new_aK aVec
        new_a' = update l 0.0 new_a
        new_m = length (bVec !! k) + length (bVec !! l) - 2
        new_b_k = map (joinProbs (bVec !! k) (bVec !! l)) [0..new_m]
        new_b_l = [1.0]
        new_b = update k new_b_k bVec
        new_b' = update l new_b_l new_b
    in  CoalState new_a' new_b' (csD state)

update :: Int -> a -> [a] -> [a]
update i val vec = let (x, y:ys) = splitAt i vec in x ++ (val:ys)

joinProbs :: [Double] -> [Double] -> Int -> Double
joinProbs bVec1 bVec2 i = sum $ zipWith (*) (take' (i + 1) bVec1) (reverse . take' (i + 1) $ bVec2)
    where take' i vec = take i (vec ++ repeat 0)

defaultTimes = getTimeSteps 20000 400 20.0

approx_exp :: Double -> Double
approx_exp arg = exp arg --if abs arg < 0.05 then 1.0 + arg else exp arg

getTimeSteps :: Int -> Int -> Double -> [Double]
getTimeSteps n0 lingen tMax = map (getTimeStep alpha nr_steps tMax) [1..nr_steps-1]
    where tMin     = 1.0 / (2.0 * fromIntegral n0)
          alpha    = fromIntegral lingen / (2.0 * fromIntegral n0)
          nr_steps = floor $ log(1.0 + tMax / alpha) / log(1.0 + tMin / alpha)

getTimeStep :: Double -> Int -> Double -> Int -> Double
getTimeStep alpha nr_steps tMax i =
    alpha * exp (fromIntegral i / fromIntegral nr_steps * log (1.0 + tMax / alpha)) - alpha
