module Core (defaultTimes, getProb, Join) where

import Math.Combinatorics.Exact.Binomial (choose)
import Control.Monad.State (State, get, put, execState)
import Data.List (sortBy)

type Join = (Double, Int, Int, Double) -- t, k, l, N

data CoalState = CoalState {
    csA :: [Double],
    csB :: [[Double]],
    csD :: Double
} deriving (Show)

data ModelCoalState = ModelCoalState {
    mcsT :: Double,
    mcsJoins :: [Join],
    mcsLambda :: [Double],
    mcsCoalState :: CoalState
} deriving (Show)

getProb :: [Double] -> [Double] -> [Join] -> Double -> [Int] -> [Int] -> Either String Double
getProb timeSteps lambda joins theta nVec config = do
    let sortedJoins = sortBy (\(t1,_,_,_) (t2,_,_,_) -> t1 `compare` t2) joins
        lambda' = if length lambda > 0 then lambda else replicate (length nVec) 1.0
        initState = ModelCoalState 0.0 sortedJoins lambda' (makeInitCoalState nVec config)
        finalState = mcsCoalState $ execState (mapM_ singleStep timeSteps) initState
    checkLengths nVec config lambda'
    checkPopSizes lambda sortedJoins
    checkJoins sortedJoins (length nVec)
    return $ csD finalState * theta * fromIntegral (product $ zipWith choose nVec config)
  where
    checkLengths n m l =
        if length n /= length m || length n /= length l
            then Left "nVec, mVec, lambda have incompatible lengths"
            else Right ()
    checkPopSizes lambda joins =
        if any (\l -> l < 0.001 || l > 100.0) (lambda ++ map (\(_,_,_,l) -> l) joins)
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
    ModelCoalState t joins lambda coalState <- get
    let (t', k, l, n) = if null joins then (1.0/0.0, 0, 0, 0.0) else head joins
        ms = if t' < nextTime
            then let deltaT = t' - t
                     c = updateCoalState deltaT lambda coalState
                     c' = performJoin k l c
                     lambda' = update k (1.0 / n) lambda
                     c'' = updateCoalState (nextTime - t') lambda' c'
                 in  ModelCoalState nextTime (tail joins) lambda' c''
        else let deltaT = nextTime - t
                 c = updateCoalState deltaT lambda coalState
             in  ModelCoalState nextTime joins lambda c
    put ms

updateCoalState :: Double -> [Double] -> CoalState -> CoalState
updateCoalState deltaT lambda coalState =    
    let aNew = updateA deltaT lambda (csA coalState)
        bNew = updateB deltaT lambda (csA coalState) (csB coalState)
        dNew = updateD deltaT (csB coalState) (csD coalState)
    in  CoalState aNew bNew dNew

updateA :: Double -> [Double] -> [Double] -> [Double]
updateA deltaT = zipWith go where
    go lambdaK aK = aK * exp(-0.5 * (aK - 1) * lambdaK * deltaT)

updateB :: Double -> [Double] -> [Double] -> [[Double]] -> [[Double]]
updateB deltaT = zipWith3 (updateBk deltaT)

updateBk :: Double -> Double -> Double -> [Double] -> [Double] 
updateBk deltaT lambdaK aK bK = zipWith3 go [0..] bK (tail bK ++ [0.0])
  where
    go i bKi bKi' = bKi * exp(-(i * (i - 1) / 2 * lambdaK + i * aK * lambdaK) * deltaT)
                              + bKi' * (1.0 - exp(-(i * (i + 1)) / 2.0 * lambdaK * deltaT))

updateD :: Double -> [[Double]] -> Double -> Double
updateD deltaT b d =
    let kn = length b in  d + deltaT * sum (map go [0..(kn - 1)])
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

defaultTimes = getTimeSteps 20000.0 400 20.0

getTimeSteps :: Double -> Int -> Double -> [Double]
getTimeSteps n0 lingen tMax = map (getTimeStep alpha nr_steps tMax) [1..nr_steps]
    where tMin     = 1.0 / (2.0 * n0)
          alpha    = fromIntegral lingen / (2.0 * n0)
          nr_steps = floor $ log(1.0 + tMax / alpha) / log(1.0 + tMin / alpha)

getTimeStep :: Double -> Int -> Double -> Int -> Double
getTimeStep alpha nr_steps tMax i =
    alpha * exp(fromIntegral i / fromIntegral nr_steps * log(1.0 + tMax / alpha)) - alpha

-- delete :: Int -> [a] -> [a]
-- delete i vec = let (x, y:ys) = splitAt i vec in x ++ ys
