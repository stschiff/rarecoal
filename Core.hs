module Core (defaultTimes, getProb, ModelSpec(..), Join) where

import Math.Combinatorics.Exact.Binomial (choose)
import Control.Monad.State (State, get, put, execState)
import Data.List (sortBy)
import Debug.Trace (trace)
import qualified Data.Vector.Unboxed as V

(!) = (V.!)
(//) = (V.//)

type Join = (Double, Int, Int, Double) -- t, k, l, N

data ModelSpec = ModelSpec {
    msTimeSteps :: [Double],
    msPopSize :: [Double],
    msJoins :: [Join],
    msTheta :: Double
}

data CoalState = CoalState {
    csA :: V.Vector Double,
    csB :: [V.Vector Double],
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
    let a = V.fromList $ zipWith (\n m -> fromIntegral (n - m)) nVec config
        b = [V.generate (m + 1) (\i -> if i == m then 1.0 else 0.0) | m <- config]
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

updateA :: Double -> [Double] -> V.Vector Double -> V.Vector Double
updateA deltaT popSize a = V.zipWith go (V.fromList popSize) a
  where
    go popSizeK aK = aK * approx_exp(-0.5 * (aK - 1.0) * (1.0 / popSizeK) * deltaT)

updateB :: Double -> [Double] -> V.Vector Double -> [V.Vector Double] -> [V.Vector Double]
updateB deltaT popSize a b = zipWith3 (updateBk deltaT) popSize (V.toList a) b

updateBk :: Double -> Double -> Double -> V.Vector Double -> V.Vector Double
updateBk deltaT popSizeK aK bK =
    let m = (V.length bK - 1)
    in  V.generate (m + 1) go
  where
    go i =
        let bKi = bK!i
            bKi' = if i < (V.length bK - 1) then bK!(i + 1) else 0.0
            i' = fromIntegral i
        in  bKi * approx_exp(-(i' * (i' - 1) / 2.0 * (1.0 / popSizeK) + i' * aK * (1.0 / popSizeK)) * deltaT)
            + bKi' * (1.0 - approx_exp(-(i' * (i' + 1)) / 2.0 * (1.0 / popSizeK) * deltaT))

updateD :: Double -> [V.Vector Double] -> Double -> Double
updateD deltaT b d = d + deltaT * sum (map go [0..(kn - 1)])
  where
    kn   = length b 
    go k = if V.length (b!!k) == 1 then 0.0 else product $ zipWith (\l bl -> bl!(if l /= k then 0 else 1)) [0..] b

performJoin :: Int -> Int -> CoalState -> CoalState
performJoin k l state =
    let aVec = csA state
        bVec = csB state
        new_aK = aVec!k + aVec!l
        new_a  = aVec // [(k, new_aK), (l, 0.0)]
        bk = bVec !! k
        bl = bVec !! l
        new_m = V.length bk + V.length bl - 2
        new_b_k = V.generate (new_m + 1) (joinProbs bk bl)
        new_b_l = V.singleton 1.0
        new_b = update k new_b_k bVec
        new_b' = update l new_b_l new_b
    in  CoalState new_a new_b' (csD state)

update :: Int -> a -> [a] -> [a]
update i val vec = let (x, y:ys) = splitAt i vec in x ++ (val:ys)

joinProbs :: V.Vector Double -> V.Vector Double -> Int -> Double
joinProbs bVec1 bVec2 i =
    let m1 = V.length bVec1 - 1
        m2 = V.length bVec2 - 1
    in  sum [bVec1!j * bVec2!(i - j) | j <- [0..i], j <= m1 && (i-j) <= m2]

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
