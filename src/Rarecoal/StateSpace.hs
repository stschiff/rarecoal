module Rarecoal.StateSpace (JointState, JointStateSpace(..), makeJointStateSpace, genericStateToId, genericNrStates, genericIdToState, genericX1Up, genericX1, getNonZeroStates) where

import Control.Exception.Base (assert)
import Control.Monad (foldM)
import Data.List (nub)
import Data.MemoCombinators (arrayRange)
import Debug.Trace (trace)
import qualified Data.Vector.Unboxed as V

type JointState = V.Vector Int

data JointStateSpace = JointStateSpace  {
    _jsStateToId :: JointState -> Int,
    _jsIdToState :: Int -> JointState,
    _jsX1up :: Int -> [Maybe Int],
    _jsX1 :: Int -> Int,
    _jsNrPop :: Int,
    _jsMaxAf :: Int,
    _jsNrStates :: Int
}

makeJointStateSpace :: Int -> Int -> JointStateSpace
makeJointStateSpace nrPop maxAf =
    let stateToId = genericStateToId maxAf
        idToState = genericIdToState maxAf nrPop
        x1up xId = 
            let states = (genericX1Up . idToState) xId
            in  [if V.all (<=maxAf) s then Just (stateToId s) else Nothing | s <- states]
        x1 = stateToId . genericX1 nrPop
        nrStates = genericNrStates maxAf nrPop
        idToStateMemo = arrayRange (0, nrStates - 1) idToState
        x1upMemo = arrayRange (0, nrStates - 1) x1up
        x1Memo = arrayRange (0, nrStates - 1) x1
    in  JointStateSpace stateToId idToStateMemo x1upMemo x1Memo nrPop maxAf nrStates

genericStateToId :: Int -> JointState -> Int
genericStateToId maxAf state = ass $ V.ifoldl (\v i x -> v + x * (maxAf + 1) ^ i) 0 state
  where
    ass = assert (V.all (<=maxAf) state)

genericNrStates :: Int -> Int -> Int
genericNrStates maxAf nrPop = (maxAf + 1) ^ nrPop

genericIdToState :: Int -> Int -> Int -> JointState
genericIdToState maxAf nrPop id_ = ass $ V.fromList (take nrPop (go id_))
  where
    go x = x `mod` (maxAf + 1) : go (x `div` (maxAf + 1))
    ass = assert (id_ < nrStates)
    nrStates = genericNrStates maxAf nrPop

genericX1Up :: JointState -> [JointState]
genericX1Up x = [x V.// [(k, x V.! k + 1)] | k <- [0..V.length x - 1]]

genericX1 :: Int -> Int -> JointState
genericX1 n k = V.replicate n 0 V.// [(k, 1)]

getNonZeroStates :: JointStateSpace -> [Int] -> [Int]
getNonZeroStates jointStateSpace nonZeroStates =
    let states = map (_jsIdToState jointStateSpace) nonZeroStates
        nrPop = _jsNrPop jointStateSpace
        maxAf = _jsMaxAf jointStateSpace
        maxMVec = V.fromList . map maximum $ [map (V.!i) states | i <- [0 .. nrPop - 1]]
        allStates = filter (\v -> V.sum v > 0 && V.sum v <= maxAf) $ expandPattern maxMVec
        allStateIds = map (_jsStateToId jointStateSpace) allStates
    in  nub allStateIds
  where
    expandPattern :: JointState -> [JointState]
    expandPattern maxMVec =
        let k = V.length maxMVec
        in  foldM go maxMVec [0..k-1]
      where
        go vec_ i =
            let maxVal = vec_ V.! i
            in if maxVal == 0 then [vec_] else [vec_ V.// [(i, val)] | val <- [0..maxVal]]
