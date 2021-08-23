{-# LANGUAGE OverloadedStrings #-}
module RarecoalLib.StateSpace (JointState, JointStateSpace(..), makeJointStateSpace,
    genericStateToId,  genericNrStates,  genericIdToState,
    genericStateToId2, genericNrStates2, genericIdToState2, genericX1Up, genericX1,
    fillUpStateSpace,
    ModelEvent(..), EventType(..), ModelSpec(..), validateModel, CoreFunc,
    getRegularizationPenalty)
where

import           Control.Exception.Base (assert)
import           Control.Monad          (foldM, when)
import           Data.List              (nub, sortBy)
import           Data.MemoCombinators   (arrayRange, integral, wrap)
import qualified Data.Vector.Unboxed    as V
import           RarecoalLib.Utils      (computeAllConfigs)


type JointState = V.Vector Int

data JointStateSpace = JointStateSpace
    { _jsStateToId :: JointState -> Int
    , _jsIdToState :: Int -> JointState
    , _jsX1up      :: Int -> V.Vector Int
    , _jsX1        :: Int -> Int
    , _jsNrPop     :: Int
    , _jsMaxAf     :: Int
    , _jsNrStates  :: Int
    }

data ModelEvent = ModelEvent
    { meTime      :: Double
    , meEventType :: EventType
    }
    deriving (Show, Read)

data EventType = Join Int Int
    | Split Int Int Double
    | SetPopSize Int Double
    | SetFreeze Int Bool
    deriving (Show, Read)

data ModelSpec = ModelSpec
    { mNrPops                :: Int
    , mTimeSteps             :: [Double]
    , mTheta                 :: Double
    , mDiscoveryRates        :: [Double]
    , mPopSizeRegularization :: Double
    , mNoShortcut            :: Bool
    , mEvents                :: [ModelEvent]
    }
    deriving (Show)

type CoreFunc = ModelSpec -> [Int] -> [Int] -> Either String Double

-- makeJointStateSpace :: Int -> Int -> JointStateSpace
-- makeJointStateSpace nrPop maxAf =
--     let stateToId = genericStateToId maxAf
--         idToState = genericIdToState maxAf nrPop
--         x1up xId =
--             let states = (genericX1Up . idToState) xId
--             in  V.fromList [if V.all (<=maxAf) s then (stateToId s) else -1 | s <- states]
--         x1 = stateToId . genericX1 nrPop
--         nrStates = genericNrStates maxAf nrPop
--         (idToStateMemo, x1upMemo, x1Memo) = if nrStates < 20000
--             then
--                 (arrayRange (0, nrStates - 1) idToState,
--                  arrayRange (0, nrStates - 1) x1up,
--                  arrayRange (0, nrStates - 1) x1)
--             else
--                 (integral idToState, integral x1up, integral x1)
--     in  JointStateSpace stateToId idToStateMemo x1upMemo x1Memo nrPop maxAf nrStates

makeJointStateSpace :: Int -> Int -> JointStateSpace
makeJointStateSpace nrPop maxAf =
    let stateToId = genericStateToId2 maxAf nrPop
        idToState = genericIdToState2 maxAf nrPop
        x1up xId =
            let states = (genericX1Up . idToState) xId
            in  V.fromList [if V.sum s <= maxAf then (stateToId s) else -1 | s <- states]
        x1 = stateToId . genericX1 nrPop
        nrStates = genericNrStates2 maxAf nrPop
        stateToIdMemo =
            wrap (genericIdToState maxAf nrPop) (genericStateToId maxAf) integral stateToId
        idToStateMemo = arrayRange (0, nrStates - 1) idToState
        x1upMemo = arrayRange (0, nrStates - 1) x1up
        x1Memo = arrayRange (0, nrStates - 1) x1
    in  JointStateSpace stateToIdMemo idToStateMemo x1upMemo x1Memo nrPop maxAf nrStates

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

genericStateToId2 :: Int -> Int -> JointState -> Int
genericStateToId2 maxAf nrPop state =
    let allPatterns = computeAllConfigs maxAf (replicate nrPop maxAf)
        filtered = filter ((==stateV) . snd) . zip [0..] $ allPatterns
    in  if null filtered
        then error $ "cannot find state " ++ show state
        else fst . head $ filtered
  where
    stateV = V.toList state

genericNrStates2 :: Int -> Int -> Int
genericNrStates2 maxAf nrPop = length $ computeAllConfigs maxAf (replicate nrPop maxAf)

genericIdToState2 :: Int -> Int -> Int -> JointState
genericIdToState2 maxAf nrPop = V.fromList . ((computeAllConfigs maxAf (replicate nrPop maxAf)) !!)

genericX1Up :: JointState -> [JointState]
genericX1Up x = [x V.// [(k, x V.! k + 1)] | k <- [0..V.length x - 1]]

genericX1 :: Int -> Int -> JointState
genericX1 n k = V.replicate n 0 V.// [(k, 1)]

-- getNonZeroStates :: JointStateSpace -> [Int] -> [Int]
-- getNonZeroStates jointStateSpace nonZeroStates =
--     let states = map (_jsIdToState jointStateSpace) nonZeroStates
--         nrPop = _jsNrPop jointStateSpace
--         maxAf = _jsMaxAf jointStateSpace
--         maxMVec = V.fromList . map maximum $ [map (V.!i) states | i <- [0 .. nrPop - 1]]
--         allStates = filter (\v -> V.sum v > 0 && V.sum v <= maxAf) $ expandPattern maxMVec
--         allStateIds = map (_jsStateToId jointStateSpace) allStates
--     in  nub allStateIds
--   where
--     expandPattern :: JointState -> [JointState]
--     expandPattern maxMVec =
--         let k = V.length maxMVec
--         in  foldM go maxMVec [0..k-1]
--       where
--         go vec_ i =
--             let maxVal = vec_ V.! i
--             in if maxVal == 0 then [vec_] else [vec_ V.// [(i, val)] | val <- [0..maxVal]]

fillUpStateSpace :: JointStateSpace -> [Int] -> [Int]
fillUpStateSpace jointStateSpace nonZeroStates =
    let states = map (_jsIdToState jointStateSpace) nonZeroStates
        allStates = concatMap expandPattern states
        allStateIds = map (_jsStateToId jointStateSpace) allStates
    in  nub allStateIds
  where
    expandPattern :: JointState -> [JointState]
    expandPattern state =
        let k = V.length state
        in  foldM go state [0..k-1]
      where
        go :: JointState -> Int -> [JointState]
        go vec_ i =
            let maxVal = vec_ V.! i
            in  if maxVal == 0 then [vec_] else [vec_ V.// [(i, val)] | val <- [1..maxVal]]


validateModel :: ModelSpec -> Either String ()
validateModel (ModelSpec _ _ _ dr _ _ events) = do
    when (or [t < 0 | ModelEvent t _ <- events]) $ Left "Negative event times"
    when (or [r <= 0 || r > 1 | r <- dr]) $ Left "illegal discovery Rate"
    let sortedEvents =
            sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) -> time1 `compare` time2) events
    checkEvents sortedEvents
    -- when (reg > 1.0) $ checkRegularization (length dr) reg sortedEvents
  where
    checkEvents [] = Right ()
    checkEvents e@(ModelEvent t (Join k l):rest) = do
        when (k >= length dr || l >= length dr || k < 0 || l < 0) $
            Left ("illegal branch indices in event " ++ show e)
        let illegalEvents = or $ do
                ModelEvent _ ee <- rest
                case ee of
                    Join k' l'      -> return $ k' == l || l' == l
                    Split k' l' _   -> return $ k' == l || l' == l
                    SetPopSize k' _ -> return $ k' == l
                    SetFreeze k' _  -> return $ k' == l
        if k == l || illegalEvents
        then Left ("Illegal join from " ++ show l ++ " to " ++ show k ++ " at time " ++ show t)
        else checkEvents rest
    checkEvents (e@(ModelEvent _ (SetPopSize k p)):rest) = do
        when (k >= length dr || k < 0) $
            Left ("illegal branch indices in event " ++ show e)
        if p <= 0 then Left ("Illegal population size: " ++ show p) else checkEvents rest
    checkEvents (e@(ModelEvent _ (Split l k m)):rest) = do
        when (k >= length dr || l >= length dr || k < 0 || l < 0) $
            Left ("illegal branch indices in event " ++ show e)
        if m < 0.0 || m > 1.0 then Left ("Illegal split rate" ++ show m) else checkEvents rest
    checkEvents (e@(ModelEvent _ (SetFreeze k _)):rest) = do
        when (k >= length dr || k < 0) $
            Left ("illegal branch indices in event " ++ show e)
        checkEvents rest

getRegularizationPenalty :: ModelSpec -> Either String Double
getRegularizationPenalty ms = do
    let initialPopSizes = V.replicate (mNrPops ms) 1.0
    return $ go 0 initialPopSizes sortedEvents
  where
    go res _ [] = res
    go res ps (ModelEvent t (SetPopSize k newP):rest) =
        let newPs = ps V.// [(k, newP)]
            oldP = ps V.! k
            newRes = if t /= 0.0 then res + regFunc oldP newP else res
        in  go newRes newPs rest
    go res ps (ModelEvent _ (Join l k):rest) =
        let fromP = ps V.! k
            toP = ps V.! l
            newRes = res + regFunc fromP toP
        in  go newRes ps rest
    go res ps (_:rest) = go res ps rest
    sortedEvents = sortBy (\(ModelEvent time1 _) (ModelEvent time2 _) -> time1 `compare` time2)
        (mEvents ms)
    reg = mPopSizeRegularization ms
    regFunc oldP newP = if newP > oldP
                            then reg * (newP / oldP - 1.0)^(2::Int)
                            else reg * (oldP / newP - 1.0)^(2::Int)
