import Rarecoal.Core (getProb, defaultTimes, ModelSpec(..), ModelEvent(..), EventType(..))

import Criterion.Main (bgroup, bench, whnf, defaultMain)

main = defaultMain [
    bgroup "getProb" [ bench "5popAf2" $ whnf (get5popProb False) [0,1,0,0,1]
                     , bench "5popAf4" $ whnf (get5popProb False) [0,1,1,0,2]
                     , bench "5popAf6" $ whnf (get5popProb False) [0,1,2,2,1]
                     , bench "5popAf8" $ whnf (get5popProb False) [2,1,1,2,2]
                     , bench "5popAf8noShortcut" $ whnf (get5popProb True) [2,1,1,2,2]
                     ]
    ]

get5popProb :: Bool -> [Int] -> Double
get5popProb noShortCut config = case prob of
    Left err -> -1.0
    Right res -> res
  where
    prob = getProb modelSpec nVec noShortCut config
    modelSpec = ModelSpec defaultTimes 0.001 events
    events = [ ModelEvent 0.0025 (Join 0 1)
             , ModelEvent 0.006 (Join 2 3)
             , ModelEvent 0.0075 (Join 2 4)
             , ModelEvent 0.01 (Join 0 2)
             ]
    nVec = [100, 100, 100, 100, 100]
