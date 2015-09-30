import Criterion.Main (bgroup, bench, whnf, defaultMain)

main = defaultMain [
    bgroup "getProb" [ bench "5popAf2" $ whnf (getProb) [0,1,0,0,1]
                     , bench "5popAf4" $ whnf (getProb) [0,1,1,0,2]
                     , bench "5popAf6" $ whnf (getProb) [0,1,2,2,1]
                     , bench "5popAf8" $ whnf (getProb) [2,1,1,2,2]
                     ]
]