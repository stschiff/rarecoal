module Utils (computeAllConfigs) where

computeAllConfigs nrPop maxFreq = 
   let maxPowerNum = (maxFreq + 1) ^ nrPop
       order = map (digitize (maxFreq + 1) nrPop) [1..maxPowerNum]
   in  filter (\v -> sum v <= maxFreq) order

digitize :: Int -> Int -> Int -> [Int]
digitize base nrDigit num 
    | nrDigit == 1 = [num]
    | otherwise    = let digitBase = base ^ (nrDigit - 1)
                         digit = div num digitBase
                         rest = num - digit * digitBase
                     in  (digit:digitize base (nrDigit - 1) rest)  
