module MaxPairSum where 

maxPairSum :: [Int] -> Int
maxPairSum xs = helper (-1)(minBound::Int) 2 xs where
        helper maxPos maxSum curPos (x:y:z:xs) | x + z > maxSum = helper curPos (x + z) (curPos + 1) (y:z:xs)
                                               | otherwise = helper maxPos maxSum (curPos + 1) (y:z:xs)
        helper (-1) _ _ _ = error "The list must have at least three elements"
        helper maxPos _ _ _ = maxPos