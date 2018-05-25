module Finding where
fstEncounter :: Int -> [Int] -> Int
fstEncounter a xs = helper 1 a xs where
                        helper _ _ [] = -1
                        helper curr a (x:xs) | a == x = curr
                                             | otherwise = helper (curr + 1) a xs