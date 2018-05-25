module Reversing where
myReverse :: [a] -> [a]
myReverse = rev [] where
    rev xs [] = xs
    rev xs (y : ys) = rev (y : xs) ys