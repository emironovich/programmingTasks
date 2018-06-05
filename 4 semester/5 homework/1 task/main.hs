module Main where

main = do
    putStrLn "Enter the number"
    nStr <- getLine
    showPartition $ partition (read nStr) (read nStr)
    
partition :: Int -> Int -> [[Int]]
partition n k   | 0 < k && k <= n  = partition n (k - 1)  ++ (map (k:) (partition (n - k) k))
                 | k > n            = partition n n
                 | n == 0 && k == 0 = [[]]
                 |otherwise         = []
                 
showPartition :: [[Int]] -> IO()
showPartition ((x:[]):ps) = do
        putStrLn $ show x
        showPartition ps
showPartition ((x:xs):ps) = do
        putStr $ show x ++ "+"
        showPartition (xs:ps)
showPartition [] = return()