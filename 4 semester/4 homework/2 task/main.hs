module Main where

import Data.Char

main = myloop []

myloop :: [Int] -> IO ()
myloop lst = do
    putStrLn "Enter command number"
    putStrLn "0 - exit"
    putStrLn "1 - add value to sorted list"
    putStrLn "2 - remove value from list"
    putStrLn "3 - print list"
    command <- getLine
    case command of
        "0" -> do
            return ()
        "1" -> do
            putStrLn "Enter the value you want to add"
            valueStr <- getLine
            myloop $ addValue lst (read valueStr)
        "2" -> do 
            putStrLn "Enter the value you want to remove"
            valueStr <- getLine
            myloop $ removeValue lst (read valueStr)
        "3" -> do 
            print lst
            myloop lst
            

addValue :: [Int] -> Int -> [Int]        
addValue (x:xs) v | v < x = v:x:xs
                  | otherwise = x : (addValue xs v)
addValue [] v = [v]

removeValue :: [Int] -> Int -> [Int] 
removeValue (x:xs) v | x > v =  x:xs
                     | x < v = x : (removeValue xs v)
                     | otherwise = removeValue xs v
removeValue [] v = []