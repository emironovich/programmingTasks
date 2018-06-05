module Biggest where

import Control.Monad

firstBiggestNeighbour :: Ord a => [a] -> Maybe a
firstBiggestNeighbour (x:y:z:xs) = maybeBiggest x y z `mplus` firstBiggestNeighbour (y:z:xs) where
    maybeBiggest :: Ord a => a -> a -> a -> Maybe a
    maybeBiggest x y z | x < y && y > z = Just y
                       | otherwise = Nothing
firstBiggestNeighbour _ = Nothing