module Even where

countEven xs = foldr (+) 0 (map (\x -> (x `mod` 2 + 1) `mod` 2) xs)

countEven' xs = foldr (\x y -> (y + 1)) 0 (filter (\x -> x `mod` 2 < 1) xs)

countEven'' xs = foldr (\x y -> ((x `mod` 2 + 1) `mod` 2) + y) 0 xs