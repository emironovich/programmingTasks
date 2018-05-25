module Twos where
twoPwrLst :: Int -> [Integer]
twoPwrLst n | n >= 0 = take n $ [ 2^x | x <- [0..] ]
            | otherwise = error "arg must be >= 0"