module DigitSum where
digitSum :: Integer -> Integer
digitSum x | x >= 0 = let  
                helper 0 s = s
                helper y s = helper (y `div` 10) (s + y `mod` 10)
             in helper x 0
           | otherwise = digitSum (-x)