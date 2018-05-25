module Fib where
fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 0 =  let
                helperPlus p q 2 = p + q
                helperPlus p q n = helperPlus q (p + q) (n - 1)
            in helperPlus 0 1 n
            | n < 0 = let
                helperMinus p q (-1) = q - p
                helperMinus p q n = helperMinus (q - p) p (n + 1)
            in helperMinus 0 1 n