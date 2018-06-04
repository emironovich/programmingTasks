module Product where

makeProductList n = (take n [1..]) >>= (\x ->
               (take n [1..]) >>= (\y ->
               return (x * y)))