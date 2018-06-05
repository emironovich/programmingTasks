module ThreeSum where
-- threeSumLst xs ys zs = take m $ zipWith (+) (xs++[0,0..]) (zipWith (+) (ys++[0,0..]) (zs++[0,0..])) where
                        -- m = max (length xs) (max (length ys) (length zs))

threeSumLst (x:xs) (y:ys) (z:zs) = (x + y + z) : threeSumLst xs ys zs
threeSumLst (x:xs) (y:ys) []     = (x + y) : threeSumLst xs ys []
threeSumLst (x:xs) []     (z:zs) = (x + z) : threeSumLst xs [] zs
threeSumLst []     (y:ys) (z:zs) = (y + z) : threeSumLst [] ys zs
threeSumLst xs     []     []     = xs
threeSumLst []     ys     []     = ys
threeSumLst []     []     zs     = zs