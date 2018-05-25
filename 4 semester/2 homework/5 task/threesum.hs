module ThreeSum where
threeSumLst xs ys zs = take m $ zipWith (+) (xs++[0,0..]) (zipWith (+) (ys++[0,0..]) (zs++[0,0..])) where
                        m = max (length xs) (max (length ys) (length zs))