module PointFree where

-- func0 x l = map (\y -> y*x) l
-- func1 x = map (\y -> y*x)
-- func2 x = map (*x)
func = map . (*)