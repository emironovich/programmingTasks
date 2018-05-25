module Brackets where
bracketCheck :: [Char] -> Bool
bracketCheck xs = helper [] xs where
                 helper bs ('(' : xs) = helper ('(':bs) xs
                 helper ('(' : bs) (')' : xs) = helper bs xs
                 helper _ (')' : xs) = False
                 helper bs ('{' : xs) = helper ('{':bs) xs
                 helper ('{' : bs) ('}' : xs) = helper bs xs
                 helper _ ('}' : xs) = False
                 helper bs ('[' : xs) = helper ('[':bs) xs
                 helper ('[' : bs) (']' : xs) = helper bs xs
                 helper _ (']' : xs) = False
                 helper bs (x:xs) = helper bs xs
                 helper [] [] = True
                 helper _ [] = False
                 