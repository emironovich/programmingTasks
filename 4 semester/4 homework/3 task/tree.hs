module Tree where

import Data.Tree
import Data.Foldable


preorder :: Tree a -> [a]
preorder (Node a []) = a:[]
preorder (Node a xs)= a : (foldr (\a b -> (preorder a) ++ b ) [] xs)
    
exampleTree = Node 'F' [Node 'B' [Node 'A' [] , Node 'D' [Node 'C' [], Node 'E' []] ], Node 'G' [Node 'I' [Node 'H' []]]]

