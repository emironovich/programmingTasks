module Tree where

import Data.Tree
import Data.Foldable


preorder :: Tree a -> [a]

preorder t = toList t

--exampleTree = Node 'F' [Node 'B' [Node 'A' [] , Node 'D' [Node 'C' [], Node 'E' []] ], Node 'G' [Node 'I' [Node 'H' []]]]
