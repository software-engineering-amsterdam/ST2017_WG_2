module Ex5 (Rel, symClos) where

import Data.List

type Rel a = [(a, a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos (o@(a, b):xs) = nub (o : (b, a) : (symClos xs))

main = do
    print (symClos [(1,2),(2,3),(3,4)])