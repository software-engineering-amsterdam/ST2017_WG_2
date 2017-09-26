module Ex6 where

import Data.List
import Ex5

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos r = r @@ r

main = do
    print (trClos [(1,2),(2,3),(3,4)])