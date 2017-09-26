module Lab4Ex6 where
import Data.List
import Lab4Ex5


-- Given by assignment
infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = 
  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]
  
  
-- trClos [(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos set1 = do
    let x = nub (set1 ++ (set1 @@ set1))
    if x == set1 then sort set1
    else trClos x


runLab4Ex6 = do
    let a = [(1,2),(2,3),(3,4)]
    let expected = [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]
    let actual = trClos a
    print (actual == expected)