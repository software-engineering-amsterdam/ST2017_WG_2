-- About 5min

module Lab4Ex5 where
import Data.List

-- symClos [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)]
type Rel a = [(a,a)]
symClos :: Ord a => Rel a -> Rel a

symClos [] = []
symClos ((x, y):xs) = sort (nub ([(x, y), (y, x)] ++ (symClos xs)))

main = do
    print (symClos [(1,2),(2,3),(3,4)])