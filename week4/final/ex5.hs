-- About 5min
-- We chose this solution because it the best implementation of our group.

-- The symmetric closure is the ordered list of the original
-- relation with all its inversed pairs added.

module Ex5 where
import Data.List

type Rel a = [(a,a)]
symClos :: Ord a => Rel a -> Rel a

symClos [] = []
symClos ((x, y):xs) = sort (nub ([(x, y), (y, x)] ++ (symClos xs)))

main5 :: IO ()
main5 = do
    print (symClos [(1,2),(2,3),(3,4)])