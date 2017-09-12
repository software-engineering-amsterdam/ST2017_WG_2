-- Start 15min

module Lab2 where
import Data.List
import Data.Eq

-- Exercise says we can assume no duplicates
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] [] = True
isPermutation a b = length (filter (\x -> not (elem x a)) b) == 0

main = do
	print (isPermutation [1] [1])
	print (isPermutation [1, 2, 3] [1, 3, 2])
	print (isPermutation [1, 2, 3, 4] [1, 2, 3])
	print (isPermutation [1, 2, 4] [1, 3, 2])
	print (isPermutation [2] [1])