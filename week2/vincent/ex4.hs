-- ID: 11408227
-- Name: Vincent Jong
-- Time: 10:15 - 

permList1 = [10, 25, 55, 37, 81, 43, 78]
permList2 = [55, 37, 43, 78, 81, 25, 10]
permList3 = [37, 42, 78, 25, 81, 55, 10]
permListClones1 = [25, 25, 55, 37, 43, 78, 10]
permListClones2 = [25, 55, 37, 43, 78, 10, 25]
permListClones3 = [25, 55, 55, 37, 43, 78, 10]

countElem :: Eq a => a -> [a] -> Int
countElem a [] = 0
countElem a (x:xs) = (if a == x then 1 else 0) + countElem a xs

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && all (\x -> x `elem` ys) xs && all (\y -> y `elem` xs) ys && all (\x -> countElem x xs == countElem x ys) xs

--checkClones :: Bool
--checkClones = isPermutation per ys

--checkSameElements :: Bool
--checkClones xs ys = isPermutation
