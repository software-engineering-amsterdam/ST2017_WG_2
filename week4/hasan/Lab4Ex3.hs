-- About 1 hour
-- All 

module Lab4Ex3 where
import SetOrd
import Control.Monad
import Test.QuickCheck
import Lab4Ex2
   
intersectSet :: (Ord a) => Set a -> Set a -> Set a
intersectSet (Set []) set2 = emptySet
intersectSet (Set (x:xs)) set2 =
	if inSet x set2 then insertSet x (intersectSet (Set xs) set2)
	else intersectSet (Set xs) set2 
	
differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet (Set []) set2 = set2
differenceSet (Set (x:xs)) set2 = 
	if not (inSet x set2) then insertSet x (differenceSet (Set xs) set2)
	else differenceSet (Set xs) (deleteSet x set2) 
	
runLab4Ex3 = do
	let x = intersectSet (list2set [1, 2, 3, 4]) (list2set [2, 3])
	let y = differenceSet (list2set [1, 2, 3, 4]) (list2set [2, 3])
	print x
	print y
	

set2list :: Set a -> [a]
set2list (Set []) = []  
set2list (Set (x:xs)) = [x] ++ (set2list (Set xs))

checkPropertiesForSet :: Set Int -> Set Int -> Bool
checkPropertiesForSet set1 set2 = 
	subSet set1 set1 -- a set must always be a subset of itself
	&& subSet set1 (unionSet set1 set1) && subSet (unionSet set1 set1) set1 -- the union of itself must be a subset of itself, and vice versa
	&& all (\x -> (inSet x set1) || (inSet x set2)) (set2list (unionSet set1 set2)) -- each element x in the union between a and b must be in a and b
	&& all (\x -> (inSet x set1) /= (inSet x set2)) (set2list (differenceSet set1 set2))   -- each element x in the difference between a and b must only be in of [a, b]
	&& all (\x -> (inSet x set1) && (inSet x set2)) (set2list (intersectSet set1 set2)) -- each element x in the intersection between a and b must be in a and b
	&& all (\x -> not (inSet x (differenceSet set1 set2))) (set2list (intersectSet set1 set2)) -- all elements of the difference must not be in the intersection
	&& all (\x -> not (inSet x (intersectSet set1 set2))) (set2list (differenceSet set1 set2)) -- all elements of the intersection must not be in the difference


test1Helper :: Int -> Bool
test1Helper _ = do
	set1 <- arbitrarySet
	set2 <- arbitrarySet
	res <- checkPropertiesForSet set1 set2
	return res
	
test1 :: Bool
test1 = all test1Helper [0..100]
	

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
	arbitrary = do
		a <- arbitrary
		return (list2set a)
		
tester :: (Set Int) -> (Set Int) -> Bool
tester set1 set2 = checkPropertiesForSet set1 set2

test2 = do
	quickCheck checkPropertiesForSet