-- About 1 hour

module Lab4Ex7 where
import Lab4Ex6
import Data.List
import Lab4Ex5
import Control.Monad
import Test.QuickCheck

getNodes :: Ord a => Rel a -> [a]
getNodes n = nub (concat (map (\(x, y) -> [x, y]) n))

hasPath :: Ord a => Rel a -> a -> a -> [a] -> Bool
hasPath n a b tested = any (\(x, y) -> x == a && (y == b || (not (elem y tested) && hasPath n y b ([y] ++ tested)))) n

checkSymClosProperties :: Ord a => Rel a -> Bool
checkSymClosProperties n = 
    sort (nub (trClos n)) == trClos n -- It should be sorted and have no duplicates
    && all (\(x, y) -> (x, y) `elem` (symClos n) && (y, x) `elem` (symClos n)) n -- every tuple and its reverse should be in the symbolic closure

checkTrClosProperties :: Ord a => Rel a -> Bool
checkTrClosProperties n = 
    sort (nub (symClos n)) == symClos n -- It should be sorted and have no duplicates
    && all (\(x, y) -> hasPath n x y []) n -- All tuples (x, y) must have a path from x to y
    
runLab4Ex7 = do
    let x = getNodes [(1, 2), (2,3), (4,5)]
    print x
    let y = checkSymClosProperties [(1, 2), (2,3), (4,5)]
    print y
    let z = checkTrClosProperties [(1, 2), (2,3), (4,5)]
    print z
    let a = symClos [(0, 1)]
    print a
    
tester123 :: Rel Int -> Bool
tester123 n = (checkSymClosProperties nn && checkTrClosProperties nn) where nn = nub n
        
test123 = quickCheck tester123