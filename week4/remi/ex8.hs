-- About 15 minutes

-- The symmetric closure of the transitive closure of a relation R
-- and the transitive closure of the symmetric closure of R
-- are not the same.

-- For example, consider R = [(1,2),(2,3)]
-- its symmetric closure is [(1,2),(2,1),(2,3),(3,2)]
-- its transitive closure is [(1,2),(1,3),(2,3)].

-- The transitive closue of the symmetric closure is
-- [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)].
-- The symmetric closure of the transitive closure is
-- [(1,2),(2,1),(1,3),(3,1),(2,3),(3,2)].

module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import Ex5
import Ex6

main = do
    print (trClos (symClos [(1,2),(2,3)]))
    print (symClos (trClos [(1,2),(2,3)]))