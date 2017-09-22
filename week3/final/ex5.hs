-- About 3 hours

-- Test 1 by checking if every clause has at least one variable that is true

-- Test 2 by generating clauses and by a function created to turn clauses back into forms.
-- The form is turned into clauses and the clauses are turned back into a form
-- and then we check if that form is equal to the original.

module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Ex1
import Ex3
import Ex4

type Clause  = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg x) = map (\y -> map (\z -> -z) y) (cnf2cls x)
cnf2cls (Dsj x) = [concat (concat (map cnf2cls x))]
cnf2cls (Cnj x) = concat (map cnf2cls x)

any2cls :: Form -> Clauses
any2cls f = cnf2cls (cnf f)

-- Check if every clause has at least one variable that is true
evl2 :: Valuation -> Clauses -> Bool
evl2 valuations clauses = all (\clause -> any (\clauseVariable -> elem ((abs clauseVariable), (abs clauseVariable) >= 0) valuations) clause) clauses

-- Clauses back to form, so we can automate testing
cl2form :: Clause -> Form
cl2form x
    | length x == 1 = head (map Prop x)
    | otherwise = Dsj (map Prop x)

cls2form :: Clauses -> Form
cls2form x
    | length x == 1 = cl2form (head x)
    | otherwise = Cnj (map cl2form x)

-- QuickCheck function
testClauses :: Form -> Bool
testClauses x = equiv x (head (parse (show (cls2form (any2cls x)))))

main5 = do
    let form = head (parse "*(8 4 (1<=>-5))")
    let valuations = [(8, True), (4, True), (5, False), (1, True)]
    let clauses = [[8],[4],[-5,-1],[1,5]]
    print (evl2 valuations clauses)

    quickCheck testClauses