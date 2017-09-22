-- Started at 11:50

module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Ex3

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

main5 = do
    let form = head (parse "*(8 4 (1<=>-5))")
    let valuations = [(8, True), (4, True), (5, False), (1, True)]
    let clauses = [[8],[4],[-5,-1],[1,5]]
    print (evl2 valuations clauses)