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

main5 = any2cls (head (parse ("*(8 4 (1<=>-5))")))