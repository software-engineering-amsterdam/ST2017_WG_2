-- Started at 11:50

module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Ex3

type Clause  = [Int]
type Clauses = [Clause]

generateClause :: Form -> Clause
generateClause (Prop x) = [x]
generateClause (Neg (Prop x)) = [-x]
generateClause (Dsj [x, y]) = generateClause x ++ generateClause y
generateClause x = []

cnf2cls :: Form -> Clauses
cnf2cls (Cnj [x, y]) = cnf2cls x ++ cnf2cls y
cnf2cls x = [generateClause x]

any2cls :: Form -> Clauses
any2cls f = cnf2cls (cnf f)

main5 = any2cls (head (parse ("*(8 4 (1<=>-5))")))