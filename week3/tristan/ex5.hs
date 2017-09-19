module Ex5 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Ex4

type Clause  = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
