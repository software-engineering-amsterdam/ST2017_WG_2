module Ex5 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Ex3
import Ex4

type Clause  = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls (Prop p)       = [[p]]
cnf2cls (Neg (Prop p)) = [[(-p)]]
cnf2cls (Dsj f)        = [concat (concat (map cnf2cls f))]
cnf2cls (Cnj f)        = concat (map cnf2cls f)

evalClauses :: Valuation -> Clauses -> Bool
evalClauses valuations clauses = all (\clause -> evalClause valuations clause) clauses

evalClause :: Valuation -> Clause -> Bool
evalClause valuations clause = any (\prop -> evalProp valuations prop) clause

evalProp :: Valuation -> Int -> Bool
evalProp valuations prop = case value of
    Just (_, state) -> validateProp prop state
    Nothing         -> False
    where value = find (\(name, state) -> name == prop) valuations

validateProp :: Int -> Bool -> Bool
validateProp p v
    | p > 0 = v
    | otherwise = not v

main = do
   let valuations = [(1, True), (2, False)]
   let clauses = [[1], [2, 1]]

   print (evalClauses valuations clauses)