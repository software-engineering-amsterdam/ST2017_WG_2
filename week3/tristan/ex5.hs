module Ex5 where

import Debug.Trace
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
    Just (name, state) -> validateProp prop state
    Nothing            -> False
    where value = find (\(name, state) -> name == (abs prop)) valuations

validateProp :: Int -> Bool -> Bool
validateProp p v
    | p > 0 = v
    | otherwise = not v

main = do
    let valuations = [(8, True), (4, True), (5, False), (1, True)]
    let clauses = [[8],[4],[-5,-1],[1,5]]

    print (evalClauses valuations clauses)