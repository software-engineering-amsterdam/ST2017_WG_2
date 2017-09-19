module Lab3Ex5 where

import Lab3Ex3
import Lecture3
import Data.List

type Clause  = [Int]
type Clauses = [Clause]

cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg x) = map (\y -> map (\z -> -z) y) (cnf2cls x)
cnf2cls (Dsj x) = [concat (concat (map cnf2cls x))]
cnf2cls (Cnj x) = concat (map cnf2cls x)

evl2 :: Valuation -> Clauses -> Bool
evl2 valuations clauses = all (\clause -> any (\clauseVariable -> if clauseVariable >= 0 then elem (clauseVariable, True) valuations else elem (-clauseVariable, False) valuations) clause) clauses

mainLab3Ex5 = do
    let form = head (parse "*(8 4 (1<=>-5))")
    let valuations = [(8, True), (4, True), (5, False), (1, False)]
    let clauses = [[8],[4],[-5,-1],[1,5]]
    --let clauses = [[8], [4], [-5]]
    --print (cnf2cls (cnf form))
    print (evl2 valuations clauses)