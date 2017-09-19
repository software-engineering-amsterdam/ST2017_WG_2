-- ID: 11408227
-- Name: Vincent Jong
-- Time: 13:00 -  (Programming), (Testing), (Answering)

module Ex5 where

import Data.List
import Test.QuickCheck
import System.Random
import Lecture3
import Ex1

type Clause = [Int]
type Clauses = [Clause]

s = Prop 4

testForm1 = Neg (p)
testForm2 = Cnj [Neg p, q]
testForm3 = Dsj [p, q]
testForm4 = Cnj [Dsj [p, q], Dsj [p, r]]
testForm5 = Cnj [p, Dsj [q, r], Dsj [q, s]]

form2Clause :: Form -> Clause
form2Clause (Prop x) = [x]
form2Clause (Neg (Prop x)) = [-x]

forms2Clause :: [Form] -> Clause
forms2Clause [] = []
forms2Clause (x:xs) = form2Clause x ++ forms2Clause xs

cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [(form2Clause (Prop x))]
cnf2cls (Neg (Prop x)) = [(form2Clause (Neg (Prop x)))]
cnf2cls (Dsj list) = [forms2Clause list]
cnf2cls (Cnj list) = map (\x -> form2Clause x) list