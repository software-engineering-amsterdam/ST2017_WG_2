-- About 2.5 hours

module Ex3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- From exercise 1
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

equiv :: Form -> Form -> Bool
equiv f g = entails f g && entails g f

-- First transform form into arrow free form
-- Next, transform arrow free form into negation normal form
-- Finally, simplify
toCNF :: [Form] -> [Form]
toCNF f = map distribute sf
    where af = map arrowfree f
          nf = map nnf af
          sf = map simplify nf

-- Distribute ORs inwards over ANDs
distribute :: Form -> Form
distribute (Prop x) = Prop x
distribute (Dsj [x, Cnj[y, z]]) = Cnj [Dsj [x, y], Dsj [x, z]]
distribute (Dsj [Cnj[y, z], x]) = Cnj [Dsj [y, x], Dsj [z, x]]
distribute x = x

-- Simplify tautology to +(true, false)
-- Simplify contradiction to *(true, false)
-- Simplify conjunction between equivalent expressions to one of the expressions
-- Simplify disjunction between equivalent expressions to one of the expressions
simplify :: Form -> Form
simplify (Prop x) = Prop x
simplify fs
   | tautology fs = Dsj [Prop propName, Neg (Prop propName)]
   | contradiction fs = Cnj [Prop propName, Neg (Prop propName)]
   where propName = head (propNames fs)
simplify (Cnj [x, y])
    | equiv x y = simplify x
    | otherwise = Cnj [simplify x, simplify y]
simplify (Dsj [x, y])
    | equiv x y = simplify x
    | otherwise = Dsj [simplify x, simplify y]
simplify x = x

-- main = do
--     -- print (toCNF (parse "-+(1 2)"))
--     -- print (toCNF (parse "+(1 *(1 2))"))
--     print (toCNF (parse "((1 ==> 2) <=> (-2 ==> -1))"))
--     print (toCNF (parse "(1 ==> -+(2 -3))"))
--     print (toCNF (parse "+(-(1 ==> 2) (3 ==> 1))"))
--     print (toCNF (parse "+(*(1 2) 3)"))