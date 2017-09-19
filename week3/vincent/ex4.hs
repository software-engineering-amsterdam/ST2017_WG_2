-- ID: 11408227
-- Name: Vincent Jong
-- Time: 10:40 - 13:00 (Programming), (Testing), (Answering)

module Ex4 where

import Data.List
import Test.QuickCheck
import System.Random
import Lecture3
import Control.Monad
import Ex1
import Ex3

-- Generate a formula 

randDepth :: Gen Int
randDepth = choose (2, 4)

maxDepth = 3

generateProp :: Gen Form
generateProp = do
    x <- choose (0, 5)
    return (Prop x)

generateNeg :: Int -> Gen Form
generateNeg depth = do
    x <- generateFormula (depth - 1)
    return (Neg x)

generateDisj :: Int -> Gen Form
generateDisj depth = do
    d <- randDepth
    x <- vectorOf d (generateFormula (depth - 1))
    return (Dsj x)

generateConj :: Int -> Gen Form
generateConj depth = do
    d <- randDepth
    x <- vectorOf d (generateFormula (depth - 1))
    return (Cnj x)

generateImpl :: Int -> Gen Form
generateImpl depth = do
    let nDepth = depth - 1
    x <- generateFormula (nDepth)
    y <- generateFormula (nDepth)
    return (Impl x y)

generateEquiv :: Int -> Gen Form
generateEquiv depth = do
    let nDepth = depth - 1
    x <- generateFormula (nDepth)
    y <- generateFormula (nDepth)
    return (Equiv x y)

generateFormula :: Int -> Gen Form
generateFormula 0 = generateProp
generateFormula depth = oneof [generateProp, (generateNeg depth), 
                               (generateDisj depth), (generateConj depth),
                               (generateImpl depth), (generateEquiv depth)]

getFormula :: IO Form
getFormula = generate (generateFormula maxDepth)

printTestForms = do
    generatedForm <- getFormula
    print generatedForm

main = do
    quickCheck (forAll (generateFormula maxDepth) (\ form -> equiv form (cnf form)))