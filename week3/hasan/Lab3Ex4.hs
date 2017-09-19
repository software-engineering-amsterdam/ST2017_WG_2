module Lab3Ex4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Lab3Ex1
import Lab3Ex3
import Control.Monad

arbitraryForm :: Gen Form
arbitraryForm = do
    depth <- choose (0, 8)
    createForm depth

createForm :: Int -> Gen Form
createForm 0 = liftM Prop (choose (0, 10))
createForm depth = oneof [
    liftM Neg (createForm depth2), 
    liftM Dsj (vectorOf 2 (createForm depth2)), 
    liftM Cnj (vectorOf 2 (createForm depth2)),
    liftM2 Impl (createForm depth2) (createForm depth2),
    liftM2 Equiv (createForm depth2) (createForm depth2)
    ] where depth2 = depth-1
    
    
checkCNFProperty :: Form -> Bool
checkCNFProperty f = equiv f (cnf f)
checkPropNames f =  all (\x -> elem x originalPropNames) (propNames (cnf f)) where originalPropNames = propNames f

mainLab3Ex4 = do
	quickCheck (forAll arbitraryForm (\f -> checkPropNames f && checkCNFProperty f))