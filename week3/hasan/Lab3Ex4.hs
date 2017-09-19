module Lab3Ex4 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Lab3Ex1

arbitraryForm :: Gen Form
arbitraryForm  = do
	depth <- choose (0, 10)
	createForm depth

	
arbitaryProp :: Gen Form
arbitaryProp = do
	a <- choose (1,10) 
	return (Prop a)
	
arbitaryNeg :: Int -> Gen Form
arbitaryNeg depth = do
	a <- createForm (depth-1)
	return (Neg a)
	
arbitaryDsj :: Int -> Gen Form
arbitaryDsj depth = do
	n <- choose (2, 4)
	x <- vectorOf n (createForm (depth-1))
	return (Dsj x)
	
arbitaryCnj :: Int -> Gen Form
arbitaryCnj depth = do
	n <- choose (2, 4)
	x <- vectorOf n (createForm (depth-1))
	return (Cnj x)

arbitaryImpl :: Int -> Gen Form
arbitaryImpl depth = do
	x <- createForm (depth-1)
	y <- createForm (depth-1)
	return (Impl x y)

arbitaryEquiv :: Int -> Gen Form
arbitaryEquiv depth = do
	x <- createForm (depth-1)
	y <- createForm (depth-1)
	return (Equiv x y)

createForm :: Int -> Gen Form
createForm 0 = arbitaryProp
createForm depth = oneof [arbitaryProp, (arbitaryNeg depth), (arbitaryDsj depth), (arbitaryCnj depth), (arbitaryImpl depth), (arbitaryEquiv depth)]


mainLab3Ex4 = do
	form <- generate arbitraryForm
	print (form)