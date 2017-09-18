-- This took ~30 minutes

module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

contradiction :: Form -> Bool
contradiction form = all (\ v -> not (evl v form)) (allVals form)

tautology :: Form -> Bool
tautology form = all (\ v -> evl v form) (allVals form)

entails :: Form -> Form -> Bool
entails left right = tautology (Impl left right)

equiv :: Form -> Form -> Bool
equiv left right = entails left right && entails right left

main = do
    let form1 = head (parse "((1 ==> 2) <=> (-2 ==> -1))")
    let form2 = head (parse "+(1 -1)")

    print (entails form1 form2 == True)
    print (equiv form1 form2 == True)