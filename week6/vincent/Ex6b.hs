-- Time: 11:15 - 12:15 (about 1 hr)
-- Shoutout to Hasan for finding a list of valid Mercenne primes from: https://primes.utm.edu/mersenne/

module Ex6 where

import Data.List
import System.Random
import Lecture6

calcMerc x = (2 ^ x) - 1

isMercenne :: Integer -> IO Bool
isMercenne x = primeMR 10 (calcMerc x)

getMercenne :: [Integer] -> [Integer] -> IO [Integer]
getMercenne [] res = return res
getMercenne (x:xs) res = do
    mc <- isMercenne x
    if mc && prime x then getMercenne xs (res ++ [x]) else getMercenne xs res

mercenneList x = getMercenne (take x primes) []

-- First check if the elements are in proven mercenne numbers below

validMercenne = [2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521, 607, 1279, 2203, 2281, 3217, 4253, 4423, 9689, 9941, 11213, 19937, 21701, 23209, 44497, 86243, 110503, 132049, 216091, 756839, 859433, 1257787, 1398269, 2976221, 3021377, 6972593, 13466917, 20996011, 24036583, 25964951 ]

checkKnown :: [Integer] -> [Integer] -> Bool
checkKnown xs ys = if length xs <= length ys
    then all (\x -> x `elem` ys) xs
    else False

check1 :: IO Bool
check1 = do
    mList <- mercenneList 300
    return (checkKnown mList validMercenne)

-- Property that Mercenne != Wieferich
calcWief x = 1 `mod` (x*x)

check2 :: Bool
check2 = do
    let mList = take 300 primes
    all (\x -> calcMerc x /= calcWief x) mList

main = do
    print "Checking elements if in valid list"
    stmt1 <- check1
    print stmt1
    print "Checking Mercenne != Wieferich"
    let stmt2 = check2
    print stmt2