-- Time: 10:00 - 11:00 (about 1 hr)

-- With k = 1, the Miller Rabin test find a lot of numbers that aren't actually prime.
-- *Ex6> main
-- [137243534644009,112374872517529,67858397221969,15181505298649,14685655594249,4507445537641,2724933935809,1201586232601,527519713969,21515221081,2301745249,294409]
-- "Nr of integers that are actually prime in that list: 0"
-- *Ex6> main
-- [118974229155289,73103085605161,62303597046289,8544361005001,856666552249,27278026129,9624742921,2301745249]
-- "Nr of integers that are actually prime in that list: 0"
-- *Ex6> main
-- [221568419989801,177548395641481,91968282854641,81159260227849,57060521336809,53269464581929,39782913594409,22027380041449,11004252611041,3267961077889,2920883888089,856666552249,21515221081,2301745249,172947529]
-- "Nr of integers that are actually prime in that list: 0"
-- *Ex6> main
-- [177548395641481,81159260227849,13633039686169,11765530852489,7622722964881,856666552249,216821881,172947529]
-- "Nr of integers that are actually prime in that list: 0"

-- With k = 2, the test becomes much more selective so it find less 'prime' integers. However the numbers that it finds are still not prime.
-- *Ex6> main
-- [77833567590769]
-- "Nr of integers that are actually prime in that list: 0"
-- *Ex6> main
-- [39782913594409,173032371289,216821881]
-- "Nr of integers that are actually prime in that list: 0"
-- *Ex6> main
-- []
-- "Nr of integers that are actually prime in that list: 0"
-- *Ex6> main
-- []
-- "Nr of integers that are actually prime in that list: 0"

-- With k = 3, it already seems to work better in that none of the carmichael numbers are prime
-- *Ex6> main
-- []
-- "Nr of integers that are actually prime in that list: 0"
-- *Ex6> main
-- []
-- "Nr of integers that are actually prime in that list: 0"
-- *Ex6> main
-- []
-- "Nr of integers that are actually prime in that list: 0"
-- *Ex6> main
-- []
-- "Nr of integers that are actually prime in that list: 0"
-- *Ex6> main
-- []
-- "Nr of integers that are actually prime in that list: 0"
-- *Ex6> main
-- []
-- "Nr of integers that are actually prime in that list: 0"

module Ex6 where

import Data.List
import System.Random
import Lecture6
import Ex5
import Ex1

mrComposite' :: Integer -> Integer -> Bool
mrComposite' x n = let
    (r,s) = decomp (n-1)
    fs     = takeWhile (/= 1) 
       (map (\ j -> Ex1.exM x (2^j*s) n)  [0..r])
  in 
    Ex1.exM x s n /= 1 && last fs /= (n-1)

primeMR' :: Int -> Integer -> IO Bool
primeMR' _ 2 = return True
primeMR' 0 _ = return True
primeMR' k n = do 
    a <- randomRIO (2, n-1) :: IO Integer
    if Ex1.exM a (n-1) n /= 1 || mrComposite' a n
    then return False else primeMR' (k-1) n

getPrimes :: Int -> [Integer] -> [Integer] -> IO [Integer]
getPrimes k [] result = return result
getPrimes k (x:xs) result = do
    isPrime <- primeMR' k x
    if isPrime 
        then getPrimes k xs ([x] ++ result)
        else getPrimes k xs result

subCarmichael = take 100 carmichael -- [294409,56052361]

nrOfPrimes :: [Integer] -> Int
nrOfPrimes [] = 0
nrOfPrimes (x:xs) = if prime x then 1 + nrOfPrimes xs else nrOfPrimes xs

main = do
    let k = 3
    res <- getPrimes k subCarmichael []
    print res
    let nr = nrOfPrimes res
    print ("Nr of integers that are actually prime in that list: " ++ (show nr))