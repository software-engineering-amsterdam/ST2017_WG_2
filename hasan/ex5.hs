-- About 45min

-- Do you have to test that your answer is correct? How could this be checked?
--		There is no known mathematical formula that immediately tells us what the answer is.
--		However, we can check by hand if the answer is correct by summing 101 prime numbers in a sliding window and seeing which smallest 101 primes are summing to a prime as well.

-- Import libraries
import Test.QuickCheck
import Data.List

-- Check if a number is a prime by making sure there is no number between 2 and n-1 that divides it and has a remainder of 0
isPrime :: Int -> Bool
isPrime n = if n == 1 then False else all (\t -> rem n t /= 0) [2..(n-1)]

-- Append numbers between 0 and n to the list if it's a prime
getPrimes :: Int -> [Int]
getPrimes n = if n < 2 then [] else if isPrime n then getPrimes (n-1) ++ [n] else getPrimes (n-1)

-- Find the first 5 numbers in the list of primes who's sum is also a prime
findPrimeOf101Primes :: [Int] -> Int
findPrimeOf101Primes n = if all isPrime (take 101 n) && isPrime (sum (take 101 n)) then sum(take 101 n) else findPrimeOf101Primes (tail n)

-- Get all primes between 0 and 101 and then find the first 5 primes that sum to a prime
primesTemp = getPrimes 10000
main = print (findPrimeOf101Primes primesTemp)