-- About 45min

-- How would you test this function, by the way?
--		I would test this function by checking it against a list of primes that is known to be correct.
--		So I would take that list of known primes, remove any prime in the list who has no reversal that is not in the list.
--		Then I would take the maximum value say N, and test my function by making sure the list I have left equals the list of primes my function gives with argument N.


-- Import libraries
import Test.QuickCheck
import Data.List

reversal :: Int -> Int
reversal = read . reverse . show

-- Check if a number is a prime by making sure there is no number between 2 and n-1 that divides it and has a remainder of 0
isPrime :: Int -> Bool
isPrime n = if n == 1 then False else all (\t -> rem n t /= 0) [2..(n-1)]

-- Append numbers between 0 and n to the list if it's a prime and it's reversal is a prime as well
getPrimes :: Int -> [Int]
getPrimes n = if n < 2 then [] else if isPrime n && isPrime (reversal n) then getPrimes (n-1) ++ [n] else getPrimes (n-1)

-- Print all primes between 0 and 10000 which has a reversal that is also a prime
main = print (getPrimes 10000)