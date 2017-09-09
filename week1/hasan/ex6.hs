-- About 45min

-- Import libraries
import Data.List

-- Check if a number is a prime by making sure there is no number between 2 and n-1 that divides it and has a remainder of 0
isPrime :: Int -> Bool
isPrime n = if n == 1 then False else all (\t -> rem n t /= 0) [2..(n-1)]

-- Append numbers between 0 and n to the list if it's a prime
getPrimes :: Int -> [Int]
getPrimes n = if n < 2 then [] else if isPrime n then getPrimes (n-1) ++ [n] else getPrimes (n-1)

-- Endless list of primes starting from given number
primes n = if isPrime (n) then [n] ++ primes (n+1) else primes(n+1)

-- For a given list [p1,p2,...pn] returns p1*p2*...*pn
times :: [Int] -> Int
times n = if length(n) == 0 then 1 else (head n) * (times (tail n))

-- For a given number n, it calculates consecutive primes p1,p2,..,pn and returns p1*p2*...*pn
primesTimes :: Int -> Int
primesTimes n = times (take n (primes 2))

-- Find the first n consecutive primes which if you time each other and add 1 it does not result in a prime
findFirstError :: Int -> [Int]
findFirstError count = if isPrime ((primesTimes count) + 1) == False then (take count (primes 2)) else findFirstError (count+1)

-- Print the result
main = print (findFirstError 1)