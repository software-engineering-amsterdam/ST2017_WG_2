-- According to the linked source:
-- "Fermat's little theorem states that if p is a prime number, then for any integer b, 
-- the number b p − b is an integer multiple of p"
-- However there are also non-prime numbers that hold this property. 
-- The non-prime numbers that have this property are the carmicheal numbers
-- This means that Fermat's primality check will give an incorrect answer

module Ex5 (carmichael) where
import Lecture6
import Control.Monad

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
    k <- [2..], 
    prime (6*k+1), 
    prime (12*k+1), 
    prime (18*k+1) ]
    

isPrime :: [Integer] -> IO [Bool]
isPrime (c:cc) = do
    res <- primeTestsF 1 c
    if length cc > 0 then do
        res2 <- isPrime cc
        return (res:res2)
    else 
        return [res]

main = do
    let numbers = (take 100 carmichael)
    res <- isPrime numbers
    let fermatPrimeCount = length (filter (\x -> x) res)
    let actualPrimeCount = length (filter prime numbers)
    putStrLn "Fermat's prime count on carmichael's numbers: "
    print fermatPrimeCount
    putStrLn "Actual prime count on carmichael's numbers: "
    print actualPrimeCount