-- Miller-Rabin's primality check says that no carmicheal number is a prime
-- while Fermat's primality check incorrectly said it were prime numbers
-- It is important to state that at k=10 it says none of the first 1000 carmichael numbers
-- are prime. For lower k it says that some are prime, 
-- for example for one run at k=3 it said that 2 numbers out of 1000 were prime
-- When I run MR for 25 times with k=3 and 1000 carmicheal numbers then we get an average of
-- 1.2307693 numbers out of 1000 that are incorrectly classified as prime 

module Ex6a () where
import Ex5
import Lecture6
import Data.List

numbers = (take 1000 carmichael)

average :: [Int] -> Float
average n = fromIntegral (sum n) / fromIntegral (length n)

isPrime :: [Integer] -> IO [Bool]
isPrime (c:cc) = do
    res <- primeMR 3 c
    if length cc > 0 then do
        res2 <- isPrime cc
        return (res:res2)
    else 
        return [res]

getPercentageMr :: [Integer] -> Integer -> IO [Int]
getPercentageMr n = do
    res <- isPrime numbers
    let mrPrimeCount = length (filter (\x -> x) res)
    if n > 0 then do
        res2 <- getPercentageMr (n-1)
        return (mrPrimeCount:res2)
    else return [mrPrimeCount]

main = do
    res <- isPrime numbers
    --let mrPrimeCount = length (filter (\x -> x) res)
    mrPrimeCountMultipleRuns <- getPercentageMr 25
    print mrPrimeCountMultipleRuns
    let mrPrimeCount = average mrPrimeCountMultipleRuns
    let actualPrimeCount = length (filter prime numbers)
    putStrLn "Miller-Rabins's prime count on carmichael's numbers: "
    print mrPrimeCount
    putStrLn "Actual prime count on carmichael's numbers: "
    print actualPrimeCount