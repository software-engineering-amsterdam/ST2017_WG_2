-- Source Mersenne numbers: https://primes.utm.edu/mersenne/
-- Findings:
-- Finding Mersenne numbers take a long time and after running for a
-- few seconds the algorithm is only able to find Mersenne numbers
-- lower or equal to p=31
-- However, the numbers that are found are genuine Mersenne numbers
-- As you can see by comparing the genuine list to the generated list


module Ex6b where
import Lecture6

genuineMersennePrimes = [
    2, 3, 5, 7, 13, 17, 19, 31, 61, 89, 107, 127, 521, 
    607, 1279, 2203, 2281, 3217, 4253, 4423, 9689, 9941, 
    11213, 19937, 21701, 23209, 44497, 86243, 110503, 
    132049, 216091, 756839, 859433, 1257787, 1398269, 
    2976221, 3021377, 6972593, 13466917, 20996011, 
    24036583, 25964951]
    
filterPrime :: [Integer] -> IO [Integer]
filterPrime (c:cc) = do
    res <- primeMR 10 c
    res2 <- primeMR 10 (2^c -1)
    if length cc == 0 then return []
    else if res && res2 then do
        res3 <- filterPrime cc
        return (c:res3)
    else do
        res3 <- filterPrime cc
        return res3
main = do
    primes' <- filterPrime [2..1000]
    print primes'
    let allGenuine = all (\x -> elem x genuineMersennePrimes) primes'
    print "All found numbers are genuine mersenne primes:"
    print allGenuine