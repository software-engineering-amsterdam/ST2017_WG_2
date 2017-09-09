-- ID: 11408227
-- Name: Vincent Jong
-- Time: ~10 min

primes :: [Int]
primes = 2 : filter isPrime [3..2000000] 

isPrime :: Int -> Bool
isPrime n = all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

main = print $ sum primes