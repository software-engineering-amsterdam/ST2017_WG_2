-- About 5min

-- Prime functions from lecture #1
prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where xs = takeWhile (\y -> y^2 <= n) primes
primes :: [Integer]
primes = 2 : filter prime [3..2000000]

main = do
	-- Takes a few seconds
	print (sum primes)