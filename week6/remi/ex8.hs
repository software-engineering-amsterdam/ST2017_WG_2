-- About an hour

module Ex8 where

import Data.List
import System.Random
import Lecture6

-- Generate an odd random integer between m and n
oddRandomInt :: Integer -> Integer -> IO Integer
oddRandomInt m n = do
    r <- randomRIO (m, n)
    if r `mod` 2 == 0 then oddRandomInt m n else return r

-- Takes k for the MR primality check and an odd int
-- If the odd int is prime, the int is returned
-- Otherwise, 2 is added to the prime until a prime number is found
closestPrime :: Integer -> Integer -> IO Integer
closestPrime k p = do
        r <- primeMR (fromIntegral k) p
        if r then return p else closestPrime k (p+2)

-- Generate random prime between m and n, using k for the MR primality check
getRandomPrime :: Integer -> Integer -> Integer -> IO Integer
getRandomPrime k m n = do
        x <- oddRandomInt m n
        closestPrime k x

-- Use random 32-bit primes to generate public and private keys
-- Next, encode a message using the public key
-- Then, decode the encoded message using the private key
-- Finally, print the decoded message and check if it is the same
-- as the original.
main = do
    p <- getRandomPrime 1 (2^31) (2^32)
    q <- getRandomPrime 1 (2^31) (2^32)
    let pub = rsaPublic p q
    let prv = rsaPrivate p q
    let msg = 1234567890
    let enc = rsaEncode pub msg
    let dec = rsaDecode prv enc
    print dec
    print (dec == msg)