-- Time: 12:15 - 13:15 (about 1 hr)

module Ex7 where

import Data.List
import System.Random
import Lecture6

getNextPrime :: Integer -> IO Integer
getNextPrime x = do
    isPrime <- primeMR 10 y
    if isPrime then return y else getNextPrime y
    where y = x + 1

getPrimePair :: IO (Integer, Integer)
getPrimePair = do
    x <- getStdRandom (randomR (1, 1000000000))
    y <- getStdRandom (randomR (1, 1000000000))
    x' <- getNextPrime (x + 2^32)
    y' <- getNextPrime (y + 2^32)
    return (x', y')

main = do
    (x, y) <- getPrimePair
    let pub = rsaPublic x y
    let priv = rsaPrivate x y
    let testMessage = 589618735
    let encoded = rsaEncode pub testMessage
    let decoded = rsaDecode priv encoded
    print decoded