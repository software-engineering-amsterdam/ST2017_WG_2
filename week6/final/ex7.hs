module Ex7 where
import Lecture6
import Lecture2

findPrimeAfter :: Integer -> IO Integer
findPrimeAfter x = do
    ip <- primeMR 10 x
    if ip && prime x then return x else do
        res2 <- findPrimeAfter (x+1)
        return res2

main = do
    let bitLength = 31
    randomNumber1 <- getRandomInt (2^bitLength)
    randomNumber2 <- getRandomInt (2^bitLength)
    p <- findPrimeAfter (2^bitLength + (fromIntegral randomNumber1))
    q <- findPrimeAfter (2^bitLength + (fromIntegral randomNumber2))
    
    putStrLn "\np is:"
    print p
    
    putStrLn "\nq is:"
    print q
    
    let privateKey = rsaPublic p q
    putStrLn "\nprivate key is:"
    print privateKey
    
    let publicKey = rsaPrivate p q
    putStrLn "\npublic key is:"
    print publicKey
    
    let input = 1337133713371337
    putStrLn "\ninput is:"
    print input
    
    let encoded = rsaEncode privateKey input
    putStrLn "\nencoded input is:"
    print encoded
    
    let decoded = rsaDecode publicKey encoded
    putStrLn "\ndecoded input is:"
    print decoded
    
    let eq = decoded == input
    putStrLn "\ndecoded equals input:"
    print eq
    
    let neq = encoded /= input
    putStrLn "\nencoded does not equal input:"
    print neq
    