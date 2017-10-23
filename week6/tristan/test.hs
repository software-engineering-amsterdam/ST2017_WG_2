import Data.List
import Criterion.Main

freqList1 :: String -> [(Char,Int)]
freqList1 s = map (\c -> (head c, length c)) (group (sort s))

freqList2 :: String -> [(Char,Int)]
freqList2 s = map (\c -> (c, length (filter (==c) s))) s' where s' = sort (nub s)

main = defaultMain [
    bgroup "freqList1" [ bench "kort"   $ nf freqList1 "abc"
    				   , bench "lang"   $ nf freqList1 "kjh wkjqhr kqjhr qlkjwrh qkhr kb ka qhiqo uwhqoi rqhjsd qjhdoq qwjhrqjhqw rhqrlj qhrkjn"
                  	   , bench "langer" $ nf freqList1 "qjhwrq wrqh qhwrqjh iqjobw oqiwd qowhq ouwhd qowufhq oiwufqop fhqpwofhq ofhq pfhq [wfqoihfq [owihr q[worih qwrq h9qph s9q8dyh qp9w8ry qp9s8dy qhpw98 hqwpr98q wrqp9w8r qpsdhqp9d qpw9 r8qwr"
                  	   , bench "langst" $ nf freqList1 "jhroni uwqhouir yet89 2nyt08 ygf0m8wmydp 98fymwp9e fwqp9ef u8 yfp9,q8f o9fg oiwgf owauhf wugh o9ug oiughap moirg umerhpig ehpmrguo ehrogi ehprg hpgh wepgh wpeghevoi eivehwi ouwproituweopirtu epowrgh epworigwepro igueporigy ehprotyehrtpo eiwtperwutyepw rotiyewrpotiyewp oivhepwoh weprotewr[ituw er0tipweurtp ewoity h"],
    bgroup "freqList2" [ bench "kort"   $ nf freqList2 "abc"
     				   , bench "lang"   $ nf freqList2 "kjh wkjqhr kqjhr qlkjwrh qkhr kb ka qhiqo uwhqoi rqhjsd qjhdoq qwjhrqjhqw rhqrlj qhrkjn"
                  	   , bench "langer" $ nf freqList2 "qjhwrq wrqh qhwrqjh iqjobw oqiwd qowhq ouwhd qowufhq oiwufqop fhqpwofhq ofhq pfhq [wfqoihfq [owihr q[worih qwrq h9qph s9q8dyh qp9w8ry qp9s8dy qhpw98 hqwpr98q wrqp9w8r qpsdhqp9d qpw9 r8qwr"
                  	   , bench "langst" $ nf freqList2 "jhroni uwqhouir yet89 2nyt08 ygf0m8wmydp 98fymwp9e fwqp9ef u8 yfp9,q8f o9fg oiwgf owauhf wugh o9ug oiughap moirg umerhpig ehpmrguo ehrogi ehprg hpgh wepgh wpeghevoi eivehwi ouwproituweopirtu epowrgh epworigwepro igueporigy ehprotyehrtpo eiwtperwutyepw rotiyewrpotiyewp oivhepwoh weprotewr[ituw er0tipweurtp ewoity h"]
    ]