-- problem 89

module Ex10b where

import Control.Monad

data Roman = I | V | X | L | C | D | M deriving (Show, Eq)

getLines :: String -> IO [String]
getLines = liftM lines . readFile

minimize :: String -> String
minimize [] = []
minimize ('I':'I':'I':'I':'V':r) = minimize ('X':'I':(minimize r))
minimize ('I':'I':'I':'I':'I':r) = minimize ('V':(minimize r))
minimize ('I':'I':'I':'I':r) = minimize ('V':'I':(minimize r))
minimize ('V':'V':r) = minimize ('X':(minimize r))
minimize ('X':'X':'X':'X':'L':r) = minimize ('C':'X':(minimize r))
minimize ('X':'X':'X':'X':'X':r) = minimize ('L':(minimize r))
minimize ('X':'X':'X':'X':r) = minimize ('L':'X':(minimize r))
minimize ('L':'L':r) = minimize ('C':(minimize r))
minimize ('C':'C':'C':'C':'D':r) = minimize ('M':'C':(minimize r))
minimize ('C':'C':'C':'C':'C':r) = minimize ('D':(minimize r))
minimize ('C':'C':'C':'C':r) = minimize ('D':'C':(minimize r))
minimize ('D':'D':r) = minimize ('M':(minimize r))
minimize (r:rs) = r : minimize rs

main = do 
    lines <- getLines "roman.txt"
    let minimized = map minimize lines
    let zipped = zip lines minimized
    mapM_ print zipped