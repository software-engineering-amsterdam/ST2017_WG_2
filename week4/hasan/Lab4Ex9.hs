module Lab4Ex8 where

import Lecture4
import Data.List
import Data.Char

---------------
---SHOW PART---
---------------

instance Show Statement where
	show (Ass v e) = "(ass " ++ (id v) ++ " " ++ (show e) ++ ")"
	show (While c s) = "(while " ++ (show c) ++ " " ++ (show s) ++ ")"
	show (Seq ss) = "(seq " ++ (intercalate " " (map show ss)) ++ ")"
	show (Cond c s1 s2) = "(if " ++ (show c) ++ " " ++ (show s1) ++ " " ++ (show s2) ++ ")"

instance Show Expr where
	show (I i) = show i
	show (V v) = "(" ++ id v ++ ")"
	show (Add e1 e2) = "(+ " ++ (show e1) ++ (show e2) ++ ")"
	show (Subtr e1 e2) = "(- " ++ (show e1) ++ (show e2) ++ ")"
	show (Mult e1 e2) = "(* " ++ (show e1) ++ (show e2) ++ ")"

instance Show Condition where
	show (Prp v) = id v
	show (Eq e1 e2) = "(== " ++ (show e1) ++ " " ++ (show e2) ++ ")"
	show (Lt e1 e2) = "(<= " ++ (show e1) ++ " " ++ (show e2) ++ ")"
	show (Gt e1 e2) = "(>= " ++ (show e1) ++ " " ++ (show e2) ++ ")"
	show (Ng c) = "(not " ++ (show c) ++ ")"
	show (Cj cs) = "(and " ++ (intercalate " " (map (\c -> show c) cs)) ++ ")"
	show (Dj cs) = "(or " ++ (intercalate " " (map (\c -> show c) cs)) ++ ")"


---------------
---READ PART---
---------------
	
-- Modified from Lecture3.hs
data Token
      = TokenAdd
	  | TokenSubtr
	  | TokenMult
	  | TokenEq
      | TokenLt
      | TokenGt
	  | TokenNeg
      | TokenOP
      | TokenCP
      | TokenCj
      | TokenDj
	  | TokenAss
	  | TokenWhile
	  | TokenSeq
      | TokenInt Int
	  | TokenVar String
 deriving (Show,Eq)

lexer :: String -> [Token]
lexer [] = []
--lexer ('v':'a':'r':' ':v:cs) = TokenVar v : lexer cs
lexer ('+':cs) = TokenAdd : lexer cs
lexer ('-':cs) = TokenSubtr : lexer cs
lexer ('*':cs) = TokenMult : lexer cs
lexer ('=':'=':cs) = TokenEq : lexer cs
lexer ('<':'=':cs) = TokenLt : lexer cs
lexer ('>':'=':cs) = TokenGt : lexer cs
lexer ('n':'o':'t':cs) = TokenNeg : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('a':'n':'d':cs) = TokenCj : lexer cs
lexer ('o':'r':cs) = TokenDj : lexer cs
lexer ('a':'s':'s':cs) = TokenAss : lexer cs
lexer ('w':'h':'i':'l':'e':cs) = TokenWhile : lexer cs
lexer ('s':'e':'q':cs) = TokenSeq : lexer cs
lexer (c:cs) 
	| isSpace c = lexer cs
	| isDigit c = lexNum (c:cs)
	| otherwise = TokenVar (c:[]) : lexer cs

lexer (x:_) = error ("unknown token: " ++ [x])
lexNum cs = TokenInt (read num) : lexer rest
     where (num,rest) = span isDigit cs

type Parser a b = [a] -> [(b,[a])]
succeed :: b -> Parser a b
succeed x xs = [(x,xs)]


parseCondition :: Parser Token Condition
--parseCondition (TokenOP : tokens : TokenCP) = [(parseCondition tokens ,tokens)]
parseCondition (TokenVar x : tokens) = [(Prp x,tokens)]
parseCondition (TokenNeg : tokens) =
  [ (Ng f, rest) | (f,rest) <- parseCondition tokens ]
parseCondition (TokenCj : TokenOP : tokens) =
  [ (Cj fs, rest) | (fs,rest) <- parseConditions tokens ]
parseCondition (TokenDj : TokenOP : tokens) =
  [ (Dj fs, rest) | (fs,rest) <- parseConditions tokens ]
parseCondition (TokenEq : TokenOP : tokens) =
  [ (Eq f1 f2,rest) | (f1,y1:rest) <- parseExpression tokens, (f2,y2:rest) <- parseExpression rest, y1 == TokenCP, y2 == TokenCP ]
--parseStatement (TokenOP : tokens) =
--  [ (Impl f1 f2, rest) | (f1,ys) <- parseStatement tokens,
--                         (f2,rest) <- parseImpl ys ]
--   ++
--  [ (Equiv f1 f2, rest) | (f1,ys) <- parseStatement tokens,
--                          (f2,rest) <- parseEquiv ys ]
parseCondition a = [(Prp "a",a)] -- dummy


-- TODO
parseExpression :: Parser Token Expr
parseExpression tokens = 
	[(I 0,[])]

parseConditions :: Parser Token [Condition]
parseConditions (TokenCP : tokens) = succeed [] tokens
parseConditions tokens =
   [(f:fs, rest) | (f,ys) <- parseCondition tokens,
                   (fs,rest) <- parseConditions ys ]
				   

-- reference
--fib = Seq [Ass "x" (I 0), Ass "y" (I 1), 
--           While (Gt (V "n") (I 0))
--             (Seq [Ass "z" (V "x"), 
--                   Ass "x" (V "y"),
--                   Ass "y" (Add (V "z") (V "y")), 
--                   Ass "n" (Subtr (V "n") (I 1))])]

runLab4Ex8 = do
	let input = "(\== a a)"
	--let input = show fib
	--putStrLn (show (I 0))
	--putStrLn (show (Subtr (V "n") (I 1)))
	putStrLn input
	putStrLn ""
	print (lexer input)
	putStrLn ""
	let result = head (parseCondition (lexer input))
	putStrLn ( show result)
	--print (parseCondition (lexer (show fib)))