-- ID: 11408227
-- Name: Vincent Jong
-- Time: 14:30 -  (Programming), (Testing), (Answering)

module Ex9 where

import Data.List
import Test.QuickCheck
import System.Random
import SetOrd
import Data.Char

type Var = String

data Expr = I Integer | V Var 
           | Add Expr Expr 
           | Subtr Expr Expr 
           | Mult Expr Expr 
           deriving (Eq)

data Condition = Prp Var 
                | Eq Expr Expr 
                | Lt Expr Expr 
                | Gt Expr Expr 
                | Ng Condition 
                | Cj [Condition] 
                | Dj [Condition]
                deriving (Eq)

data Statement = Ass Var Expr
                | Cond Condition Statement Statement
                | Seq [Statement]
                | While Condition Statement
                deriving (Eq)

instance Show Expr where
    show (I i) = show i
    show (V var) = show var
    show (Add expr1 expr2) = (show expr1) ++ " + " ++ (show expr2)
    show (Subtr expr1 expr2) = (show expr1) ++ " - " ++ (show expr2)
    show (Mult expr1 expr2) = (show expr1) ++ " * " ++ (show expr2)

instance Show Condition where
    show (Prp var) = show var
    show (Eq expr1 expr2) = (show expr1) ++ " == " ++ (show expr2)
    show (Lt expr1 expr2) = (show expr1) ++ " < " ++ (show expr2)
    show (Gt expr1 expr2) = (show expr1) ++ " > " ++ (show expr2)
    show (Ng condition) = "!(" ++ (show condition) ++ ")"
    show (Cj conditions) = showCj conditions
    show (Dj conditions) = showDj conditions

showCj, showDj :: [Condition] -> String
showCj [] = ""
showCj (cond:conds) = show cond ++ " && " ++ showCj conds
showDj [] = ""
showDj (cond:conds) = show cond ++ " || " ++ showDj conds

instance Show Statement where
    show (Ass var expr) = (show var) ++ " := " ++ (show expr)
    show (Cond cond thenStmt elseStmt) = "if (" ++ (show cond) ++ ") " ++
                                         "then {\n" ++ (show thenStmt) ++ " } " ++  
                                         "else {\n" ++ (show elseStmt) ++ " }"
    show (Seq lst) = showSeq lst
    show (While cond stmt) = "while (" ++ (show cond) ++ ") " ++
                             " {\n" ++ (show stmt) ++ "}"

showSeq :: [Statement] -> String
showSeq [] = ""
showSeq ((Ass var expr):stmts) = show var ++ " := " ++ show expr ++ ";\n" ++ showSeq stmts
showSeq (stmt:stmts) = show stmt ++ showSeq stmts

data Token
    = TokenInt Integer
    | TokenVar String
    | TokenPlus
    | TokenMinus
    | TokenMult
    | TokenEq
    | TokenLt
    | TokenGt
    | TokenNg
    | TokenCj
    | TokenDj
    | TokenAss
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenSeq
    | TokenWhile
    | TokenOP
    | TokenCP
    | TokenOB
    | TokenCB

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexNum (c:cs)
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('{':cs) = TokenOB : lexer cs
lexer ('}':cs) = TokenCB : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenMult : lexer cs
lexer ('=':'=':cs) = TokenEq : lexer cs
lexer ('<':cs) = TokenLt : lexer cs
lexer ('>':cs) = TokenGt : lexer cs
lexer ('!':cs) = TokenNg : lexer cs
lexer ('&':'&':cs) = TokenCj : lexer cs
lexer ('|':'|':cs) = TokenDj : lexer cs
lexer (':':'=':cs) = TokenAss : lexer cs
lexer ('i':'f':cs) = TokenIf : lexer cs
lexer ('t':'h':'e':'n':cs) = TokenThen : lexer cs
lexer ('e':'l':'s':'e':cs) = TokenElse : lexer cs
lexer (';':cs) = TokenSeq : lexer cs
lexer ('w':'h':'i':'l':'e':cs) = TokenWhile : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
    where (num, rest) = span isDigit cs

type Parser a b = [a] -> [(b, [a])]

succeed :: b -> Parser a b
succeed x xs = [(x, xs)]

parseExpr :: Parser Token Expr
parseExpr (TokenInt i : tokens) = [(I i, tokens)]
parseExpr (TokenVar v : tokens) = [(V v, tokens)]
--parseExpr (TokenPlus : tokens) = 

--parseCond:: Parser Token Condition

--parseStatement :: Parser Token Statement
--parseStatement (TokenAss : tokens) = [(I x, tokens)]


--parse :: String -> [Statement]
--parse s = [ f | (f, _) <- parseForm (lexer s)]

--Seq [Ass "x" (I 0), Ass "y" (I 1), While (Gt (V "n") (I 0)) (Seq [Ass "z" (V "x"), Ass "x" (V "y"), Ass "y" (Add (V "z") (V "y")), Ass "n" (Subtr (V "n") (I 1))])]