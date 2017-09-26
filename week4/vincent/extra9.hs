-- ID: 11408227
-- Name: Vincent Jong
-- Time: 14:30 -  (Programming), (Testing), (Answering)

module Ex9 where

import Data.List
import Test.QuickCheck
import System.Random
import SetOrd

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
    show (Ass var expr) = (show var) ++ " = " ++ (show expr)
    show (Cond cond thenStmt elseStmt) = "if (" ++ (show cond) ++ ") " ++
                                         "then { " ++ (show thenStmt) ++ " } " ++  
                                         "else { " ++ (show elseStmt) ++ " }"
    show (Seq lst) = showSeq lst
    show (While cond stmt) = "while (" ++ (show cond) ++ ") " ++
                             "do { " ++ (show stmt) ++ " }"

showSeq :: [Statement] -> String
showSeq [] = ""
showSeq (stmt:stmts) = show stmt ++ "; " ++ showSeq stmts

data Token
    = TokenPlus
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
    | TokenSeq
    | TokenWhile
    | TokenBracket

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lex