module Lab4Ex8 where

import Lecture4
import Data.List

showExpression :: Expr -> String
showExpression (I i) = show i
showExpression (V v) = id v
showExpression (Add e1 e2) = (showExpression e1) ++ " + " ++ (showExpression e2)
showExpression (Subtr e1 e2) = (showExpression e1) ++ " - " ++ (showExpression e2)
showExpression (Mult e1 e2) = (showExpression e1) ++ " * " ++ (showExpression e2)
showExpression _ = "?unparseable expression?"

showCondition :: Condition -> String
--showCondition (Prp v) = showExpression v
showCondition (Eq e1 e2) = (showExpression e1) ++ " == " ++ (showExpression e2)
showCondition (Lt e1 e2) = (showExpression e1) ++ " <= " ++ (showExpression e2)
showCondition (Gt e1 e2) = (showExpression e1) ++ " >= " ++ (showExpression e2)
showCondition (Ng c) = "!(" ++ (showCondition c) ++ ")"
showCondition (Cj cs) = intercalate " && " (map (\c -> showCondition c) cs)
showCondition (Dj cs) = intercalate " || " (map (\c -> showCondition c) cs)
showCondition _ = "?unparseable condition?"

showStatement :: String -> Statement -> String
showStatement prefix (Ass v e) = prefix ++ (id v) ++ " = " ++ (showExpression e)
showStatement prefix (While c s) = prefix ++ "while (" ++ (showCondition c) ++ ")\r\n" ++ (showStatement (prefix ++ "\t") s)
showStatement prefix (Seq ss) = (intercalate "\r\n" (map (showStatement prefix) ss))
showStatement prefix _ = prefix ++ "?unparseable statement?"


parseExpression :: String -> Expr


runLab4Ex8 = do
	--putStrLn (showExpression (I 0))
	--putStrLn (showStatement (Subtr (V "n") (I 1)))
	putStrLn (showStatement "" fib)