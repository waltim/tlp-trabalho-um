module AlphaConversion where

import Data.List

type Symbol = Char
type Index = Int

data Expression = Var Symbol
    | Lambda Symbol Expression
    | App Expression Expression
    deriving (Eq, Read, Show)

data Expression' = Var' Index 
    | App' Expression' Expression' 
    | Lambda' Expression'
    deriving (Eq, Read, Show)

evalExpression :: Expression -> Expression'
evalExpression = toEvaluate []
    where
    -- Found a variable: look up its index in the environment
    toEvaluate environment (Var symbol) = case findIndex (== symbol) environment of
      Just index -> Var' index
      Nothing -> error "Unbound Variable"
    -- Found a lambda: we add its variable to the environment
    toEvaluate environment (Lambda symbol exp) = Lambda' (toEvaluate (symbol : environment) exp)
    -- Other cases are straightforward
    toEvaluate environment (App e1 e2) = App' (toEvaluate environment e1) (toEvaluate environment e2)


alphaEquivalence expression1 expression2 = evalExpression expression1 == evalExpression expression2