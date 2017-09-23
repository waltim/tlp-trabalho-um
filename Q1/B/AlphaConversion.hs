module AlphaConversion where

import Data.List

type Symbol = String
type Index = Int

data Expression = Var Symbol    -- variable
    | Lambda Symbol Expression  -- abstraction
    | App Expression Expression -- application
    deriving (Eq, Read)

data Expression' = Var' Index 
    | Lambda' Expression'
    | App' Expression' Expression'
    deriving (Eq, Read)

instance Show Expression where
    show (Var symbol)               = symbol
    show (App (Var s1) (Var s2))    = s1 ++ s2
    show (App t1 t2)                = "(" ++ show t1 ++ ")(" ++ show t2 ++ ")"
    show (Lambda symbol t)          = "\\" ++ symbol ++ "." ++ show t

------------------------------------------------ ALPHA EQUIVALENCE --------------------------------------------------	

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

------------------------------------------------ ALPHA CONVERSION --------------------------------------------------

freeVars :: Expression -> [Symbol]
freeVars (Var v)      = [v]
freeVars (App p q)    = freeVars p `union` freeVars q
freeVars (Lambda v t) = freeVars t \\ [v]

substitution :: Expression -> Symbol -> Expression -> Expression
substitution m@(Var var)   symbol n | var == symbol  = n
                                 | otherwise   = m
substitution   (App p q)   symbol n = App (substitution p symbol n) (substitution q symbol n)
substitution m@(Lambda var p) symbol n | var == symbol            = m 
                                 | var `elem` freeVars n = Lambda newName $ conversion p var newName
                                 | otherwise             = Lambda var $ substitution p symbol n
                                 where newName = findUnboundInTerms var [n,p]

findUnboundInTerms :: Symbol -> [Expression] -> Symbol
findUnboundInTerms originalName _ = originalName ++ "\'"

conversion :: Expression -> Symbol -> Symbol -> Expression
conversion (Lambda v m) x y | v /= x  = Lambda v $ conversion m x y
                                             | y `elem` freeVars m = error (y ++ " \\in FV(" ++ show m ++ ")")
                                             | otherwise           = Lambda y $ substitution m x (Var y)
conversion (App p q) x y              = App (conversion p x y) (conversion q x y)
conversion m@(Var v) x y | v == x     = Var y
                                             | otherwise = m

alphaConversion :: Expression -> [(Symbol, Symbol)] -> Expression
alphaConversion (exp) ((a,b):xs) = 
    let exp1 = conversion (exp) (a) (b) 
    in 
    alphaConversion (exp1) (xs)
alphaConversion exp [] = exp