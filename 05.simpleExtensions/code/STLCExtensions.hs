      {-|
      Module      : STLCExtensions
      Description : Simple Extensions to the Simply Typed Lambda Calculus
      Copyright   : (c) rbonifacio, 2017
      License     : GPL-3
      Maintainer  : rbonifacio@unb.br

      An implementation of several extensions to the Simply Typed Lambda
      Calculus, as detailed in the book Types and Programming Languages
      (B. Pierce)
      -}

      module STLCExtensions where

      import Prelude hiding (lookup)

      type Id = String 

      type Gamma = [(Id, Type)]

      type Label = String

      type RItem = (Label, Term)

      data Type = TBool
                | TInt
                | TString 
                | TUnit
                | TRecord [Maybe Type]
                | TArrow Type Type
                | TSum (+) Type Type
           deriving(Eq, Show)

      data Term = Var Id
                | Lambda (Id, Type) Term
                | App Term Term
                | Let Id Term Term
                | B Bool
                | N Int
                | S String 
                | Unit
                | Record [RItem]
                | Projection Term Label
                | IfThenElse Term Term Term
                | Add Term Term
                | Sum Record Record
              deriving(Eq, Show)  

      data Value = VBool Bool
                 | VInt Int
                 | VString String 
                 | VUnit
                 | VRecord [RItem]
                 | VFunction (Id, Type) Term
                 | VSum Record          
                 

      interp :: Term -> Value
      interp = undefined

      -- | The type checker function. It returns either a
      -- type (when the expression is well typed) or Nothing,
      -- in the case the expression is ill typed.
      (|-) :: Gamma -> Term -> Maybe Type

      -- a definition of the type checker function for each expression.

      gamma |- (B b)              = Just TBool

      gamma |- (N n)              = Just TInt

      gamma |- (S s)              = Just TString

      gamma |- Unit               = Just TUnit

      gamma |- (Sum x y)                = Just TSum

      gamma |- (Var v)            = lookup v gamma >>= \t1 -> Just t1

      gamma |- (Let v e1 e2)      = gamma          |- e1 >>= \t1 ->
                                    ((v,t1):gamma) |- e2 >>= \t2 ->
                                    Just t2                         

      gamma |- (Record items)     = let res = map (\(l,t) -> gamma |- t) items
                                    in Just (TRecord res) 

      gamma |- (Lambda (x, t1) t) = ((x,t1):gamma) |- t >>= \t2 -> return (TArrow t1 t2)

      gamma |- (App e1 e2)        = gamma |- e1 >>= \t1 ->
                                    gamma |- e2 >>= \t2 ->
                                    case t1 of
                                      (TArrow t11 t12) -> if t12 == t2 then Just t12 else Nothing
                                      otherwise -> Nothing
                               
      gamma |- (IfThenElse c t e) = gamma |- c >>= \t1 ->
                                    gamma |- t >>= \t2 ->
                                    gamma |- e >>= \t3 ->
                                    if(t1 == TBool && t2 == t3) then (Just t2) else Nothing

      gamma |- (Sum x y z) =  gamma |- y >>= \t1 ->
                              gamma |- z >>= \t2 ->
                                case x  | (x == t1) = t1
                                        | (x == t2) = t2
                                      otherwise -> Nothing

      gamma |- (Add e1 e2)        = gamma |- e1 >>= \t1 ->
                                    gamma |- e2 >>= \t2 ->
                                    if(t1 == TInt && t2 == TInt) then return TInt else Nothing


      -- | A lookup function. It searches for a specific
      -- mapping involving an identifier and a type. 
      lookup :: Id -> Gamma -> Maybe Type
      lookup k [] = Nothing
      lookup k ((v, t):tail)
       | k == v = Just t
       | otherwise = lookup k tail 
