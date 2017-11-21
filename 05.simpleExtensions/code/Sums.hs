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

type TItem = (Term)

data Type = TBool
          | TInt
          | TString 
          | TUnit
          | TRecord [Maybe Type]
          | TTuple [Maybe Type]
          | TTProjection Type
          | TRProjection Type
          | TArrow Type Type
          | TSum Type Type
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
          | Tuple [TItem]
          | TProjection Int Term
          | RProjection Label Term
          | IfThenElse Term Term Term
          | Add Term Term
          | Inl Term
          | Inr Term
          | CaseInlInr Term (Id, Term) (Id, Term)
        deriving(Eq, Show) 

data Value = VBool Bool
           | VInt Int
           | VString String 
           | VUnit
           | VRecord [RItem]
           | VTuple [TItem]
           | VRProjection Term 
           | VTProjection Term
           | VFunction (Id, Type) Term  
           | VInl Term
           | VInr Term
        deriving(Eq, Show) 

interp :: Term -> Value
interp (Inl items)                    = VInl Term
interp (Inr items)                    = VInr Term
interp (Record items)             = VRecord items
interp (Tuple items)              = VTuple items 
interp (RProjection label record) = VRProjection (searchRecord (label) (record))
interp (TProjection index tuple)  = VTProjection (searchTuple (index) (tuple))

-- | The type checker function. It returns either a
-- type (when the expression is well typed) or Nothing,
-- in the case the expression is ill typed.
(|-) :: Gamma -> Term -> Maybe Type

-- a definition of the type checker function for each expression.

gamma |- (B b)                      = Just TBool

gamma |- (N n)                      = Just TInt

gamma |- (S s)                      = Just TString

gamma |- Unit                       = Just TUnit

gamma |- (Inl term)                    = VInl Term

gamma |- (Inr term)                    = VInl Term

gamma |- (CaseInlInr t Inl Inr)       = gamma |- t >>= \t1 ->
                                      gamma |- Inl >>= \t2 ->
                                      gamma |- Inr >>= \t3 ->
                                      if(t1 == t2) then (Just t2) else if(t1 == t3) then (Just t3) else Nothing

gamma |- (Var v)                    = lookup v gamma >>= \t1 -> Just t1

gamma |- (Let v e1 e2)              = gamma         |- e1 >>= \t1 ->
                                     ((v,t1):gamma) |- e2 >>= \t2 ->
                                     Just t2                         

gamma |- (Record items)             = let res = map (\(l,t) -> gamma |- t) items
                                      in Just (TRecord res) 

gamma |- (Tuple items)              = let res = map (\(t) -> gamma |- t) items
                                      in Just (TTuple res)

gamma |- (RProjection label record) = gamma |- (searchRecord (label) (record)) >>= \t1 -> return (TRProjection t1)

gamma |- (TProjection index tuple)  = gamma |- (searchTuple (index) (tuple)) >>= \t1 -> return (TTProjection t1)

gamma |- (Lambda (x, t1) t)         = ((x,t1):gamma) |- t >>= \t2 -> return (TArrow t1 t2)

gamma |- (App e1 e2)                = gamma |- e1 >>= \t1 ->
                                      gamma |- e2 >>= \t2 ->
                                      case t1 of
                                      (TArrow t11 t12) -> if t12 == t2 then Just t12 else Nothing
                                      otherwise -> Nothing
                         
gamma |- (IfThenElse c t e)         = gamma |- c >>= \t1 ->
                                      gamma |- t >>= \t2 ->
                                      gamma |- e >>= \t3 ->
                                      if(t1 == TBool && t2 == t3) then (Just t2) else Nothing

    
gamma |- (Add e1 e2)                = gamma |- e1 >>= \t1 ->
                                      gamma |- e2 >>= \t2 ->
                                      if(t1 == TInt && t2 == TInt) then return TInt else Nothing


-- | A lookup function. It searches for a specific
-- mapping involving an identifier and a type. 
lookup :: Id -> Gamma -> Maybe Type
lookup k [] = Nothing
lookup k ((v, t):tail)
 | k == v = Just t
 | otherwise = lookup k tail 

 
-- | A match function. It combines a label list and 
-- a term list to a Record type
match :: [Label] -> [Term] -> Maybe Term
match ls ts = Record <$> matchItems ls ts 


-- | A match items function. It combines the labels and terms
-- in a list so that the Record is built
matchItems :: [l] -> [t] -> Maybe [(l,t)]
matchItems [] []         = Just []
matchItems (l:ls) (t:ts) = ((l,t):) <$> matchItems ls ts
matchItems _ _           = Nothing


-- | A search function to records. It looks for a certain element in
-- the record by its label and returns its value
searchRecord :: Label -> Term -> Term
searchRecord _ (Record [])                  = error "element in Record not found"
searchRecord (x) (Record ((label,item):xs)) = if x == label then item else searchRecord (x) (Record xs)


-- | A search function to tuples. Its looks for a certain element by index
-- in the tuple and return its value 
searchTuple :: Int -> Term -> Term 
searchTuple _ (Tuple [])        = error "element in Tuple not found"
searchTuple index (Tuple items) = items !! index
