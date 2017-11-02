module EnumFix where

import Control.Monad
import FBackTrack                       -- monad for non-determinism
import Data.List (elemIndex)

import Debug.Trace

-- The data type of untyped lambda terms
-- We use deBruijn indices. That is, all bound variables
-- are non-negative integers. Negative integers index global
-- variables, such as the distinct variable e.

data Exp = 
    V Int                      -- deBruijn index, if non-negative
  | A Exp Exp
  | L Exp
 deriving (Show, Eq)

-- Pretty-print terms
pp :: Exp -> String
pp e = go [] 0 e
 where
 go vars p (V n) = vars !! n
 go vars p (A e1 e2) = paren (p > 10) (go vars 10 e1 ++ " " ++ go vars 11 e2)
 go vars p (L e) = 
   let vi = length vars in
   let vn = var_name vi in
   let body = go (vn:vars) 0 e in
   paren (p > 0) ("L" ++ vn ++ "." ++ body)

 var_name i | i < length var_names = [var_names !! i]
 var_name i = "x" ++ show i
 var_names = "xyzuvwst"

 paren True  text = "(" ++ text ++ ")"
 paren False text = text

-- Checking if a term is too big, bigger than n constructors.
-- It is useful to `nice' checking of large terms, biasing
-- the search to smaller terms first
-- The function returns the remaining of the constructor balance
too_big :: Int -> Exp -> Int
too_big 0 _   = 0
too_big n V{} = n-1
too_big n (A e1 e2) = (too_big $! too_big n e1) e2
too_big n (L e) = (too_big $! (n-1)) e

-- Test if a variable of a specific index is free in an expression
is_free :: Int -> Exp -> Bool
is_free n (V n')    = n /= n'
is_free n (L e)     = is_free (n+1) e
is_free n (A e1 e2) = is_free n e1 && is_free n e2

-- Check to see if a term is an application with a variable at
-- its head.
-- Such a term preserves its shape upon reductions (if there are any).
is_vheaded :: Exp -> Bool
is_vheaded V{}     = True
is_vheaded (A e _) = is_vheaded e
is_vheaded _       = False


-- Non-deterministically generate all closed lambda-terms
-- (which must be abstractions)
-- This is always possible since the set of terms is recursively
-- enumerable.

-- The most straightforward generator
a_term_naive :: MonadPlus m => m Exp
a_term_naive = L `liftM` go 0
 where
 -- go n: generate a lambda term with free variables 0..n
 go n = choose_var n `mplus` choose_lam n `mplus` choose_app n
 choose_var 0 = return (V 0)
 choose_var n = return (V n) `mplus` choose_var (n-1)  
 choose_app n = liftM2 A (go n) (go n)
 choose_lam n = L `liftM` go (n+1)

{-
a_term :: MonadPlus m => m Exp
a_term = L `liftM` go 0
 where
 -- go n: generate a lambda term with free variables 0..n
 go n = choose_var n `mplus` choose_app n `mplus` yield (choose_lam n)
 choose_var 0 = return (V 0)
 choose_var n = choose_var (n-1) `mplus` return (V n)
 choose_app n = liftM2 A (go n) (go n)
 choose_lam n = L `liftM` go (n+1)
-}

a_term :: MonadPlus m => m Exp
a_term = L `liftM` go 0
 where
 -- go n: generate a lambda term with free variables 0..n
 go n = do
   size <- iota 1
   gen n True size
 -- gen n l s: generate a lambda term with free variables 0..n and
 -- of the size exactly s. The second argument tells if to generate
 -- abstractions
 gen _ _ s | s <= 0 = mzero
 gen n _     1 = choose_var n
 gen n True  2 = choose_lam n 2
 gen n False 2 = mzero
 gen n True  s = choose_app n s `mplus` choose_lam n s
 gen n False s = choose_app n s

 choose_var n = msum . map (return . V) $ [0..n]
 choose_lam n s = penalty (40*n) $ L `liftM` gen (n+1) True (s-1)
 choose_app n s = do
   let s' = s - 1                       -- Account for the 'A' constructor
   let gen_redex = do
                   lefts <- range 4 (s' - 3)
                   liftM2 A (choose_lam n lefts) (choose_lam n (s' - lefts))
   let gen_noredex = do
                     lefts <- range 1 (s' - 1)
                     liftM2 A (gen n False lefts) (gen n True (s'-lefts))
   gen_noredex `mplus` penalty 4 gen_redex

 -- choose the size of a term: i .. 
 iota i | i > 16 = mzero
 iota i = return i `mplus` penalty 3 (iota (i+1))
 -- choose a number within i .. j
 range i j | i > j  = mzero
 range i j | i == j = return i
 range i j = return i `mplus` range (i+1) j

 -- Lowers the priority of m, so choices of m will be tried less often
yield :: MonadPlus m => m a -> m a
yield m = mzero `mplus` m

-- Penalize the action making it of lower priority
penalty :: MonadPlus m => Int -> m a -> m a
penalty 0 m = m
penalty n m = mzero `mplus` penalty (n-1) m

-- Generate sample terms
test_terms = map pp $ runM (Just 5) a_term
-- ["Lx. x","Lx. Ly. y","Lx. x x","Lx. Ly. Lz. z","Lx. (Ly. y) x"]

-- The Y combinator, for testing
-- \f. (\u. uu) (\p. f(p p))
term_Y = L $ A (L $ A (V 0) (V 0)) (L $ A (V 1) (A (V 0) (V 0)))
term_Y_pp = pp term_Y
-- "Lx. (Ly. y y) (Ly. x (y y))"

-- Eta-expanded Y combinator
term_Y_eta = L $ (A term_Y (V 0))
term_Y_eta_pp = pp term_Y_eta

term_Y_id = L $ (A term_Y (A (L (V 0)) (V 0)))
term_Y_id_pp = pp term_Y_id

delta = L (A (V 0) (V 0))
omega = A delta delta

-- 3d Church numeral
c3 = L $ L $ V 1 `A` (V 1 `A` (V 1 `A` V 0))

-- The successor
lsucc = L $ L $ L $ V 1 `A` (V 2 `A` V 1 `A` V 0)
-- "Lx. Ly. Lz. y (x y z)"

-- The addition function
ladd = L $ L $ L $ L $ V 3 `A` V 1 `A` (V 2 `A` V 1 `A` V 0)
-- "Lx. Ly. Lz. Lu. x z (y z u)"

-- The S combinator
combS = L $ L $ L $ V 2 `A` V 0 `A` (V 1 `A` V 0)
-- "Lx. Ly. Lz. x z (y z)"

-- Fokker's one-base X combinator
combX = L $ V 0 `A` combS `A` (L $ L $ L $ V 2)
-- "Lx. x (Ly. Lz. Lu. y u (z u)) (Ly. Lz. Lu. y)"

-- locate_terms_naive = loop terms $ take 10000 $ zip [1..] a_term_naive
-- locate_terms_naive = loop terms $ take 10000 $ zip [1..] (iter_deepening $ reify a_term_naive)
locate_terms_naive = loop terms $ zip [1..] (runM (Just 100000) a_term_naive)
 where
 loop _ [] = []
 loop [] _ = []
 loop terms ((i,h):t) | (l,(_:r)) <- break (== h) terms =
    (i, pp h) : loop (l++r) t
 loop terms (_:t) = loop terms t
 terms = [delta, c3, lsucc, term_Y, L omega, ladd, combS, combX,
          term_Y_eta, term_Y_id]

-- Testing that we can generate interesting terms, including
-- Y and its variations
locate_terms = loop terms $ zip [1..] (runM (Just 200000) a_term)
 where
 loop _ [] = []
 loop [] _ = []
 loop terms ((i,h):t) | (l,(_:r)) <- break (== h) terms =
   (i, pp h) : loop (l++r) t
 loop terms (_:t) = loop terms t
 terms = [delta, c3, lsucc, term_Y, L omega, ladd, combS, combX,
          term_Y_eta, term_Y_id]

-- Just 1650 for Y


e = V (-1)                         -- distinguished variable

list_fix :: MonadPlusOnce m => m Exp
list_fix = do
 term <- a_term
 -- term <- return term_Y   -- for testing
 -- term <- return term_Y_eta   -- for testing
 -- term <- return term_Y_id   -- for testing
 -- term <- return (L $ omega)   -- for testing
 trace ("got term " ++ pp term) (return ())
 let fixe = A term e
 once $ check_equal fixe (A e fixe)
 return term

-- Check that two lambda-terms are equal
check_equal :: MonadPlus m => Exp -> Exp -> m ()

  -- Do eta-reductions right away
check_equal (L (A t (V 0))) t' | is_free 0 t = check_equal t t'
check_equal t' (L (A t (V 0))) | is_free 0 t = check_equal t t'

   -- first, some easy checks
check_equal (V n1) (V n2) = if n1 == n2 then return () else mzero
check_equal V{} L{}  = mzero
check_equal t@V{} (A t1 t2) = fully_reduce_app t1 t2 >>= check_equal t

check_equal L{} V{}  = mzero
check_equal (L t1) (L t2) = check_equal t1 t2
check_equal t@L{} (A t1 t2) = fully_reduce_app t1 t2 >>= check_equal t

check_equal t1@A{} t2@V{} = check_equal t2 t1
check_equal t1@A{} t2@L{} = check_equal t2 t1

-- If two terms are vheaded applications, they should compare
-- along the spine: vheaded application keep their spine through further
-- reductions.
check_equal (A t1f t1x) (A t2f t2x) | is_vheaded t1f && is_vheaded t2f =
 check_equal t1f t2f >> check_equal t1x t2x

-- If t1 is in the vhead form, then t2 must reduce to a vhead form
check_equal t1@A{} t2 | is_vheaded t1 = vhead_norm t2 >>= check_equal t1
check_equal t2 t1@A{} | is_vheaded t1 = vhead_norm t2 >>= check_equal t1

check_equal t1 t2 | t1 == t2 = return ()

 -- always contract an application to a variable
check_equal t1@(A L{} V{}) t2 = reduce_one_step t1 >>= check_equal t2
check_equal t2 t1@(A L{} V{}) = reduce_one_step t1 >>= check_equal t2

-- We are now comparing two applications that may reduce to something else.
-- Chances are the applications can be equal componentwise.
-- If not, we have to reduce try reductions.
-- check_equal t1 t2 | trace ("ce: " ++ show t1 ++"; " ++ show t2) False = undefined
check_equal t1 t2 =
    try_componentwise t1 t2 `mplus`  throttle (try1 t1 t2 `mplus` try1 t2 t1)
 where
 try_componentwise (A t1f t1x) (A t2f t2x) =
     check_equal t1f t2f >> check_equal t1x t2x
 try1 t1 t2 = do
  t1 <- reduce_one_step t1
  check_equal t1 t2

 -- impose a limit for now. Later on: iterative deepening
 -- throttle m = if too_big 11 t1 == 0 || too_big 11 t2 == 0 then mzero else m
 throttle m = if too_big 11 t1 == 0 then penalty 10 m else m


-- Pick a redex and contract it. Fail if no redex or if applying
-- any redex leaves the term intact (that is, no progress on the term
-- can be made either way).
reduce_one_step :: MonadPlus m => Exp -> m Exp
reduce_one_step t = go t
 where
 go (V _) = mzero
 go (L (A t (V 0))) | is_free 0 t = return t -- always do eta, if applicable
 go (L t) = go t >>= return . L
 go (A v@V{} t) = go t >>= return . A v
 -- always contract an application to a variable
 go (A (L tb) t2@V{}) = return $ subst tb 0 t2
 go t@(A t1 t2) = try1 `mplus` try_redex t1 `mplus` try2
  where
  try1 = go t1 >>= \t1 -> return (A t1 t2)
  try2 = go t2 >>= \t2 -> return (A t1 t2)
  try_redex (L tb) = let t' = subst tb 0 t2 in -- no progress
                     if t == t' then mzero else return t'
  try_redex _ = mzero


-- Attempt to reduce a term to a vhead normal form
vhead_norm :: MonadPlus m => Exp -> m Exp
vhead_norm t = go 10 t
 where
 go 0 t = mzero -- `mplus` go 10 t -- try again at lower pri
 go _ t@V{} = return t
 go n (L (A t (V 0))) | is_free 0 t = go n t
 go _ t@L{} = return t
 go _ t@(A V{} _)   = return t
 go n (A (L tb) t2) = go (n-1) $ subst tb 0 t2
 go n (A t1 t2) = go n t1 >>= go2 n t2

 -- Result is in the vhead form. Check if we have a redex
 go2 n t2 (L tb) = go (n-1) $ subst tb 0 t2
 go2 n t2 t      = return (A t t2)

-- Attempt to reduce the application (A t1 t2) so that the
-- result is no longer an application
fully_reduce_app :: MonadPlus m => Exp -> Exp -> m Exp
fully_reduce_app t1 t2 = vhead_norm (A t1 t2) >>= check
 where
 check A{} = mzero
 check t   = return t


-- subst tb n t
-- substitute in tb the variable n with t, shifting down
-- the variables greater than n
subst :: Exp -> Int -> Exp -> Exp
subst (V m) n t | m == n = t
subst (V m) n _ | m > n  = V (m-1)
subst v@V{} _ _  = v
subst (L tb) n t = L (subst tb (n+1) t)
subst (A tb1 tb2) n t = A (subst tb1 n t) (subst tb2 n t)

-- A few checks

teq1 = runM (Just 1000) $               -- eta-expanded omega
       once $ check_equal omega (A (L (A delta (V 0))) delta)
-- [()]         -- Equal

teq2 = runM (Just 100) $                -- eta-expanded omega
       once $ check_equal omega (A (L (A delta (V 0))) (L (A delta (V 0))))
-- [()]

teq3 = runM (Just 100) $
       once $ check_equal omega (A delta (L (A delta (A (L (V 0)) (V 0)))))
-- [()]

teq4 = runM (Just 100) $
       once $ check_equal term_Y_eta term_Y
-- [()]

teq5 = runM (Just 400) $
       once $ check_equal term_Y term_Y_id

-- Main function

test_fix = map pp $ runM (Just 2000) list_fix


main = print $ test_fix