module StandardLib where

import Lambda

true :: Term
true = Lambda "t" (Lambda "f" (Var "t"))

false :: Term
false = Lambda "t" (Lambda "f" (Var "f"))

zero :: Term
zero = Lambda "s" (Lambda "z" (Var "z"))
--------------------------------------------------------------------------------

succ :: Term
succ = App(Lambda "m"(Lambda "s"(Lambda "z"(Var "s"))))(App(App(Var "m")(Var "s"))(Var "z")) => \m->\s->z->s(m s z)

succ = App(Lambda "m"(Lambda "s"(Lambda "z"(Var "s"))))(App(Var "m")(App(Var "s")(Var "z"))) => \m->\s->z->s(m s z)

succ = Lambda "m"(Lambda "s"(Lambda "z"(App (Var "s")(App(Var "m")(App(Var "s")(Var "z"))))))

eval (App t1 t2)         = eval (subst var t2 body)
 where (Lambda var body) = eval t1

subst v t (Var x)
  | v == x = t
  | otherwise = Var x

subst v t1 (Lambda x t2)
  | v == x = Lambda x t2
  | otherwise = Lambda x (subst v t1 t2)

subst v t1 (App t2 t3) = App (subst v t1 t2) (subst v t1 t3)

eval(App succ zero)
          -------------------------------------succ-----------------------------------    ---------------zero--------------
eval(App (Lam "m"(Lam "s"(Lam "z"(App (Var "s")(App(Var "m")(App(Var "s")(Var "z"))))))) (Lambda "s" (Lambda "z" (Var "z"))) )


App(eval(App(Lambda "m"(Lambda "s"(Lambda "z"(Var "s"))))(App(App(Var "m")(Var "s"))(Var "z"))))(Lambda "s"(Lambda"z"(Var "z")))

eval(App(Lambda "s"(Lambda "z"(Var "s")))(App(Lambda "s"(Lambda "z"(Var "s")))(App(Var "s")(Var "z")))) ->

eval(App(Lambda "s"(Lambda "z"(Var "s")))(App(Var "s")(Var "z"))) -


succ = \m.\s.\z. s(m s z)

succ C0 = \m.\s.\z.s(m s z) (\s.\z.z)
        = \s.\z.s(((\s.\z.z) s) z)
        = \s.\z.s((\z.z) z)
        = \s.\z.s z
