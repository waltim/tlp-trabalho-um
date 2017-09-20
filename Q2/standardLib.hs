module StandardLib where

import Lambda

type Id = String

true :: Term
true  = Lambda "t"(Lambda "f"(Var "t"))

false :: Term
false = Lambda "t"(Lambda "f"(Var "f"))

zero :: Term
zero = Lambda "s" (Lambda "z" (Var "z"))

c1 :: Term
c1 = Lambda "s"(Lambda "z"(App(Var "s")(Var "z")))

c2 :: Term
c2 = Lambda "s"(Lambda "z"(App(Var "s")(App(Var "s")(Var "z"))))

c3 :: Term
c3 = Lambda "s"(Lambda "z"(App(Var "s")(App(Var "s")(App(Var "s")(Var "z")))))

ands :: Term
ands = Lambda "b"(App(Lambda "c"(App(Var "b")(Var "c")))false)
--eval(App ands(App true true))
-- > \t->\f->t
--eval(App ands(App true false))
-- > \t->\f->f
--eval(App ands(App false true))
-- >  \t->\f->f
--eval(App ands(App false false))
-- > \t->\f->f
ors :: Term
ors = Lambda "b"(App(Lambda "c"(App(Var "b")true))(Var "b"))
--eval(App(App ors true)true)
-- > \t->\f->t

nots :: Term
nots = Lambda "b"(App(App(Var "b")false)true)
--------------------------------------------------------------------------------------------
scc :: Term
scc = Lambda "n"(Lambda "s"(Lambda "z"(App(Var "s")(App(App(Var "n")(Var "s"))(Var "z")))))

--plus = λm. λn. λs. λz. m s (n s z);
plus :: Term
plus = Lambda "m"(Lambda "n"(Lambda "s"(Lambda "z"(App(App(Var "m")(Var "s"))(App(App(Var "n")(Var "s"))(Var "z"))))))
--eval(App(App plus c3)c2)

--times = λm. λn. m (plus n) c0
times :: Term
times = Lambda "m"(Lambda "n"(App(App(Var "m")(App plus (Var "n")))zero))

--iszro = λm. m (λx. fls) tru;
iszro :: Term
iszro = Lambda "m"(App(App(Var "m")(Lambda "s" false))true)

pair :: Term
pair = Lambda "f"(Lambda "s"(Lambda "b"(App(App(Var "b")(Var "f"))(Var "s"))))
--Aplicação de "pair"
--App(App pair (Var "v"))(Var "w") ==> \f -> \s -> \b -> bfsvw
--eval(App(App pair (Var "v"))(Var "w")) ==> \b -> bvw

first :: Term
first = App(Lambda "p"(Var "p")) true
--App first (App(App pair (Var "v"))(Var "w"))
--eval(App first (eval(App(App pair (Var "v"))(Var "w"))))

second :: Term
second = App(Lambda "p"(Var "p")) false

zz :: Term
zz = App(App pair zero) zero

ss :: Term
ss = App(Lambda "p"(eval(App pair (eval(App second (Var "p"))) )))(App(eval(App plus c1))(eval(App second (Var "p"))))

--prd = λm. fst (m ss zz)
prd :: Term
prd = Lambda "m"(App first(App (Var "m")(App ss zz)))
--prd = Lambda "m"(App(App(App first (Var "m"))ss)zz)
--pred :: Term
--pred = undefined
