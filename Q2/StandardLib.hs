module StandardLib where

import Lambda

type Id = String

true :: Term
true  = Lambda "t"(Lambda "f"(Var "t"))

false :: Term
false = Lambda "t"(Lambda "f"(Var "f"))

zero :: Term
zero = Lambda "s" (Lambda "z" (Var "z"))

test :: Term
test = Lambda "l"(App(Lambda "m"(App(Lambda "n"(Var "l"))(Var "m")))(Var "n"))
--eval(App(App(App test true)(Var "v"))(Var "w"))

c1 :: Term
c1 = Lambda "s"(Lambda "z"(App(Var "s")(Var "z")))

c2 :: Term
c2 = Lambda "s"(Lambda "z"(App(Var "s")(App(Var "s")(Var "z"))))

c3 :: Term
c3 = Lambda "s"(Lambda "z"(App(Var "s")(App(Var "s")(App(Var "s")(Var "z")))))

c4 :: Term
c4 = Lambda "s"(Lambda "z"(App(Var "s")(App(Var "s")(App(Var "s")(App(Var "s")(Var "z"))))))

c5:: Term
c5 = Lambda "s"(Lambda "z"(App(Var "s")(App(Var "s")(App(Var "s")(App(Var "s")(App(Var "s")(Var "z")))))))

ands :: Term
ands = Lambda "b"(App(Lambda "c"(App(Var "b")(Var "c")))false)

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
--eval(App(App times c1)c2)

--iszro = λm. m (λx. fls) tru;
iszro :: Term
iszro = Lambda "m"(App(App(Var "m")(Lambda "b" false))true)

pair :: Term
pair = Lambda "f"(Lambda "s"(Lambda "b"(App(App(Var "b")(Var "f"))(Var "s"))))
--eval(App(App pair (Var "v"))(Var "w")) ==> \b -> bvw

first :: Term
first = Lambda "p"(App(Var "p")true)
--App first (App(App pair (Var "v"))(Var "w"))
--eval(App first (eval(App(App pair (Var "v"))(Var "w"))))

second :: Term
second = Lambda "p"(App(Var "p")false)

zz :: Term
zz = App(App pair zero) zero

-- ss = λp. pair (snd p) (plus c1 (snd p))
ss :: Term
ss = Lambda "p"(App(App pair(App second (Var "p")))(App(App plus c1)(App second (Var "p"))))

-- prd = λm. fst (m ss zz)
prd :: Term
prd = Lambda "m"(App first(App(App (Var "m") ss)zz))

--subt = λm. λn. n prd m
subt :: Term
subt = Lambda "m"(Lambda "n"(App(App(Var "n")prd)(Var "m")))
--eval(App(App subt c1)c1)

equals :: Term
equals = Lambda "m"(Lambda "n"(App ands(App(App iszro (App(App(Var "m")prd)(Var "n")))(App iszro (App(App(Var "n")prd)(Var "m"))))))

ite :: Term
ite = Lambda "p"(Lambda "a"(Lambda "b"(App(App(Var "p")(Var "a"))(Var "b"))))
