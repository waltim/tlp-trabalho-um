Class notes

C0 = \s.\z.z
C1 = \s.\z.s z
C2 = \s.\z.s(s z)
... = ...

tru  = Lambda "t" (Lambda "f"(Var "t"))
fls  = Lambda "t" (Lambda "f" (Var "f"))
test = Lambda "l"(App(Lambda "m"(App(Lambda "n"(Var "l"))(Var "m")))(Var "n"))

eval(App(App(App test true)(Var "v"))(Var "w"))

App(Lambda "s"(Lambda "z"(Var "s")))(Var "z")

App(Lambda "x"(App(Lambda "y"(Var "x"))(Var "y")))(Var "x")

eval(App(App first (Var "v"))(Var "w")) ==> v
eval(App(App second (Var "v"))(Var "w")) ==> w

pp = App(Lambda "p"(App(Var "p")true))(Lambda "b"(App(App(Var "b")(Var "v"))(Var "w"))) ==> \p -> p\t -> \f -> t\b -> bvw
eval pp ==> v

tt = App(Lambda "b"(App(App(Var "b")(Var "v"))(Var "w")))true ==> \b -> bvw\t -> \f -> t
eval tt ==> v

[x -> s]x          = s
[x -> s]y          = y                          if y ≠ x
[x -> s](λy.t1)    = (λy. t1                    if y = x
                   =  λy. [x -> s]t1             if y ≠ x and y ∉ FV(s)
[x -> s](t1 t2)    = ([x -> s]t1 ([x -> s]t2)


(eval(App(App pair (Var "v"))(Var "w")))
