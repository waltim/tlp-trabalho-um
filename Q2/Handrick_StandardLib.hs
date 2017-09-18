module StandardLib where

import Lambda

true :: Term

true = Lambda "t" (Lambda "f" (Var "t"))

zero = Lambda "s" (Lambda "z" (Var "z")) 
one = App (Lambda "s" (Lambda "z" (Var "s"))) (Var "z")
two = App (Lambda "s" (Lambda "z" (Var "s"))) (App (Var "s") (Var "z"))



finalSuc = App ((App (Var "m") (Var "s")))(Var "z") -- Final do succ
suc = Lambda "m" ((Lambda "s" (App (Lambda "z" (Var "s"))(finalSuc))))


oneWithSucc = App (suc) (zero)
