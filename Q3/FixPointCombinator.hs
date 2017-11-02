module FixPointCombinator where
	
churchFac = \n ->
        App
            (App
                (App churchITE (App isZero n))
                    (App successor zeroc))
                        (App (App mult (churchFac (App predecessor n))) n)

pureChurchFac = Lambda "f" $ Lambda "n" $
        App
            (App
                (App churchITE (App isZero (Var "n")))
                    (App successor zeroc))
                        (App (App mult (App (Var "f") (App predecessor (Var "n")))) (Var "n") )

fix = Lambda "f" $ App (Lambda "x" $ App (Var "f") (Lambda "y" (App (App (Var "x") (Var "x")) (Var "y"))))
                       (Lambda "x" $ App (Var "f") (Lambda "y" (App (App (Var "x") (Var "x")) (Var "y"))))

ycomb = Lambda "p" $ App (Lambda "f" $ App (Var "p") (App (Var "f") (Var "f"))) (Lambda "f" $ App (Var "p") (App (Var "f") (Var "f")))