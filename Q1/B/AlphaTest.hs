module AlphaTest where 

import AlphaConversion
import Test.HUnit
import Data.Bool

-- \x.x
exp1 = Lambda 'x' (Var 'x')

-- \y.y
exp2 = Lambda 'y' (Var 'y')

-- \x.\y.\z.xz(yz)
exp3 = Lambda 'x' $ Lambda 'y' $ Lambda 'z' $ Var 'x' `App` Var 'z' `App` (Var 'y' `App` Var 'z')

-- \a.\b.\c.ac(bc)
exp4 = Lambda 'a' $ Lambda 'b' $ Lambda 'c' $ Var 'a' `App` Var 'c' `App` (Var 'b' `App` Var 'c')

-- \a.\b.\c.ab(ac)
exp5 = Lambda 'a' $ Lambda 'b' $ Lambda 'c' $ Var 'a' `App` Var 'b' `App` (Var 'a' `App` Var 'c')

-- \x.\y.xy
exp6 = Lambda 'x' $ Lambda 'y' $ (Var 'x' `App` Var 'y')

-- \a.\b.ab
exp7 = Lambda 'a' $ Lambda 'b' $ (Var 'a' `App` Var 'b')

-- \a.\b.abc
exp8 = Lambda 'a' $ Lambda 'b' $ (Var 'a' `App` Var 'b' `App` Var 'c')

-- \a.\a.aaz
exp9 = Lambda 'a' $ Lambda 'a' $ (Var 'a' `App` Var 'a' `App` Var 'z')

-- Test Cases
test1 = TestCase (assertEqual "exp1 alphaEquivalence exp2"  (exp1 `alphaEquivalence` exp2) (True))
test2 = TestCase (assertEqual "exp3 alphaEquivalence exp4"  (exp3 `alphaEquivalence` exp4) (True))
test3 = TestCase (assertEqual "exp3 alphaEquivalence exp5"  (exp3 `alphaEquivalence` exp5) (False))
test4 = TestCase (assertEqual "exp6 alphaEquivalence exp7"  (exp6 `alphaEquivalence` exp7) (True))
test5 = TestCase (assertEqual "exp6 alphaEquivalence exp8"  (exp6 `alphaEquivalence` exp8) (False))
test6 = TestCase (assertEqual "exp6 alphaEquivalence exp9"  (exp6 `alphaEquivalence` exp9) (True)) -- False 

tests = TestList [TestLabel "test1" test1,
    TestLabel "test2" test2,
    TestLabel "test3" test3,
    TestLabel "test4" test4,
    TestLabel "test5" test5,
    TestLabel "test6" test6]

-- To run all tests
runAlphaTests = runTestTT tests