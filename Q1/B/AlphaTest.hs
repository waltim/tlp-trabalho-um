module AlphaTest where 

import AlphaConversion
import Test.HUnit
import Data.Bool

-- \x.x
exp1 = Lambda "x" (Var "x")

-- \y.y
exp2 = Lambda "y" (Var "y")

-- \x.\y.\z.xz(yz)
exp3 = Lambda "x" $ Lambda "y" $ Lambda "z" $ Var "x" `App` Var "z" `App` (Var "y" `App` Var "z")

-- \a.\b.\c.ac(bc)
exp4 = Lambda "a" $ Lambda "b" $ Lambda "c" $ Var "a" `App` Var "c" `App` (Var "b" `App` Var "c")

-- \a.\b.\c.ab(ac)
exp5 = Lambda "a" $ Lambda "b" $ Lambda "c" $ Var "a" `App` Var "b" `App` (Var "a" `App` Var "c")

-- \x.\y.xy
exp6 = Lambda "x" $ Lambda "y" $ (Var "x" `App` Var "y")

-- \a.\b.ab
exp7 = Lambda "a" $ Lambda "b" $ (Var "a" `App` Var "b")

-- \a.\b.abc
exp8 = Lambda "a" $ Lambda "b" $ (Var "a" `App` Var "b" `App` Var "c")


-- Test cases for alpha equivalence
testEquivalence1 = TestCase (assertEqual "exp1 alphaEquivalence exp2"  (exp1 `alphaEquivalence` exp2) (True))
testEquivalence2 = TestCase (assertEqual "exp3 alphaEquivalence exp4"  (exp3 `alphaEquivalence` exp4) (True))
testEquivalence3 = TestCase (assertEqual "exp3 alphaEquivalence exp5"  (exp3 `alphaEquivalence` exp5) (False))
testEquivalence4 = TestCase (assertEqual "exp6 alphaEquivalence exp7"  (exp6 `alphaEquivalence` exp7) (True))
testEquivalence5 = TestCase (assertEqual "exp6 alphaEquivalence exp8"  (exp6 `alphaEquivalence` exp8) (False))

testsEquivalence = TestList [TestLabel "testEquivalence1" testEquivalence1,
    TestLabel "testEquivalence2" testEquivalence2,
    TestLabel "testEquivalence3" testEquivalence3,
    TestLabel "testEquivalence4" testEquivalence4,
    TestLabel "testEquivalence5" testEquivalence5]

-- To run all tests for alpha equivalence
runEquivalenceTests = runTestTT testsEquivalence

-- Test cases for alpha conversion
testConversion1 = TestCase (assertEqual "alphaConversion exp1 [('x','y')]" (alphaConversion exp1 [("x","y")]) (exp2))
testConversion2 = TestCase (assertEqual "alphaConversion exp4 [('x', 'a'), ('y','b'), ('z','c')]" (alphaConversion exp3 [("x", "a"), ("y","b"), ("z","c")]) (exp4))
testConversion3 = TestCase (assertEqual "alphaConversion exp6 [('x', 'a'), ('y','b')]" (alphaConversion exp6 [("x", "a"), ("y","b")]) (exp7))

testsConversion = TestList [TestLabel "testConversion1" testConversion1,
    TestLabel "testConversion2" testConversion2,
    TestLabel "testConversion3" testConversion3]

-- To run all tests for alpha conversion
runConversionTests = runTestTT testsConversion
