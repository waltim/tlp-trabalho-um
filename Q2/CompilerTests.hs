module CompilerTests where

import StandardLib as S
import Lambda as L
import RBL as R
import Compiler as C

import Test.HUnit

boolT = translate (R.B True)
boolF = translate (R.B False)

int0 = translate (R.N 0)
int3 = translate (R.N 3)

add2_3 = R.Add (R.N 2)(R.N 3)
sub5_1 = R.Sub (R.N 5)(R.N 1)

tadd2_3 = translate add2_3
tsub5_1 = translate sub5_1

andtt = translate (R.And (R.B True)(R.B True))
andtf = translate (R.And (R.B True)(R.B False))
andft = translate (R.And (R.B False)(R.B True))
andff = translate (R.And (R.B False)(R.B False))

ortt = translate (R.Or (R.B True)(R.B True))
ortf = translate (R.Or (R.B True)(R.B False))
orft = translate (R.Or (R.B False)(R.B True))
orff = translate (R.Or (R.B False)(R.B False))

nott = translate (R.Not (R.B True))
notf = translate (R.Not (R.B False))

eq3 = L.eval(L.App(L.App equals int3)c3)
eq5 = L.eval(L.App(L.App equals tadd2_3)c5)
eq4 = L.eval(L.App(L.App equals tsub5_1)c4)
test_for_wrong = L.eval(L.App(L.App equals tadd2_3)tsub5_1)

ite1 = translate (R.IfThenElse (R.B True) (R.B True) (R.B False))
ite2 = translate (R.IfThenElse (R.B False) (R.B True) (R.B False))
ite3 = translate (R.IfThenElse (R.B True) add2_3 sub5_1)
ite4 = translate (R.IfThenElse (R.B False) add2_3 sub5_1)

let1 = translate (R.Let "x" (R.N 4) (R.Add (R.Ref "x")(R.N 1)))
leteq = L.eval(L.App(L.App equals let1)c5)

lam = R.Lambda "x" (R.Add (R.Ref "x")(R.N 1))

tlam = translate lam

apl = translate (R.App lam (R.N 4))

test1 = TestCase (assertEqual "should be true" true (L.eval boolT))
test2 = TestCase (assertEqual "should be false" false (L.eval boolF))
test3 = TestCase (assertEqual "should be zero" zero (L.eval int0))
test4 = TestCase (assertEqual "should be 3" true eq3)
test5 = TestCase (assertEqual "Add 2 3" true eq5)
test6 = TestCase (assertEqual "Subt 5 1" true eq4)

test7 = TestCase (assertEqual "true && true" true andtt)
test8 = TestCase (assertEqual "true && false" false andtf)
test9 = TestCase (assertEqual "false && true" false andft)
test10 = TestCase (assertEqual "false && false" false andff)

test11 = TestCase (assertEqual "true || true" true ortt)
test12 = TestCase (assertEqual "true || false" true ortf)
test13 = TestCase (assertEqual "false || true" true orft)
test14 = TestCase (assertEqual "false || false" false orff)

test15 = TestCase (assertEqual "!true" false nott)
test16 = TestCase (assertEqual "!false" true notf)

test17 = TestCase (assertEqual "testing for wrong" false test_for_wrong)

test18 = TestCase (assertEqual "If (True) then True else False" true ite1)
test19 = TestCase (assertEqual "If (False) then True else False" false ite2)
test20 = TestCase (assertEqual "If (True) then add2_3 else sub5_1" tadd2_3 ite3)
test21 = TestCase (assertEqual "If (False) then add2_3 else sub5_1" tsub5_1 ite4)

test22 = TestCase (assertEqual "let x=4 in x+1" true leteq)

test23 = TestCase (assertEqual "(\\x->x+1)4" let1 apl)


allTests = TestList [ test1
                      , test2
                      , test3
                      , test4
                      , test5
                      , test6
                      , test7
                      , test8
                      , test9
                      , test10
                      , test11
                      , test12
                      , test13
                      , test14
                      , test15
                      , test16
                      , test17
                      , test18
                      , test19
                      , test20
                      , test21
                      , test22
                      , test23
                      ]

runAll = runTestTT allTests
