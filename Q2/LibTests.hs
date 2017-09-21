module LibTests where

import StandardLib
import Lambda

import Test.HUnit

sc1 = eval(App scc zero)
sc2 = eval(App scc sc1)
sc3 = eval(App scc sc2)
sc4 = eval(App scc sc3)
sc5 = eval(App scc sc4)
sc6 = eval(App scc sc5)

sc3is0 = eval(App iszro sc3)
sc2is0 = eval(App iszro sc2)
sc1is0 = eval(App iszro sc1)

pr2 = eval(App prd sc3)
pr1 = eval(App prd pr2)
pr0 = eval(App prd pr1)

pl2 = eval(App(App plus c1)sc1)
sub2 = eval(App(App subt c4)pl2)

ti0 = eval(App(App times zero)pl2)
ti4 = eval(App(App times pr2)c2)
ti6 = eval(App(App times c2)c3)

pr2is0 = eval(App iszro pr2)
pr1is0 = eval(App iszro pr1)
pr0is0 = eval(App iszro pr0)

zeroiszero = eval(App(App equals zero)zero)
c1isc1 = eval(App(App equals c1)c1)
c2isc2 = eval(App(App equals c2)c2)
sc1isc1 = eval(App(App equals sc1)c1)
sc1_pr2 = eval(App(App equals sc1)pr2)
pr2_sc1 = eval(App(App equals pr2)sc1)

pl2isc2 = eval(App(App equals pl2)c2)
sub2issc2 = eval(App(App equals sub2)sc2)
sub2_sc1 = eval(App(App equals sub2)sc1)
sc1_sub2 = eval(App(App equals sc1)sub2)
check0 = eval(App(App equals ti0)pr0)
check4 = eval(App(App equals ti4) sc4)
check6 = eval(App(App equals ti6) sc6)

test1 = TestCase (assertEqual "evaluate sc1" (false) (sc1is0))
test2 = TestCase (assertEqual "evaluate pr1" (false) (pr1is0))
test3 = TestCase (assertEqual "evaluate pr0" (true) (pr0is0))

test4 = TestCase (assertEqual "zero equals zero" (true) (zeroiszero))
test5 = TestCase (assertEqual "c1 equals c1" (true) (c1isc1))
test6 = TestCase (assertEqual "c2 equals c2" (true) (c2isc2))
test7 = TestCase (assertEqual "sc1 equals c1" (true) (sc1isc1))

test8 = TestCase (assertEqual "sc1 differs pr2" (false) (sc1_pr2))
test9 = TestCase (assertEqual "pr2 differs sc1" (false) (pr2_sc1))

--Tests with 'plus' and 'subt'
test10 = TestCase (assertEqual "c1 plus sc1 equals c2" (true) (pl2isc2))
test11 = TestCase (assertEqual "c4 subtract pl2 equals sc2" (true) (sub2issc2))
test12 = TestCase (assertEqual "sub2 differs sc1" (false) (sub2_sc1))
test13 = TestCase (assertEqual "sc1 differs sub2" (false) (sc1_sub2))

--Test with 'times'
test14 = TestCase (assertEqual "ti0 equals pr0" (true) (check0))
test15 = TestCase (assertEqual "ti4 equals sc4" (true) (check4))
test16 = TestCase (assertEqual "t16 equals sc6" (true) (check6))

--Tests with Booleans
att = eval(App ands(App true true))
atf = eval(App ands(App true false))
aft = eval(App ands(App false true))
aff = eval(App ands(App false false))

ott = eval(App(App ors true)true)
otf = eval(App(App ors true)false)
oft = eval(App(App ors false)true)
off = eval(App(App ors false)false)

nt = eval(App nots true)
nf = eval(App nots false)

ite1 = eval(App(App(App ite nf)true)false)
ite2 = eval(App(App(App ite nt)true)false)
ite3 = eval(App(App(App ite nf)c1)c2)
ite4 = eval(App(App(App ite nt)c1)c2)

test17 = TestCase (assertEqual "true && true" (true) att)
test18 = TestCase (assertEqual "true && false" (false) atf)
test19 = TestCase (assertEqual "false && true" (false) aft)
test20 = TestCase (assertEqual "false && false" (false) aff)

test21 = TestCase (assertEqual "true || true" (true) ott)
test22 = TestCase (assertEqual "true || false" (true) otf)
test23 = TestCase (assertEqual "false || true" (true) oft)
test24 = TestCase (assertEqual "false || false" (false) off)

test25 = TestCase (assertEqual "!true" (false) nt)
test26 = TestCase (assertEqual "!false" (true) nf)

test27 = TestCase (assertEqual "if !false then true else false" true ite1)
test28 = TestCase (assertEqual "if !true then true else false" false ite2)
test29 = TestCase (assertEqual "if !false then c1 else c2" c1 ite3)
test30 = TestCase (assertEqual "if !true then c1 else c2" c2 ite4)

zeroTests = TestList [ test1
                      , test2
                      , test3
                      ]

runZeros = runTestTT zeroTests

equalTests = TestList [ test4
                        , test5
                        , test6
                        , test7
                        , test8
                        , test9
                        ]
runEquals = runTestTT equalTests

opTests = TestList [ test10
                     , test11
                     , test12
                     , test13
                     , test14
                     , test15
                     , test16
                     ]

runOp = runTestTT opTests

boolTests = TestList [ test17
                      , test18
                      , test19
                      , test20
                      , test21
                      , test22
                      , test23
                      , test24
                      , test25
                      , test26
                      ]

runBools = runTestTT boolTests

iteTests = TestList [ test27
                    , test28
                    , test29
                    , test30
                    ]

runItes = runTestTT iteTests
