{- 
Author: Asger Pedersen and Kristoffer Cobley
-}

import Test.HUnit
import Data.Either as E
import MSM
-- tests cjmp
pCjmp = [PUSH 1,PUSH (-1), CJMP 5,PUSH 4, ADD, DUP, NEG, HALT]

-- lots of elements on the stack
pLong = [PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,PUSH 1,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD,ADD, HALT]

-- example program from the forum leaves 1008 on the stack
pFourtyTwo = [ NEWREG 0,NEWREG 1,PUSH 0,PUSH 42,STORE,PUSH 1,PUSH 24,STORE,PUSH 0,PUSH 0,PUSH 1,LOAD,NEG,ADD, CJMP 17, PUSH 29,JMP,PUSH 0,LOAD,ADD,PUSH 1,PUSH 1,LOAD,PUSH 1,NEG,ADD,STORE,PUSH 9,JMP,HALT]

-- example program from the forum leaves 0 and 2 on the stack
pTenDivFive =  [ NEWREG 0,NEWREG 1,PUSH 0,PUSH 10,STORE,PUSH 1,PUSH 5,STORE,PUSH 0,PUSH 0,LOAD,PUSH 0,NEG,ADD,PUSH 1,ADD,NEG,CJMP 20,PUSH 32,JMP,PUSH 0, PUSH 0,LOAD,PUSH 1,LOAD,NEG,ADD,STORE,PUSH 1,ADD,PUSH 9,JMP,PUSH (-1),ADD,PUSH 0,LOAD,PUSH 1,LOAD,ADD,HALT]

-- example program, when it terminates it leaves 42 on the top of the stack
p42 =  [NEWREG 0, PUSH 1, DUP, NEG, ADD, PUSH 40, STORE, PUSH 2, PUSH 0, LOAD, ADD, HALT]

-- check error on [] (POP, DUP, LOAD, NEG, JMP, CJMP i) 
pEmpty0 =  [POP, HALT]
pEmpty1 =  [DUP, HALT]
pEmpty2 =  [LOAD, HALT]
pEmpty3 =  [NEG, HALT]
pEmpty4 =  [JMP, HALT]
pEmpty5 =  [CJMP 1, HALT]

-- check error on <2 elem (ADD, STORE, SWAP)
lt2elems0 =  [PUSH 1, ADD, HALT]
lt2elems1 =  [PUSH 1, STORE, HALT]
lt2elems2 =  [PUSH 1, SWAP, HALT]

-- check error on reg not allocated (LOAD, STORE)
noReg0 =  [PUSH 1, LOAD, HALT]
noReg1 =  [PUSH 1, PUSH 1, STORE, HALT]

-- check error on already allocated (NEWREG a)
allocSame =  [NEWREG 1, NEWREG 1, HALT]


-- PC points outside of program (JMP, CJMP)
outsideProg0 =  [PUSH 10, JMP, HALT]
outsideProg1 =  [PUSH (-10), JMP, HALT]
outsideProg2 =  [PUSH (-10), CJMP 10, HALT]
outsideProg3 =  [PUSH (-10), CJMP (-10), HALT]

-- Sub
-- When it terminates it leaves 1 on top of the stack
pSub1 =  [PUSH 2, PUSH 1, SUB, HALT]
-- Fails because there is only 1 element in the stack
pSubFail1 =  [PUSH 4, SUB, HALT]

-- Add
-- when it terminates it leaves 6 on top of the stack
pAdd1 =  [PUSH 3, PUSH 2, PUSH 1, ADD,ADD, HALT]
-- Fails because there is only 1 element in the stack
pAddFail1 = [PUSH 5, ADD, HALT]

-- Mult
-- when it terminates it leaves 42 on top of the stack
pMult1 =  [PUSH 3, PUSH 2, PUSH 7, MULT,MULT, HALT]
-- Fails because there is only 1 element in the stack
pMultFail1 = [PUSH 5, MULT, HALT]

-- example program from the forum, leaves 6 on the stack
pie =  [ NEWREG 0,NEWREG 1,NEWREG 2,NEWREG 3,PUSH 0,PUSH 28,STORE,PUSH 1,PUSH 1,STORE,PUSH 0,PUSH 0,LOAD,PUSH 1,LOAD,NEG,ADD,PUSH 1,ADD,NEG, CJMP 23,PUSH 80,JMP,PUSH 0,PUSH 2,PUSH 0,LOAD,STORE,PUSH 3,PUSH 1,LOAD,STORE,PUSH 0,PUSH 2,LOAD,PUSH 0,NEG,ADD,PUSH 1,ADD,NEG,CJMP 44, PUSH 56,JMP,PUSH 2,PUSH 2,LOAD,PUSH 3,LOAD,NEG,ADD,STORE,PUSH 1,ADD,PUSH 33,JMP,PUSH (-1),ADD,PUSH 2,LOAD,PUSH 3,LOAD,ADD,SWAP,POP, NEG,ADD,CJMP 72,PUSH 1,ADD,PUSH 72,JMP,PUSH 1,PUSH 1,LOAD,PUSH 1,ADD,STORE,PUSH 11,JMP,HALT]


-- testcases begin here!
testP42 = TestCase $ assertBool "P42" $ 42 `elem` stack (head(E.rights [MSM.runMSM p42])) 

testPie = TestCase $ assertBool "Pie" $ 6 `elem` stack (head(E.rights [MSM.runMSM pie])) 

testCjmp = TestCase $ assertBool "Cjmp" $ (-1) `elem` stack (head(E.rights [MSM.runMSM pCjmp])) 

testLong = TestCase $ assertBool "Long stack" $ 25 `elem` stack (head(E.rights [MSM.runMSM pLong]))  

testTenDivFive = TestCase $ assertBool "Ten div five" $ [0,2] == stack (head(E.rights [MSM.runMSM pTenDivFive])) 

testFourtyTwo = TestCase $ assertBool " 42*24" $ 1008 `elem` stack (head(E.rights [MSM.runMSM pFourtyTwo])) 


testEmpty0 = TestCase $ assertBool "Empty stack POP" $ "Stack is empty" ==  head(E.lefts [MSM.runMSM pEmpty0])

testEmpty1 =  TestCase $ assertBool "Empty stack DUP" $ "Stack is empty" ==  head(E.lefts [MSM.runMSM pEmpty1])

testEmpty2 =  TestCase $ assertBool "Empty stack LOAD" $ "Stack is empty" ==  head(E.lefts [MSM.runMSM pEmpty2])

testEmpty3 =  TestCase $ assertBool "Empty stack NEG" $ "Stack is empty" ==  head(E.lefts [MSM.runMSM pEmpty4])

testEmpty4 =  TestCase $ assertBool "Empty stack JMP" $ "Stack is empty" ==  head(E.lefts [MSM.runMSM pEmpty4])

testEmpty5 =  TestCase $ assertBool "Empty stack CJMP" $ "Stack is empty" ==  head(E.lefts [MSM.runMSM pEmpty5])

testlt2elems0 = TestCase $ assertBool "less than 2 elem on stack ADD" $ "Not enough variables on stack for ADD operation" ==  head(E.lefts [MSM.runMSM lt2elems0])

testlt2elems1 = TestCase $ assertBool "less than 2 elem on stack STORE" $ "Not enough variables on stack for STORE operation" ==  head(E.lefts [MSM.runMSM lt2elems1])

testlt2elems2 = TestCase $ assertBool "less than 2 elem on stack SWAP" $ "Not enough variables on stack for SWAP operation" == head(E.lefts [MSM.runMSM lt2elems2])

testnoReg0 = TestCase $ assertBool "register not allocated 0" $ "register 1 not allocated" ==  head(E.lefts [MSM.runMSM noReg0])

testnoReg1 = TestCase $ assertBool "register not allocated 1" $ "register 1 not allocated" ==  head(E.lefts [MSM.runMSM noReg1])

testallocSame = TestCase $ assertBool "register already allocated" $ "register 1 already allocated" ==  head(E.lefts [MSM.runMSM allocSame])

testoutsideProg0 = TestCase $ assertBool "PC out of bounds 0" $ "PC out of bounds" ==  head(E.lefts [MSM.runMSM outsideProg0])

testoutsideProg1 = TestCase $ assertBool "PC out of bounds 1" $ "PC out of bounds" ==  head(E.lefts [MSM.runMSM outsideProg1])

testoutsideProg2 = TestCase $ assertBool "PC out of bounds 2" $ "PC out of bounds" ==  head(E.lefts [MSM.runMSM outsideProg2])

testoutsideProg3 = TestCase $ assertBool "PC out of bounds 3" $ "PC out of bounds" ==  head(E.lefts [MSM.runMSM outsideProg3])

testpSub1 = TestCase $ assertBool "Sub leaves -1 on stack" $ (-1) `elem` stack (head(E.rights [MSM.runMSM pSub1])) 

testpSubFail1 = TestCase $ assertBool "Sub fail" $ "Not enough variables on stack for SUB operation" ==  head(E.lefts [MSM.runMSM pSubFail1])

testpAdd1 = TestCase $ assertBool "Add leaves 6 on stack" $ 6 `elem` stack (head(E.rights [MSM.runMSM pAdd1]))

testpAddFail1 = TestCase $ assertBool "Add fail" $ "Not enough variables on stack for ADD operation" ==  head(E.lefts [MSM.runMSM pAddFail1])

testpMult1 = TestCase $ assertBool "Mult leaves 42 on stack" $ 42 `elem` stack (head(E.rights [MSM.runMSM pMult1])) 

testpMultFail1 = TestCase $ assertBool "Add fail" $ "Not enough variables on stack for MULT operation" ==  head(E.lefts [MSM.runMSM pMultFail1])

tests = TestList [TestLabel "MSM testsuite" $ TestList [testP42,testPie,testCjmp,testLong,testTenDivFive,testFourtyTwo,testEmpty0,testEmpty1,testEmpty2,testEmpty3,testEmpty4,testEmpty5,testlt2elems0,testlt2elems1,testlt2elems2, testnoReg0, testnoReg1, testallocSame,testoutsideProg0,testoutsideProg1,testoutsideProg2,testoutsideProg3, testpSub1, testpSubFail1,testpAdd1,testpAddFail1,testpMult1,testpMultFail1]]
-- =================MAIN========================
main = runTestTT tests