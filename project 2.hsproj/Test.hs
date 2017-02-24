--Name: Hao Luo--
--KUID: 2737588--
module Test where
  
import Project_2

testcase = ["if ((2 / 0) <= 4) then 1 else 2",               "If 3+4 then 3 else 2",               "3+3+0",               "3+2+5",               "If 3+5<=10 then 10-2 else 3*5",               "If 10<=8 then 6 else 5",
               "2/0<=4",
               "1+0+1",               "If 2+3<=5 then if True then 2 else 4 else 3",               "If True then If True then 2 else 5 else 10",
               "IsZero 3/0+1"
             ]

parserTest = (map parseABE testcase)
optimizeTest = (map optimize parserTest)
interpTest = (map interp testcase)
interpOpTest = (map interpOp testcase)


testingParser = mapM_ print parserTest
testingOptimize = mapM_ print optimizeTest
testingInterp = mapM_ print interpTest
testingInterpOp = mapM_ print interpOpTest

  
  
    