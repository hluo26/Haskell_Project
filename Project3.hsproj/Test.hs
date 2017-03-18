module Test where

import Project3

testCases = ["0 - 1 + 0 + 1 - 0 + 1", 
             "isZero 0",
             "if 2 + (1-1) <= 7 then false else true",
             "if isZero 0 - 3 then if 3 + 5 <= 8 && false then 5 else 7 else 2",
             "bind x = 1 in 2 + x",
             "bind x = 1 in 2 + y",
             "bind a = true in if a then 1 else 2",
             "bind a = 1 in if a then 1 else 2",
             "seq 1+2 2+3",
             "print 1+2+3+4"
             ]
listTestCases = ["first (cons 1 empty)",
                 "rest (cons 1 (cons 2 empty))",
                 "isEmpty empty", 
                 "isEmpty cons 1 empty"]

runParserTest = (map parseBBAE testCases)
runInterpsTest = (map interps testCases)
runInterpTest = (map interp testCases)
runListTest = (map interpEx listTestCases)

testParser = mapM_ print runParserTest
testSubst = mapM_ print runInterpsTest
testEnv = mapM_ print runInterpTest
testList = mapM_ print runListTest