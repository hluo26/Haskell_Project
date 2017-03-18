--Name: Hao Luo--
--KUID: 2737588--
module Test where

import Part1

import Part2

test = ["3 + 2 - 5 + 6 - 8 + 10", 
             "if 5 + (4-4) <= 7 then false else true",
             "isZero 0",
             "if isZero 0 - 3 then if seq(3 + 5)(2 + 2) <= 8 && false then 5 else 7 else 2",
             "seq (1 + 2) (2 + 3)",
             "print 1 + 5 - 2 + 6",
             "bind x = 9 in 4 + x",
             "bind x = 10 in 1 - y",
             "bind a = false in if a then 1 + 4 else 8 - 2",
             "bind a = 12 in if a then true else isZero 5",
             "seq (True && 3 - 2)(print 10)"
             ]
             
list = ["first (cons 1 empty)",
                 "rest (cons 1 (cons 2 empty))",
                 "isEmpty empty", 
                 "isEmpty cons 1 empty",
                 "first (cons 1+2 2<=5)",
                 "rest (cons isZero 5 (cons seq(5-3)(True&&False) empty))",
                 "cons if 3<=4 then 19 else false 6-3"]

parserTest = (map parseBBAE test)
substTest = (map interps test)
envTest = (map interp test)
listTest = (map interpEx list)

testingParser = mapM_ print parserTest
testingSubst = mapM_ print substTest
testingEnv = mapM_ print envTest
testingList = mapM_ print listTest