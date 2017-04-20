--Name: Hao Luo--
--KUID: 2737588--
module Test where

import Part_I

import Part_II

import Part_III

test = ["3 + 2 - 5 + 6 - 8 + 10",
        "(app (lambda n in (app (lambda f in (app (lambda n in (app f 3)) 1)) (lambda x in x + n))) 5)",
        "app (lambda x in if x + 10 then 5 else 789 / 3 + 6 * 2) 4"
             ]
             
elaboration = ["bind x=6 in bind f = lambda x in x+1 in x+2",
                "bind x=3 in x+123",
                "bind f = lambda x in x + 999 in app f 1",
                "dec 5",
                "app dec 5",
                "inc 3",
                "app inc 3",
                "app lambda x in x+10 2"
              ]

dynamicTest = (map interpDynCFAE test)
staticTest = (map interpStatCFAE test)
elaborateTest = (map interpCFBAE elaboration)

testingDy = mapM_ print dynamicTest
testingSt = mapM_ print staticTest
testingEl = mapM_ print elaborateTest