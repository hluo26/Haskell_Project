module Test where

import Project5

mycases = ["bind n = 1 in bind f = (lambda (x:Nat) in x + n) in bind n = 10 in app f 1",
            -- binding 1 + 1 = 2
               "app(lambda (x:Nat) in x+1)4",
               -- app 4 + 1 = 5
               "lambda (x:Nat) in x+1",
               -- lambda
               "bind n = 5 in bind f = (lambda (x:Nat) in x+n) in bind n = 1 in app f 3",
               -- app 5 + 3 = 8
               "bind fact = (lambda (g:Nat) in (lambda (x:Nat) in (if (isZero x) then 1 else x * (app g x-1)))) in bind factorial = (fix fact) in (app factorial 3)",
               -- Recursion 3 * 2 * 1 = 6
               "app (fix (lambda (ie:Nat->Nat) in (lambda (x:Nat) in if (isZero x) then x else x + app ie x - 1))) 5",
               -- Recursion 5 + 4 + 3 + 2 + 1 = 15
                    "(app (lambda (n:Nat) in (app (lambda (f:Nat) in (app (lambda (n:Nat) in (app f 3)) 1)) (lambda (x:Nat) in x + n))) 5)"
                    -- Function in function = 8                    
                    ]
                    

invoke str = typeof [] (parseFBAE str)

testTypeof = mapM_ print (map invoke mycases)
testInterp = mapM_ print (map interp mycases)