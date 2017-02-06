{-# LANGUAGE GADTs #-}

module Q2 where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import ParserUtils

-- Imports for QuickCheck
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

--for part 1--

data AEE where
  Number :: Int -> AEE
  Plus :: AEE -> AEE -> AEE
  Minus :: AEE -> AEE -> AEE
  Mul :: AEE -> AEE -> AEE
  Div :: AEE -> AEE -> AEE
  If0 :: AEE -> AEE -> AEE -> AEE
  deriving (Show,Eq)
  
pprint :: AEE -> String
pprint (Number n) = show n
pprint (Plus x y) = "(" ++ pprint x ++ "+" ++ pprint y ++ ")"
pprint (Minus x y) = "(" ++ pprint x ++ "-" ++ pprint y ++ ")"
pprint (Mul x y) = "(" ++ pprint x ++ "*" ++ pprint y ++ ")"
pprint (Div x y) = "(" ++ pprint x ++ "/" ++ pprint y ++ ")"
pprint (If0 x y z) = "(" ++ "If0" ++ pprint x ++ "then" ++ pprint y ++ "else" ++ pprint z ++ ")"
  
--for part 2--

expr :: Parser AEE
expr = buildExpressionParser operators term

numExpr :: Parser AEE
numExpr = do i <- integer lexer
             return (Number (fromInteger i))
             
ifExpr :: Parser AEE 
ifExpr = do reserved lexer "if0"
            a <- expr
            reserved lexer "then"
            b <- expr
            reserved lexer "else"
            c <- expr
            return (If0 a b c)
              
operators = [ [ inFix "*" Mul AssocLeft,
                inFix "/" Div AssocLeft,
                inFix "+" Plus AssocLeft,
                inFix "-" Minus AssocLeft]
            ]
            
term = parens lexer expr <|> numExpr <|> ifExpr

parseAEE = parseString expr
parseAEEFile = parseFile expr

--for part 3--

evalErr :: AEE -> AEE
evalErr (Number n) = (Number n)
evalErr (Plus x y) = let (Number x1) = evalErr x
                         (Number y1) = evalErr y
                      in (Number (x1 + y1))
                  
evalErr (Minus x y) = let (Number x1) = evalErr x
                          (Number y1) = evalErr y
                        in (Number (x1 - y1))
                   
evalErr (Mul x y) = let (Number x1) = evalErr x
                        (Number y1) = evalErr y
                  in (Number (x1 * y1))
                  
evalErr (Div x y) = let (Number x1) = evalErr x
                        (Number y1) = evalErr y
                  in (Number (div x1 y1))
                  
evalErr (If0 x y z) = let (Number x1) = evalErr x
                    in if (x1==0) then (evalErr y) else (evalErr z)
                    

--for part 4--
               
interp = evalErr . parseAEE

-- Testing (Requires QuickCheck 2)

-- Arbitrary AST Generator

instance Arbitrary AEE where
  arbitrary =
    sized $ \n -> genAEE ((rem n 10) + 10)

genNum =
  do t <- choose (0,100)
     return (Number t)

genPlus n =
  do s <- genAEE n
     t <- genAEE n
     return (Plus s t)

genMinus n =
  do s <- genAEE n
     t <- genAEE n
     return (Minus s t)
     
genMul n =
  do s <- genAEE n
     t <- genAEE n
     return (Mul s t)
     
genDiv n =
  do s <- genAEE n
     t <- genAEE n
     return (Div s t)
     
genIf0 n =
  do s <- genAEE n
     t <- genAEE n
     u <- genAEE n
     return (If0 s t u)

genAEE :: Int -> Gen AEE
genAEE 0 =
  do term <- genNum
     return term
genAEE n =
  do term <- oneof [genNum,
                    (genPlus (n-1)),
                    (genMinus (n-1)),
                    (genMul (n-1)),
                    (genDiv (n-1)),
                    (genIf0 (n-1))]
     return term

-- QuickCheck 

testParser :: Int -> IO ()
testParser n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> parseAEE (pprint t) == t)

testevalErr' :: Int -> IO ()
testevalErr' n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprint t) == (evalErr t))
