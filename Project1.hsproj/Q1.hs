{-# LANGUAGE GADTs #-}

module Q1 where

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

data AE where
  Number :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mul :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  deriving (Show,Eq)
  
pprint :: AE -> String
pprint (Number n) = show n
pprint (Plus x y) = "(" ++ pprint x ++ "+" ++ pprint y ++ ")"
pprint (Minus x y) = "(" ++ pprint x ++ "-" ++ pprint y ++ ")"
pprint (Mul x y) = "(" ++ pprint x ++ "*" ++ pprint y ++ ")"
pprint (Div x y) = "(" ++ pprint x ++ "/" ++ pprint y ++ ")"
  
--for part 2--

expr :: Parser AE
expr = buildExpressionParser operators term

numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Number (fromInteger i)) 
              
operators = [ [ inFix "*" Mul AssocLeft,
                inFix "/" Div AssocLeft,
                inFix "+" Plus AssocLeft,
                inFix "-" Minus AssocLeft]
            ]
            
term = parens lexer expr <|> numExpr

parseAE = parseString expr
parseAEFile = parseFile expr

--for part 3--

evalErr :: AE -> Either String AE
evalErr (Number n) = (Right(Number n))
evalErr (Plus x y) = let x1 = (evalErr x)
                         y1 = (evalErr y)
                     in case x1 of
                       (Left m) -> x1
                       (Right (Number v1)) -> case y1 of
                                              (Left m)-> y1
                                              (Right (Number v2)) -> (Right (Number (v1 + v2)))
                                              (Right _) -> (Left "Type Error in +")
                       (Right _) -> (Left "Type Error in +")
                  
evalErr (Minus x y) = let x1 = (evalErr x)
                          y1 = (evalErr y)
                     in case x1 of
                       (Left m) -> x1
                       (Right (Number v1)) -> case y1 of
                                              (Left m)-> y1
                                              (Right (Number v2)) -> (Right (Number (v1 - v2)))
                                              (Right _) -> (Left "Type Error in -")
                       (Right _) -> (Left "Type Error in -")
                   
evalErr (Mul x y) = let x1 = (evalErr x)
                        y1 = (evalErr y)
                     in case x1 of
                       (Left m) -> x1
                       (Right (Number v1)) -> case y1 of
                                              (Left m)-> y1
                                              (Right (Number v2)) -> (Right (Number (v1 * v2)))
                                              (Right _) -> (Left "Type Error in *")
                       (Right _) -> (Left "Type Error in *")
                  
evalErr (Div x y) = let x1 = (evalErr x)
                        y1 = (evalErr y)
                     in case x1 of
                       (Left m) -> x1
                       (Right (Number v1)) -> case y1 of
                                              (Left m)-> y1
                                              (Right (Number v2)) -> (Right (Number (div v1 v2)))
                                              (Right _) -> (Left "Type Error in /")
                       (Right _) -> (Left "Type Error in /")

--for part 4--
               
interp = evalErr . parseAE

-- Testing (Requires QuickCheck 2)

-- Arbitrary AST Generator

instance Arbitrary AE where
  arbitrary =
    sized $ \n -> genAE ((rem n 10) + 10)

genNum =
  do t <- choose (0,100)
     return (Number t)

genPlus n =
  do s <- genAE n
     t <- genAE n
     return (Plus s t)

genMinus n =
  do s <- genAE n 
     t <- genAE n
     return (Minus s t)
     
genMul n =
  do s <- genAE n
     t <- genAE n
     return (Mul s t)
     
genDiv n =
  do s <- genAE n
     t <- genAE n
     return (Div s t)

genAE :: Int -> Gen AE
genAE 0 =
  do term <- genNum
     return term
genAE n =
  do term <- oneof [genNum,(genPlus (n-1)),
                           (genMinus (n-1)),
                           (genMul (n-1)),
                           (genDiv (n-1))]
     return term

-- QuickCheck 

testParser :: Int -> IO ()
testParser n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> parseAE (pprint t) == t)

testevalErr' :: Int -> IO ()
testevalErr' n = quickCheckWith stdArgs {maxSuccess=n}
  (\t -> (interp $ pprint t) == (evalErr t))
