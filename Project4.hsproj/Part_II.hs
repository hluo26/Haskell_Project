--Name: Hao Luo--
--KUID: 2737588--
{-# LANGUAGE GADTs #-}

module Part_II where

-- Imports for QuickCheck
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Function
import Test.QuickCheck.Monadic

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

-- Imports for PLIH
import ParserUtils

--
-- Project utilities for developing CFAE and CFBAE
-- interpreters.
--
-- Author: Perry Alexander
-- Date: 6 April 2017
--

-- CFAE AST Definition

data CFAE where
  Num :: Int -> CFAE
  Plus :: CFAE -> CFAE -> CFAE
  Minus :: CFAE -> CFAE -> CFAE
  Mult :: CFAE -> CFAE -> CFAE
  Div :: CFAE -> CFAE -> CFAE
  Lambda :: String -> CFAE -> CFAE
  Closure :: String -> CFAE -> Env -> CFAE
  App :: CFAE -> CFAE -> CFAE
  Id :: String -> CFAE
  If :: CFAE -> CFAE -> CFAE -> CFAE
  deriving (Show,Eq)

-- Parser

expr :: Parser CFAE
expr = buildExpressionParser opTable term

opTable = [ [ inFix "*" Mult AssocLeft
            , inFix "/" Div AssocLeft ]
          , [ inFix "+" Plus AssocLeft
            , inFix "-" Minus AssocLeft ]
          ]

numExpr :: Parser CFAE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

identExpr :: Parser CFAE
identExpr = do i <- identifier lexer
               return (Id i)
              
lambdaExpr :: Parser CFAE
lambdaExpr = do reserved lexer "lambda"
                i <- identifier lexer
                reserved lexer "in"
                e <- expr
                return (Lambda i e)

appExpr :: Parser CFAE
appExpr = do reserved lexer "app"
             e1 <- expr
             e2 <- expr
             return (App e1 e2)

ifExpr :: Parser CFAE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)
            
             
term = parens lexer expr
       <|> numExpr
       <|> identExpr
       <|> ifExpr
       <|> lambdaExpr
       <|> appExpr
             
-- Parser invocation

parseCFAE = parseString expr

parseCFAEFile = parseFile expr


-- CFBAE Parser

-- CFBAE AST Definition

data CFBAE where
  NumX :: Int -> CFBAE
  PlusX :: CFBAE -> CFBAE -> CFBAE
  MinusX :: CFBAE -> CFBAE -> CFBAE
  MultX :: CFBAE -> CFBAE -> CFBAE
  DivX :: CFBAE -> CFBAE -> CFBAE
  BindX :: String -> CFBAE -> CFBAE -> CFBAE
  LambdaX :: String -> CFBAE -> CFBAE
  AppX :: CFBAE -> CFBAE -> CFBAE
  IdX :: String -> CFBAE
  IfX :: CFBAE -> CFBAE -> CFBAE -> CFBAE
  deriving (Show,Eq)

-- Parser

exprX :: Parser CFBAE
exprX = buildExpressionParser opTableX termX

opTableX = [ [ inFix "*" MultX AssocLeft
            , inFix "/" DivX AssocLeft ]
          , [ inFix "+" PlusX AssocLeft
            , inFix "-" MinusX AssocLeft ]
          ]

numExprX :: Parser CFBAE
numExprX = do i <- integer lexer
              return (NumX (fromInteger i))

identExprX :: Parser CFBAE
identExprX = do i <- identifier lexer
                return (IdX i)

bindExprX :: Parser CFBAE
bindExprX = do reserved lexer "bind"
               i <- identifier lexer
               reservedOp lexer "="
               v <- exprX
               reserved lexer "in"
               e <- exprX
               return (BindX i v e)
              
lambdaExprX :: Parser CFBAE
lambdaExprX = do reserved lexer "lambda"
                 i <- identifier lexer
                 reserved lexer "in"
                 e <- exprX
                 return (LambdaX i e)

appExprX :: Parser CFBAE
appExprX = do reserved lexer "app"
              e1 <- exprX
              e2 <- exprX
              return (AppX e1 e2)

ifExprX :: Parser CFBAE
ifExprX = do reserved lexer "if"
             c <- exprX
             reserved lexer "then"
             t <- exprX
             reserved lexer "else"
             e <- exprX
             return (IfX c t e)
            
             
termX = parens lexer exprX
       <|> numExprX
       <|> identExprX
       <|> bindExprX
       <|> ifExprX
       <|> lambdaExprX
       <|> appExprX
             
-- Parser invocation

parseCFBAE = parseString exprX

parseCFBAEFile = parseFile exprX


data CFAEVal where
  NumV :: Int -> CFAEVal
  ClosureV :: String -> CFAE -> Env -> CFAEVal
  LambdaV :: String -> CFAE -> CFAEVal
  deriving (Show,Eq)
  
-- Q2 --
type Env = [(String,CFAEVal)]

evalStatCFBE :: Env -> CFAE -> CFAEVal

evalStatCFBE env (Num x) = (NumV x)

evalStatCFBE env (Plus x y) = let (NumV t1) = (evalStatCFBE env x)
                                  (NumV t2) = (evalStatCFBE env y)
                              in (NumV(t1+t2))
                              
evalStatCFBE env (Minus x y) = let (NumV t1)= (evalStatCFBE env x)
                                   (NumV t2) = (evalStatCFBE env y)
                               in (NumV(t1-t2))
                               
evalStatCFBE env (Mult x y) = let (NumV t1) = (evalStatCFBE env x)
                                  (NumV t2) = (evalStatCFBE env y)
                              in (NumV(t1*t2))
                              
evalStatCFBE env (Div x y) = let (NumV t1) = (evalStatCFBE env x)
                                 (NumV t2) = (evalStatCFBE env y)
                              in (NumV(div t1 t2))
                              
evalStatCFBE env (Lambda i b) = (ClosureV i b env)

evalStatCFBE env (App x y) = let (ClosureV i b e) = (evalStatCFBE env x)
                                 a = (evalStatCFBE env y)
                                in evalStatCFBE ((i,a):e) b
                      
evalStatCFBE env (Id x) = case (lookup x env) of
                        Just x -> x
                        Nothing -> error "Varible not found" 
                        
evalStatCFBE env (If x y z) = let (NumV t1) = (evalStatCFBE env x)
                              in if t1==0 then (evalStatCFBE env y) else (evalStatCFBE env z)

interpStatCFAE :: String -> CFAEVal
interpStatCFAE = (evalStatCFBE []) . parseCFAE
