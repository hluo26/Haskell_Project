{-# LANGUAGE GADTs #-}

module Proj2Utils where

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

-- Imports for print
--import System.IO.Unsafe
--
-- Simple caculator with variables extended Booleans and both static and
-- dynamic type checking.
--
-- Author: Perry Alexander
-- Date: Wed Jul 13 11:24:46 CDT 2016
--
-- Source files for the Boolean Binding Arithmetic Expressions (BBAE)
-- language from PLIH
--

-- BBAE AST Definition

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  Seq :: BBAE -> BBAE -> BBAE
  Print :: BBAE -> BBAE
  Cons :: BBAE -> BBAE -> BBAE
  First :: BBAE -> BBAE
  Rest :: BBAE -> BBAE
  IsEmpty :: BBAE -> BBAE
  Empty :: BBAE
  deriving (Show,Eq)

-- Parser

expr :: Parser BBAE
expr = buildExpressionParser opTable term

opTable = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
          , [ inFix "<=" Leq AssocLeft
            , preFix "isZero" IsZero ]
          , [ inFix "&&" And AssocLeft ]
          ]

numExpr :: Parser BBAE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

identExpr :: Parser BBAE
identExpr = do i <- identifier lexer
               return (Id i)

bindExpr :: Parser BBAE
bindExpr = do reserved lexer "bind"
              i <- identifier lexer
              reservedOp lexer "="
              v <- expr
              reserved lexer "in"
              e <- expr
              return (Bind i v e)

trueExpr :: Parser BBAE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser BBAE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser BBAE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)
            
seqExpr :: Parser BBAE
seqExpr = do reserved lexer "seq"
             f <- expr
             s <- expr
             return (Seq f s)

printExpr :: Parser BBAE
printExpr = do reserved lexer "print"
               t <- expr
               return (Print t)

consExpr :: Parser BBAE
consExpr = do reserved lexer "cons"
              f <- expr
              s <- expr
              return (Cons f s)

firstExpr :: Parser BBAE
firstExpr = do reserved lexer "first"
               t <- expr
               return (First t)
             
restExpr :: Parser BBAE
restExpr = do reserved lexer "rest"
              t <- expr
              return (Rest t)

isEmptyExpr :: Parser BBAE
isEmptyExpr = do reserved lexer "isEmpty"
                 t <- expr
                 return (IsEmpty t)

emptyExpr :: Parser BBAE
emptyExpr = do reserved lexer "empty"
               return Empty
             
term = parens lexer expr
       <|> numExpr
       <|> identExpr
       <|> bindExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr
       <|> consExpr
       <|> firstExpr
       <|> restExpr              
       <|> isEmptyExpr
       <|> emptyExpr
       <|> printExpr
       <|> seqExpr
       
parseBAE = parseString expr

parseBAEFile = parseFile expr

-- Parser invocation

parseBBAE = parseString expr

parseBBAEFile = parseFile expr

-- subst function

subst :: String -> BBAE -> BBAE -> BBAE

subst _ _ (Num x) = (Num x)

subst i v (Plus x y) = (Plus (subst i v x)(subst i v y))

subst i v (Minus x y) = (Minus (subst i v x)(subst i v y))

subst i v (Bind x y z) = if i==x
                            then (Bind x (subst i v y) z )
                            else (Bind x (subst i v y) (subst i v z))
                            
subst i v (Id x) = if i ==x
                   then v
                   else (Id x)

-- eval function

evals :: BBAE -> (Either String BBAE)

evals (Num x) = (Right(Num x))

evals (Plus x y) = do
  t1 <- (evals x)
  t2 <- (evals y)
  case t1 of
    (Num v1) -> case t2 of
                (Num v2) -> (Right(Num(v1+v2)))
                (Boolean _) -> (Left "Type Error in +")
    (Boolean _) -> (Left "Type Error in +")
    
evals (Minus x y) = do
  t1 <- (evals x)
  t2 <- (evals y)
  case t1 of
    (Num v1) -> case t2 of
                (Num v2) -> (Right(Num(v1-v2)))
                (Boolean _) -> (Left "Type Error in -")
    (Boolean _) -> (Left "Type Error in -")
    
evals (Bind i v b) = do
  t1 <- (evals v)
  (evals (subst i t1 b))
    
    
evals (Id id) = error "Undeclared Variable"
                                                 
evals (Boolean x) = (Right(Boolean x))

evals (And x y) = do
  t1 <- (evals x)
  t2 <- (evals y)
  case t1 of 
    (Boolean v1) -> case t2 of
                (Boolean v2) -> (Right(Boolean(v1 && v2)))
                (Boolean _) -> (Left "Type Error in &&")
    (Boolean _) -> (Left "Type Error in &&")
    
evals (Leq x y) = do
  t1 <- (evals x)
  t2 <- (evals y)
  case t1 of 
    (Num v1) -> case t2 of
                (Num v2) -> (Right(Boolean(v1 <= v2)))
                (Boolean _) -> (Left "Type Error in <=")
    (Boolean _) -> (Left "Type Error in <=")
    
evals (IsZero x) = do
  t1 <- (evals x)
  case t1 of 
     (Num v1) -> (Right(Boolean(v1==0)))
     (Boolean _) -> (Left "Type Error in IsZero")
     
evals (If x y z) = let t1 = (evals x)
                     in case t1 of
                     (Left _) -> t1
                     (Right (Boolean v)) -> if v then (evals y) else (evals z)
                     (Right _) -> (Left "Type error in If")
                     
evals (Seq x y) = let t1 = (evals x)
                      t2 = (evals y)
                   in case t1 of 
                     (Left _) -> t1
                     (Right v1) -> case t2 of
                          (Left _) ->t2
                          (Right v2) -> t2
                          (Right _) -> (Left "Type error in Seq")
                     (Right _) -> (Left "Type error in Seq")
    
evals (Print x) = let t1 = (evals x)
                  in case t1 of 
                    (Left _) -> t1
                    (Right x) -> seq (print t1) Right (Num 0)                     
  
-- env function

type Env = [(String,BBAE)]

eval :: Env -> BBAE -> (Either String BBAE)

eval env (Num x) = Right(Num x)

eval env (Plus x y) =
  let t1 = (eval env x)
      t2 = (eval env y)
      in case t1 of
       (Left m) -> t1
       (Right (Num v1)) -> case t2 of
                            (Left m) -> t2
                            (Right (Num v2)) -> (Right (Num (v1+v2)))
                            (Right _) -> (Left "Type Error in +")
       (Right _) -> (Left "Type Error in +")
       
eval env (Minus x y) =
  let t1 = (eval env x)
      t2 = (eval env y)
      in case t1 of
       (Left m) -> t1
       (Right (Num v1)) -> case t2 of
                            (Left m) -> t2
                            (Right (Num v2)) -> (Right (Num (v1-v2)))
                            (Right _) -> (Left "Type Error in -")
       (Right _) -> (Left "Type Error in -")
       
eval env (Bind i v b) = do
   t1 <- (eval env v) 
   eval ((i,t1):env) b
   
eval env (Id id) = case (lookup id env) of
                     Just x -> Right x
                     Nothing -> error "Variable not found"
                     
eval env (Boolean x) = Right(Boolean x)

eval env (And x y) = 
  let t1 = (eval env x)
      t2 = (eval env y)
      in case t1 of
       (Left m) -> t1
       (Right (Boolean v1)) -> case t2 of
                            (Left m) -> t2
                            (Right (Boolean v2)) -> (Right (Boolean (v1&&v2)))
                            (Right _) -> (Left "Type Error in &&")
       (Right _) -> (Left "Type Error in &&")
       

eval env (Leq x y) = 
  let t1 = (eval env x)
      t2 = (eval env y)
      in case t1 of
       (Left m) -> t1
       (Right (Num v1)) -> case t2 of
                            (Left m) -> t2
                            (Right (Num v2)) -> (Right (Boolean (v1<=v2)))
                            (Right _) -> (Left "Type Error in <=")
       (Right _) -> (Left "Type Error in <=")
       
eval env (IsZero x) = do
  t1 <- (eval env x)
  case t1 of 
     (Num v1) -> (Right(Boolean(v1==0)))
     (Boolean _) -> (Left "Type Error in IsZero")
     
eval env (If x y z) = let t1 = (eval env x)
                     in case t1 of
                     (Left _) -> t1
                     (Right (Boolean v)) -> if v then (eval env y) else (eval env z)
                     (Right _) -> (Left "Type error in If")
                     
eval env (Seq x y) = let t1 = (eval env x)
                         t2 = (eval env y)
                   in case t1 of 
                     (Left _) -> t1
                     (Right v1) -> case t2 of
                          (Left _) ->t2
                          (Right v2) -> t2
                          (Right _) -> (Left "Type error in Seq")
                     (Right _) -> (Left "Type error in Seq")
    
eval env (Print x) = let t1 = (eval env x)
                  in case t1 of 
                     (Left _) -> t1
                     (Right x) -> seq (print t1) Right (Num 0)
                     

interps :: String -> (Either String BBAE)
interps = evals . parseBBAE

interp :: String -> (Either String BBAE)
interp = (eval []) . parseBBAE 