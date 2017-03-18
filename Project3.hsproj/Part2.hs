--Name: Hao Luo--
--KUID: 2737588--
{-# LANGUAGE GADTs #-}

module Part2 where

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
import System.IO.Unsafe
--
-- Simple caculator with variables extended Booleans and both static and
-- dynamic type checking.
--
-- Author: Perry Alexander
-- Date: Wed Jul 13 11:24:46 CDT 2016
--
-- Source files for the Boolean Binding Arithmetic Expressions (BBAEL)
-- language from PLIH
--

-- BBAEL AST Definition

data BBAEL where
  Num :: Int -> BBAEL
  Plus :: BBAEL -> BBAEL -> BBAEL
  Minus :: BBAEL -> BBAEL -> BBAEL
  Bind :: String -> BBAEL -> BBAEL -> BBAEL
  Id :: String -> BBAEL
  Boolean :: Bool -> BBAEL
  And :: BBAEL -> BBAEL -> BBAEL
  Leq :: BBAEL -> BBAEL -> BBAEL
  IsZero :: BBAEL -> BBAEL
  If :: BBAEL -> BBAEL -> BBAEL -> BBAEL
  Seq :: BBAEL -> BBAEL -> BBAEL
  Print :: BBAEL -> BBAEL
  Cons :: BBAEL -> BBAEL -> BBAEL
  First :: BBAEL -> BBAEL
  Rest :: BBAEL -> BBAEL
  IsEmpty :: BBAEL -> BBAEL
  Empty :: BBAEL
  deriving (Show,Eq)

-- Parser

expr :: Parser BBAEL
expr = buildExpressionParser opTable term

opTable = [ [ inFix "+" Plus AssocLeft
              , inFix "-" Minus AssocLeft ]
          , [ inFix "<=" Leq AssocLeft
            , preFix "isZero" IsZero ]
          , [ inFix "&&" And AssocLeft ]
          ]

numExpr :: Parser BBAEL
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

identExpr :: Parser BBAEL
identExpr = do i <- identifier lexer
               return (Id i)

bindExpr :: Parser BBAEL
bindExpr = do reserved lexer "bind"
              i <- identifier lexer
              reservedOp lexer "="
              v <- expr
              reserved lexer "in"
              e <- expr
              return (Bind i v e)

trueExpr :: Parser BBAEL
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser BBAEL
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser BBAEL
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)
            
seqExpr :: Parser BBAEL
seqExpr = do reserved lexer "seq"
             f <- expr
             s <- expr
             return (Seq f s)

printExpr :: Parser BBAEL
printExpr = do reserved lexer "print"
               t <- expr
               return (Print t)

consExpr :: Parser BBAEL
consExpr = do reserved lexer "cons"
              f <- expr
              s <- expr
              return (Cons f s)

firstExpr :: Parser BBAEL
firstExpr = do reserved lexer "first"
               t <- expr
               return (First t)
             
restExpr :: Parser BBAEL
restExpr = do reserved lexer "rest"
              t <- expr
              return (Rest t)

isEmptyExpr :: Parser BBAEL
isEmptyExpr = do reserved lexer "isEmpty"
                 t <- expr
                 return (IsEmpty t)

emptyExpr :: Parser BBAEL
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

parseBBAEL = parseString expr

parseBBAELFile = parseFile expr


-- env function

type Env = [(String,BBAEL)]

eval :: Env -> BBAEL -> (Either String BBAEL)

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
                     Nothing -> Left "Variable not found"
                     
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

    
eval env (Print x) = let t1 = (eval env x)
                  in case t1 of 
                     (Left _) -> t1
                     (Right x) -> seq (unsafePerformIO (print x)) (Right (Num 0))
                     
eval env (Cons x y) = let t1 = (eval env x)
                          t2 = (eval env y)
                      in case t1 of
                        (Left a) -> t1
                        (Right b) -> case t2 of
                          (Left c) -> t2
                          (Right d) -> Right (Cons b d)

  
eval env (First x) = let t1 = (eval env x)
  in case t1 of
    (Left a) -> t1  
    (Right(Cons y z)) -> Right y
    (Right _) -> Left "This one is not a list"
    
eval env (Rest x) = let t1 = (eval env x)
  in case t1 of
    (Left a) -> t1  
    (Right(Cons y z)) -> Right z
    (Right _) -> Left "This one is not a list"
    
eval env (IsEmpty x) = let t1 = (eval env x)
  in case t1 of
    Left a -> t1
    Right Empty -> Right (Boolean True)
    Right _ -> Right (Boolean False)
    
eval env (Empty) = Right (Empty)

interpEx :: String -> (Either String BBAEL)
interpEx e = let p = parseBBAEL e in (eval [] p)
