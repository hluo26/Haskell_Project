{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Project5 where

import Text.ParserCombinators.Parsec
import Control.Monad
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token

data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  deriving (Show,Eq)

data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Mult :: FBAE -> FBAE -> FBAE
  Div :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> TFBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  Boolean :: Bool -> FBAE
  And :: FBAE -> FBAE -> FBAE
  Or :: FBAE -> FBAE -> FBAE
  Leq :: FBAE -> FBAE -> FBAE
  IsZero :: FBAE -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  Fix :: FBAE -> FBAE
  deriving (Show,Eq)

tokenDef =
  javaStyle { Token.identStart = letter
            , Token.identLetter = alphaNum
            , Token.reservedNames = [ "lambda"
                                    , "bind"
                                    , "in"
                                    , "if"
                                    , "then"
                                    , "else"
                                    , "isZero"
                                    , "app"
                                    , "Num"
                                    , "Bool"
                                    , "true"
                                    , "false"
                                    , "fix" ]
            , Token.reservedOpNames = [ "+","-","*","/","&&","||","<=","=",":","->"]
            }

lexer = Token.makeTokenParser tokenDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer

-- Term parser

expr :: Parser FBAE
expr = buildExpressionParser operators term

operators = [ [Infix (reservedOp "*" >> return (Mult )) AssocLeft,
               Infix (reservedOp "/" >> return (Div )) AssocLeft ]
            , [Infix (reservedOp "+" >> return (Plus )) AssocLeft,
               Infix (reservedOp "-" >> return (Minus )) AssocLeft ]
            , [Infix (reservedOp "&&" >> return (And )) AssocLeft,
               Infix (reservedOp "||" >> return (Or )) AssocLeft]
            , [Infix (reservedOp "<=" >> return (Leq )) AssocLeft ]
            , [Prefix (reserved "isZero" >> return (IsZero )) ]
            ]

numExpr :: Parser FBAE
numExpr = do i <- integer
             return (Num (fromInteger i))

trueExpr :: Parser FBAE
trueExpr = do i <- reserved "true"
              return (Boolean True)

falseExpr :: Parser FBAE
falseExpr = do i <- reserved "false"
               return (Boolean False)

ifExpr :: Parser FBAE
ifExpr = do reserved "if"
            c <- expr
            reserved "then"
            t <- expr
            reserved "else"
            e <- expr
            return (If c t e)

identExpr :: Parser FBAE
identExpr = do i <- identifier
               return (Id i)

bindExpr :: Parser FBAE
bindExpr = do reserved "bind"
              i <- identifier
              reservedOp "="
              v <- expr
              reserved "in"
              e <- expr
              return (Bind i v e)

lambdaExpr :: Parser FBAE
lambdaExpr = do reserved "lambda"
                (i,t) <- parens argExpr
                reserved "in"
                b <- expr
                return (Lambda i t b)

argExpr :: Parser (String,TFBAE)
argExpr = do i <- identifier
             reservedOp ":"
             t <- ty
             return (i,t)

appExpr :: Parser FBAE
appExpr = do reserved "app"
             f <- expr
             a <- expr
             return (App f a)

fixExpr :: Parser FBAE
fixExpr = do reserved "fix"
             t <- expr
             return (Fix t)

term = parens expr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr
       <|> identExpr
       <|> bindExpr
       <|> lambdaExpr
       <|> appExpr
       <|> fixExpr

-- Type parser

ty = buildExpressionParser tyoperators tyTerm

tyoperators = [ [Infix (reservedOp "->" >> return (:->: )) AssocLeft ] ]

tyTerm :: Parser TFBAE
tyTerm = parens ty <|> tyNat <|> tyBool

tyNat :: Parser TFBAE
tyNat = do reserved "Nat"
           return TNum

tyBool :: Parser TFBAE
tyBool = do reserved "Bool"
            return TBool

-- Parser invocation

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

parseFBAE = parseString expr

parseFile p file =
  do program <- readFile file
     case parse p "" program of
       Left e -> print e >> fail "parse error"
       Right r -> return r

parseFBAEFile = parseFile expr

data CFBAValue where
  NumV :: Int -> CFBAValue
  BooleanV :: Bool -> CFBAValue
  ClosureV :: String -> FBAE -> Env -> CFBAValue
  LambdaV :: String -> FBAE -> CFBAValue
  deriving (Show,Eq)


subst :: String -> FBAE -> FBAE -> FBAE

subst _ _ (Num x) = (Num x)

subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))

subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = if i==i'
                         then (Bind i' (subst i v v') b')
                         else (Bind i' (subst i v v') (subst i v b'))
                         
subst i v (Id i') = if i==i'
                 then v
                 else (Id i')
                 
subst i v (Lambda x y z) = (Lambda x y (subst i v z))


--Q1--

type Env = [(String,CFBAValue)]

eval :: Env -> FBAE -> CFBAValue

eval env (Num x) = (NumV x)

eval env (Id x) = case (lookup x env) of
                     Just x -> x
                     Nothing -> error "Varible not found"
                     
eval env (Plus x y) = let (NumV t1) = (eval env x)
                          (NumV t2) = (eval env y)
                          in (NumV (t1+t2))
                          
eval env (Minus x y) = let (NumV t1) = (eval env x)
                           (NumV t2) = (eval env y)
                          in (NumV (t1-t2))
                          
eval env (Mult x y) = let (NumV t1) = (eval env x)
                          (NumV t2) = (eval env y)
                          in (NumV (t1*t2))
                          
eval env (Div x y) = let (NumV t1) = (eval env x)
                         (NumV t2) = (eval env y)
                          in if t2 == 0 then error "Running time error" else (NumV (div t1 t2))
                          
eval env (Lambda i x b) = ClosureV i b env

eval env (Bind i b e) = let t1 = (eval env b)
                        in eval ((i,t1):env)e
                        
eval env (App x y) = let (ClosureV i b e) = (eval env x)
                         t1 = (eval env y)
                         in eval((i,t1):env)b
                         
eval env (Boolean x) = (BooleanV x)

eval env (And x y) = let (BooleanV t1) = (eval env x)
                         (BooleanV t2) = (eval env y)
                          in (BooleanV (t1&&t2))
                          
eval env (Or x y) = let (BooleanV t1) = (eval env x)
                        (BooleanV t2) = (eval env y)
                        in (BooleanV (t1||t2))
                        
eval env (Leq x y) = let (NumV t1) = (eval env x)
                         (NumV t2) = (eval env y)
                         in (BooleanV (t1<=t2))
                         
eval env (IsZero x) = let (NumV t1) = (eval env x)
                        in (BooleanV (t1==0))
                        
eval env (If x y z) = let (BooleanV t1) = (eval env x)
                      in if t1 then (eval env y) else (eval env z)
                                            

-- Q2 --

eval env (Fix f) = let (ClosureV i b e) = (eval env f) in
                     eval e (subst i (Fix (Lambda i TNum b)) b)
                      
                      
interp :: String -> CFBAValue

interp = eval[] . parseFBAE



