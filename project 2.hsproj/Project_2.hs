--Name: Hao Luo--
--KUID: 2737588--
{-# LANGUAGE GADTs #-}
module Project_2 where
  
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import ParserUtils

--PART I--

data ABE where
  Number :: Int -> ABE
  Boolean ::Bool -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Mul :: ABE -> ABE -> ABE
  Div :: ABE -> ABE -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If ::ABE -> ABE -> ABE -> ABE
  deriving(Show,Eq)

  
--PART II--
  
expr::Parser ABE
expr = buildExpressionParser opTable term

opTable = [[inFix "*" Mul AssocLeft,
            inFix "/" Div AssocLeft],
            [inFix "+" Plus AssocLeft,
             inFix "-" Minus AssocLeft],
            [inFix "<=" Leq AssocLeft,
              preFix "IsZero" IsZero],
              [inFix "&&" And AssocLeft]]
              
numExpr::Parser ABE
numExpr = do i <- integer lexer
             return (Number(fromInteger i))
            
trueExpr::Parser ABE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)
             
falseExpr::Parser ABE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)
             
ifExpr::Parser ABE
ifExpr = do i <- reserved lexer "if"
            a <- expr
            reserved lexer "then"
            b <- expr
            reserved lexer "else"
            c <- expr
            return (If a b c)
            
term = parens lexer expr
        <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr
       
parseABE = parseString expr

parseABEFile = parseFile expr

--PART III--

eval :: ABE -> Either String ABE
eval (Number x) = (Right (Number x))
eval (Boolean x) = (Right(Boolean x))
eval (Plus x y) = let t1 = (eval x)
                      t2 = (eval y)
                   in case t1 of
                     (Left a) -> t1
                     (Right (Number v1)) -> case t2 of
                                          (Left b) -> t2
                                          (Right (Number v2)) -> (Right(Number (v1 + v2)))
                                          (Right _) -> (Left "Type Error in +")
                     (Right _) -> (Left "Type Error in +")
                     
eval (Minus x y) = let t1 = (eval x)
                       t2 = (eval y)
                   in case t1 of
                     (Left a) -> t1
                     (Right (Number v1)) -> case t2 of
                                          (Left b) -> t2
                                          (Right (Number v2)) -> (Right(Number (v1 - v2)))
                                          (Right _) -> (Left "Type Error in -")
                     (Right _) -> (Left "Type Error in -")
                     
eval (Mul x y) = let t1 = (eval x)
                     t2 = (eval y)
                   in case t1 of
                     (Left a) -> t1
                     (Right (Number v1)) -> case t2 of
                                          (Left b) -> t2
                                          (Right (Number v2)) -> (Right(Number (v1 * v2)))
                                          (Right _) -> (Left "Type Error in *")
                     (Right _) -> (Left "Type Error in *") 
                     
eval (Div x y) = let t1 = (eval x)
                     t2 = (eval y)
                   in case t1 of
                     (Left a) -> t1
                     (Right (Number v1)) -> case t2 of
                                          (Left b) -> t2
                                          (Right (Number v2)) -> if v2 == 0 then (Left "Running time error: Divide by zero") else (Right(Number (v1 `div` v2)))
                                          (Right _) -> (Left "Type Error in /")
                     (Right _) -> (Left "Type Error in /")
                     
eval (And x y) = let t1 = (eval x)
                     t2 = (eval y)
                     in case t1 of 
                          (Left a) -> t1
                          (Right (Boolean v1)) -> case t2 of
                                                 (Left b) -> t2
                                                 (Right (Boolean v2)) -> (Right(Boolean(v1 && v2)))
                                                 (Right _) -> (Left "Type Error in &&")
                          (Right _) -> (Left "Type Error in &&")
                       
eval (Leq x y) = let t1 = (eval x)
                     t2 = (eval y)
                     in case t1 of 
                       (Left a) -> t1
                       (Right (Number v1)) -> case t2 of
                                              (Left b) -> t2
                                              (Right (Number v2)) -> (Right(Boolean(v1 <= v2)))
                                              (Right _) -> (Left "Type Error in <=")
                       (Right _) -> (Left "Type Error in <=")
                       
eval (IsZero x) = let t1 = (eval x)                 
                  in case t1 of
                    (Left m) -> t1
                    (Right (Number v)) -> (Right (Boolean (v == 0)))
                    (Right _) -> (Left "Type error in isZero")
                    
eval (If x y z) = let t1 = (eval x)
                     in case t1 of
                     (Left _) -> t1
                     (Right (Boolean v)) -> if v then (eval y) else (eval z)
                     (Right _) -> (Left "Type error in if")
                     

--PART IV--

data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving(Show, Eq)
  
typeof :: ABE -> (Either String TABE) 
typeof (Number x) = (Right TNum)
typeof (Boolean x) = (Right TBool)
typeof (Plus x y) = let t1 = (typeof x)
                        t2 = (typeof y)
                        in if t1 == (Right TNum) && t2 == (Right TNum)
                        then (Right TNum)
                        else Left "Type Mismatch in +"
                        
typeof (Minus x y) = let t1 = (typeof x)
                         t2 = (typeof y)
                         in if t1 == (Right TNum) && t2 == (Right TNum)
                         then (Right TNum)
                         else Left "Type Mismatch in -"

typeof (Mul x y) = let  t1 = (typeof x)
                        t2 = (typeof y)
                        in if t1 == (Right TNum) && t2 == (Right TNum)
                        then (Right TNum)
                        else Left "Type Mismatch in *"
                        
typeof (Div x y) = let  t1 = (typeof x)
                        t2 = (typeof y)
                        in if t1 == (Right TNum) && t2 == (Right TNum)
                        then if y==(Number 0) then (Left "Typing checking error: Divide by zero") else(Right TNum)                                
                        else Left "Type Mismatch in /"
                        
typeof (And x y) = let  t1 = (typeof x)
                        t2 = (typeof y)
                        in if t1 == (Right TBool) && t2 == (Right TBool)
                        then (Right TBool)
                        else Left "Type Mismatch in &&"
                        
typeof (Leq x y) = let  t1 = (typeof x)
                        t2 = (typeof y)
                        in if t1 == (Right TNum) && t2 == (Right TNum)
                        then (Right TBool)
                        else Left "Type Mismatch in <="
                        
typeof (IsZero x) = let t1 = (typeof x)
                        in if t1 == (Right TNum) 
                        then (Right TBool)
                        else Left "Type Mismatch in IsZero"
                        
typeof (If x y z) = let t1 = (typeof x)
                        t2 = (typeof y)
                        t3 = (typeof z)
                        in if t1 == (Right TBool)
                        then if t2 == t3 then t2 else Left"Type Mismatch in If" 
                        else Left "Type Mismatch in If"                       
                        
--PART V--

interp :: String -> Either String ABE
interp e = let x = (parseABE e) in
               case (typeof x) of
                 (Right _) -> (eval (optimize x))
                 (Left y) -> (Left y)
                 
interpOp e = let x = (parseABE e) in
               case (typeof x) of
                 (Right _) -> (eval x)
                 (Left y) -> (Left y) 
                  
--Second Problem--

optimize :: ABE -> ABE

optimize (Number x) = (Number x)

optimize (Boolean x) = (Boolean x)

optimize (Plus x (Number 0)) = (optimize x)

optimize (Plus (Number 0) y) = (optimize y)

optimize (Plus x y) = (Plus (optimize x) (optimize y))

optimize (Minus x y) = (Minus (optimize x) (optimize y))

optimize (Mul x y) = (Mul (optimize x) (optimize y))

optimize (Div x y) = (Div (optimize x) (optimize y))

optimize (And x y) = (And (optimize x) (optimize y))

optimize (Leq x y) = (Leq (optimize x) (optimize y))

optimize (IsZero x) = (IsZero (optimize x))

optimize (If (Boolean True) x y) = (optimize x)

optimize (If (Boolean False) x y) = (optimize y)

optimize (If x y z) = (If (optimize x) (optimize y) (optimize z))
