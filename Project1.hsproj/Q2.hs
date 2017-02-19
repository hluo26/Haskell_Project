

module Q2 where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import ParserUtils

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
ifExpr = do reserved lexer "If0"
            a <- expr
            reserved lexer "then"
            b <- expr
            reserved lexer "else"
            c <- expr
            return (If0 a b c)              
              
operators = [ [ inFix "*" Mul AssocLeft,
                inFix "/" Div AssocLeft],
                [inFix "+" Plus AssocLeft,
                inFix "-" Minus AssocLeft]
            ]
            
term = parens lexer expr <|> numExpr <|> ifExpr 

parseAEE = parseString expr
parseAEEFile = parseFile expr

--for part 3--

eval :: AEE -> AEE
eval (Number n) = (Number n)
eval (Plus x y) = let (Number x1) = eval x
                      (Number y1) = eval y 
                  in (Number (x1 + y1))
eval (Minus x y) = let (Number x1) = eval x 
                       (Number y1) = eval y
                   in (Number (x1 - y1))
eval (Mul x y) = let (Number x1) = eval x 
                     (Number y1) = eval y 
                  in (Number (x1 * y1))
eval (Div x y) = let (Number x1) = eval x 
                     (Number y1) = eval y 
                  in (Number (div x1 y1))
                                    
eval (If0 x y z) = let (Number x1) = (eval x)
                    in if (x1==0) then (eval y) else (eval z)
                      
                    
--for part 4--
               
interp = eval . parseAEE


