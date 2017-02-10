<<<<<<< Updated upstream

=======
--Name: Hao Luo--
--KUID: 273588--
{-# LANGUAGE GADTs #-}

module Q1 where

import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import ParserUtils

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
                inFix "/" Div AssocLeft],
                [inFix "+" Plus AssocLeft,
                inFix "-" Minus AssocLeft]
            ]
            
term = parens lexer expr <|> numExpr

parseAE = parseString expr
parseAEFile = parseFile expr

--for part 3--

eval :: AE -> AE
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

--for part 4--
               
interp = eval . parseAE

>>>>>>> Stashed changes
