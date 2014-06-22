module PTEstruturado.Parse
  ( parse
  , parseFile
  ) where

import PTEstruturado.Data

import Data.List (find)

import System.IO
import Control.Monad
import Text.Parsec.Prim hiding (runParser, try)
import Text.ParserCombinators.Parsec  hiding (Parser)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


type Parser = Parsec String [Variavel]

searchVar:: [Variavel] -> String -> Maybe Tipo
searchVar vs name = liftM tipo $ find test vs
  where 
    test (Variavel v t) = v == name

languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "se"
                                      , "então"
                                      , "senão"
                                      , "fimse"
                                      , "enquanto"
                                      , "faça"
                                      , "fimenquanto"
                                      , "verdadeiro"
                                      , "falso"
                                      , "não"
                                      , "e"
                                      , "ou"
                                      , "algoritimo"
                                      , "inicio"
                                      , "fimalgoritimo"
                                      , "var"
                                      , "int"
                                      , "real"
                                      , "logico"
                                      , "leia"
                                      , "escreva"
                                      ]
            , Token.reservedOpNames = ["+", "-", "*", "/", "%", "="
                                      , "<", "<=", "==", "!=", ">=", ">"
                                      , "e", "ou", "não"
                                      ]
            }
            
lexer = Token.makeTokenParser languageDef

identifier     = Token.identifier     lexer -- parses an identifier
reserved       = Token.reserved       lexer -- parses a reserved name
reservedOp     = Token.reservedOp     lexer -- parses an operator
parens         = Token.parens         lexer -- parses surrounding parenthesis. use: parens p
integer        = Token.integer        lexer -- parses an integer
whiteSpace     = Token.whiteSpace     lexer -- parses whitespace
semi           = Token.semi           lexer
naturalOrFloat = Token.naturalOrFloat lexer


ptestruturadoParser :: Parser Algoritimo
ptestruturadoParser = do
  whiteSpace
  name <- lookAhead (reserved "algoritimo">>identifier)
  vars <- between (reserved "algoritimo">>identifier) (reserved "inicio") varblock
  stmt <- statement
  reserved "fimalgoritimo"
  return (Algoritimo name vars stmt)

variable::Tipo -> Parser Variavel
variable t = try $ do
  name <- identifier
  vars <- getState
  case searchVar vars name of
       Just tipo 
         | tipo == t -> return (Variavel name t)
         | otherwise -> unexpected ("variable "++show name++" typed " ++ show tipo)
       Nothing       -> unexpected ("undefined variable '"++show name)

varblock :: Parser [Variavel]
varblock = (declaration <?> "variable declaration") `endBy` semi 

declaration :: Parser Variavel
declaration = try $ do
  reserved "var"
  name  <- identifier
  type' <- ptype
  vars  <- getState
  case searchVar vars name of
       Just _  -> unexpected "declaration for variable already declared"
       Nothing -> let v = Variavel name type'
                  in (modifyState (v:) >> return v)
  where 
    ptype =  (reserved "int"  >> return Inteiro)
         <|> (reserved "real"  >> return Fracionario)
         <|> (reserved "logico"  >> return Logico)
      
      

statement :: Parser Instr
statement = liftM Seq (stmt `endBy` semi) 
  where
     stmt = readStmt
        <|> assignStmt
        <|> printStmt
        <|> ifStmt
        <|> whileStmt

readStmt :: Parser Instr
readStmt = try $ do
  var <- (variable Inteiro <|> variable Fracionario <|> variable Logico)
  let name = nome var
  reservedOp "="
  reserved "leia" >> parens whiteSpace
  return (Ler name)

assignStmt :: Parser Instr
assignStmt = do
    var <- (variable Inteiro <|> variable Fracionario <|> variable Logico)
    let name = nome var
    reservedOp "="
    value <- case tipo var of
                  Logico      -> liftM Logica bExpr
                  Fracionario -> liftM Arit   aExpr
                  Inteiro     -> liftM Arit   aExpr
    return (Atrib name value)

printStmt :: Parser Instr
printStmt = reserved "escreva" >> (liftM Escreva (parens $ expr))
  
ifStmt :: Parser Instr
ifStmt = between (reserved "se") (reserved "fimse") $ do
  cond <- bExpr
  reserved "então"
  stmt1 <- statement
  stmt2 <- (reserved "senão" >> statement) <|> (return $ Seq [])
  return $ Se cond stmt1 stmt2

whileStmt :: Parser Instr
whileStmt = between (reserved "enquanto") (reserved "fimenquanto") $ do
  cond <- bExpr
  reserved "faça"
  stmt <- statement
  return $ Enquanto cond stmt
  
expr :: Parser Expr  
expr =  (liftM Logica bExpr)
    <|> (liftM Arit aExpr)



bExpr :: Parser ExpLogica
bExpr =  (buildExpressionParser bOperators) bTerm

bOperators = [ [Prefix (reserved "não" >> return Negacao)]
             , [Infix  (reserved "e"   >> return (LogicoBin E)) AssocLeft]
             , [Infix  (reserved "ou"  >> return (LogicoBin Ou)) AssocLeft]
             ]
             
bTerm =  parens bExpr
     <|> (reserved "verdadeiro"  >> return (ConsLogica True ))
     <|> (reserved "falso" >> return (ConsLogica False))
     <|> (liftM (VarLogica . nome) (variable Logico))
     <|> rExpr
     <?> "boolean expression"

rExpr :: Parser ExpLogica     
rExpr = try $ do
  l <- aExpr
  op <- relation
  r <- aExpr
  return $ RelacianalBin op l r
  where
    relation =  (reservedOp ">"  >> return Maior)
            <|> (reservedOp ">=" >> return MaiorIgual)
            <|> (reservedOp "<"  >> return Menor)
            <|> (reservedOp "<=" >> return MenorIgual)
            <|> (reservedOp "==" >> return Igual)
            <|> (reservedOp "!=" >> return Diferente)
             
    

  
aExpr :: Parser ExpArit
aExpr = buildExpressionParser aOperators aTerm

aOperators = [ [Prefix (reservedOp "-" >> return Neg)]
             , [Infix  (reservedOp "*" >> return (AritBin Mult)) AssocLeft]
             , [Infix  (reservedOp "/" >> return (AritBin Divi)) AssocLeft]
             , [Infix  (reservedOp "%" >> return (AritBin Rest)) AssocLeft]
             , [Infix  (reservedOp "-" >> return (AritBin Subt)) AssocLeft]
             , [Infix  (reservedOp "+" >> return (AritBin Soma)) AssocLeft]
             ]

aTerm =  parens aExpr
     <|> liftM (VarArit . nome) (variable Inteiro <|> variable Fracionario)
     <|> liftM ConsArit naturalOrFloat
     <?> "arithmetic expression"

parseFile :: String -> IO Algoritimo
parseFile file = do
  program  <- readFile file
  case runParser ptestruturadoParser [] file program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r
