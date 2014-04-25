module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as Token

main :: IO ()
main = parseFile "examples/Leet.java" >>= print

parseString :: String -> Program
parseString str =
  case parse pProgram "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Program
parseFile file =
  do program  <- readFile file
     case parse pProgram "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

pProgram :: Parser Program
pProgram = do
    whiteSpace
    m <- pMainClass
    cs <- many pClass
    return $ Program m cs

pMainClass :: Parser MainClass
pMainClass = do
    reserved "class"
    name <- identifier
    braces $ do
        reserved "public"
        reserved "static"
        reserved "void"
        reserved "main"
        parens $ reserved "String" >> brackets (return ()) >> identifier
        braces $ do
            s <- pStatement
            return $ MainClass name

pClass :: Parser Class
pClass = do
    reserved "class"
    return Class

pStatement :: Parser Statement
pStatement = do
    reserved "System.out.println"
    parens pExpression
    semi
    return Print

pExpression :: Parser Expression
pExpression = integer >> return IntLiteral

data Program = Program MainClass [Class] deriving (Show)

data MainClass = MainClass String deriving (Show)

data Class = Class deriving (Show)

data Statement = Print deriving (Show)

data Expression = IntLiteral deriving (Show)

languageDef = emptyDef
    { Token.commentStart    = "/*"
    , Token.commentEnd      = "*/"
    , Token.commentLine     = "//"
    , Token.identStart      = letter
    , Token.identLetter     = alphaNum <|> char '_'
    , Token.reservedNames   = words "class main public static void String extends return int boolean if else while System.out.println true false this new"
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
semi       = Token.semi       lexer
whiteSpace = Token.whiteSpace lexer
braces     = Token.braces     lexer
brackets   = Token.brackets   lexer
