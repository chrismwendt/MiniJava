module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.Parsec.Prim
import qualified Text.ParserCombinators.Parsec.Token as Token

main :: IO ()
main = parseFile "examples/While.java" >>= print

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
    _ <- identifier
    braces $ do
        reserved "public"
        reserved "static"
        reserved "void"
        reserved "main"
        parens $ symbol "String" >> brackets empty >> identifier
        braces $ do
            s <- pStatement
            return $ MainClass s

pClass :: Parser Class
pClass = do
    reserved "class"
    return Class

pStatement :: Parser Statement
pStatement =
        pBlockStatement
    <|> pIfStatement
    <|> pWhileStatement
    <|> pPrintStatement
    <|> pExpressionStatement

pBlockStatement :: Parser Statement
pBlockStatement = braces $ do
    ss <- many pStatement
    return $ BlockStatement ss

pIfStatement :: Parser Statement
pIfStatement = do
    reserved "if"
    condition <- parens pExpression
    ifTrue <- pStatement
    ifFalse <- optionMaybe (reserved "else" >> pStatement)
    return $ IfStatement condition ifTrue ifFalse

pWhileStatement :: Parser Statement
pWhileStatement = do
    reserved "while"
    condition <- parens pExpression
    s <- pStatement
    return $ WhileStatement condition s

pPrintStatement :: Parser Statement
pPrintStatement = do
    reserved "System.out.println"
    e <- parens pExpression
    semi
    return $ PrintStatement e

pExpressionStatement :: Parser Statement
pExpressionStatement = do
    e <- pExpression
    semi
    return $ ExpressionStatement e

pExpression :: Parser Expression
pExpression =
        pIntLiteral
    <|> pBooleanLiteral

pIntLiteral :: Parser Expression
pIntLiteral = do
    i <- integer
    return $ IntLiteral (fromIntegral i)

pBooleanLiteral :: Parser Expression
pBooleanLiteral =
        (reserved "false" >> return (BooleanLiteral False))
    <|> (reserved "true" >> return (BooleanLiteral True))

pType :: Parser Type
pType =
        (Text.Parsec.Prim.try $ reserved "int" >> brackets empty >> return IntArrayType)
    <|> (reserved "boolean" >> return BooleanType)
    <|> (reserved "int" >> return IntType)
    <|> (identifier >>= return . ObjectType)

pVarDeclaration :: Parser VarDeclaration
pVarDeclaration = do
    t <- pType
    name <- identifier
    semi
    return $ VarDeclaration t name

data Program = Program MainClass [Class] deriving (Show)

data MainClass = MainClass Statement deriving (Show)

data Class = Class deriving (Show)

data Statement =
      BlockStatement [Statement]
    | IfStatement Expression Statement (Maybe Statement)
    | WhileStatement Expression Statement
    | PrintStatement Expression
    | ExpressionStatement Expression
    deriving (Show)

data Expression =
      IntLiteral Int
    | BooleanLiteral Bool
    deriving (Show)

data Type =
      BooleanType
    | IntType
    | IntArrayType
    | ObjectType String
    deriving (Show)

data VarDeclaration = VarDeclaration Type String deriving (Show)

languageDef = emptyDef
    { Token.commentStart    = "/*"
    , Token.commentEnd      = "*/"
    , Token.commentLine     = "//"
    , Token.identStart      = letter
    , Token.identLetter     = alphaNum <|> char '_'
    , Token.reservedNames   = words "class main public static void extends return int boolean if else while System.out.println true false this new"
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
symbol     = Token.symbol     lexer

empty :: Parser ()
empty = return ()
