-- TODO try using Parsec's operator table
module Parser where

import Text.Parsec hiding ((<|>), many, try)
import Text.Parsec.Prim hiding ((<|>), many)
import Text.Parsec.String
import AST
import Data.Functor
import Control.Applicative
import Lexer

parseString :: String -> Program
parseString str = case parse program "" str of
    Left e  -> error $ show e
    Right r -> r

program :: Parser Program
program = Program <$> (whiteSpace *> mainClass) <*> many normalClass

mainClass :: Parser Statement
mainClass = reserved "class" >> identifier >> braces (do
    reserved "public" >> reserved "static" >> reserved "void" >> reserved "main"
    parens $ symbol "String" >> brackets nothing >> identifier
    braces statement)

normalClass :: Parser ClassDecl
normalClass = do
    reserved "class"
    name <- identifier
    extends <- optionMaybe (reserved "extends" >> identifier)
    braces $ ClassDecl name extends <$> many varDecl <*> many methodDecl

typeSpecifier :: Parser Type
typeSpecifier =
        try (IntArrayType <$ reserved "int" <* brackets nothing)
    <|> BooleanType <$ reserved "boolean"
    <|> IntType <$ reserved "int"
    <|> ObjectType <$> identifier

varDecl :: Parser VarDecl
varDecl = VarDecl <$> typeSpecifier <*> identifier <* semi

methodDecl :: Parser MethodDecl
methodDecl = do
    reserved "public"
    t <- typeSpecifier
    name <- identifier
    params <- parens $ parameter `sepBy` comma
    braces $ MethodDecl t name params <$> many (try varDecl) <*> many statement <*> (reserved "return" *> expression) <* semi

parameter :: Parser Parameter
parameter = Parameter <$> typeSpecifier <*> identifier

statement :: Parser Statement
statement =
        blockStatement
    <|> ifStatement
    <|> whileStatement
    <|> printStatement
    <|> expressionStatement

blockStatement :: Parser Statement
blockStatement = BlockStatement <$> braces (many statement)

ifStatement :: Parser Statement
ifStatement = IfStatement <$> (reserved "if" *> parens expression) <*> statement <*> optionMaybe (reserved "else" >> statement)

whileStatement :: Parser Statement
whileStatement = WhileStatement <$> (reserved "while" *> parens expression) <*> statement

printStatement :: Parser Statement
printStatement = PrintStatement <$> (reserved "System.out.println" *> parens expression <* semi)

expressionStatement :: Parser Statement
expressionStatement = ExpressionStatement <$> expression <* semi

expression :: Parser Exp
expression = assignment

assignment :: Parser Exp
assignment = do
    target <- logicOp
    o <- optionMaybe (symbol "=" >> assignment)
    return $ case o of
        Just value -> AssignExpression target value
        Nothing -> target

logicOp :: Parser Exp
logicOp = chainl1 comparisonOp ((symbol "&&" <|> symbol "||") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

comparisonOp :: Parser Exp
comparisonOp = chainl1 addOp (foldr1 (<|>) (map (try . symbol) $ words "<= < == != >= >") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

addOp :: Parser Exp
addOp = chainl1 mulOp ((symbol "+" <|> symbol "-") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

mulOp :: Parser Exp
mulOp = chainl1 unaryOp ((symbol "*" <|> symbol "/" <|> symbol "%") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

unaryOp :: Parser Exp
unaryOp = notOp <|> postfixOp

notOp :: Parser Exp
notOp = NotExp <$ symbol "!" <*> unaryOp

postfixOp :: Parser Exp
postfixOp = foldl (flip ($)) <$> primaryExpression <*> many (indexPostfixOp <|> callPostfixOp <|> memberPostfixOp)

indexPostfixOp :: Parser (Exp -> Exp)
indexPostfixOp = flip IndexExp <$> brackets expression

callPostfixOp :: Parser (Exp -> Exp)
callPostfixOp = try $ (\m as o -> CallExp o m as) <$ symbol "." <*> identifier <*> parens (expression `sepBy` comma)

memberPostfixOp :: Parser (Exp -> Exp)
memberPostfixOp = flip MemberExp <$ symbol "." <*> identifier

primaryExpression :: Parser Exp
primaryExpression =
        intLiteral
    <|> booleanLiteral
    <|> VarExp <$> identifier
    <|> ThisExp <$ reserved "this"
    <|> try (NewIntArrayExp <$> (reserved "new" *> reserved "int" *> brackets expression))
    <|> NewObjectExp <$> (reserved "new" *> identifier <* parens nothing)
    <|> parens expression

intLiteral :: Parser Exp
intLiteral = IntLiteral <$> (fromIntegral <$> integer)

booleanLiteral :: Parser Exp
booleanLiteral =
        BooleanLiteral False <$ reserved "false"
    <|> BooleanLiteral True <$ reserved "true"

nothing :: Parser ()
nothing = return ()
