-- TODO try using Parsec's operator table
module Parser where

import Text.Parsec hiding ((<|>), many, try)
import Text.Parsec.Prim hiding ((<|>), many)
import Text.Parsec.String
import Text.Parsec.Expr
import AST
import Data.Functor
import Data.Functor.Identity
import Control.Applicative
import Lexer

parseString :: String -> Program
parseString str = case parse program "" str of
    Left e  -> error $ show e
    Right r -> r

program :: Parser Program
program = Program <$> (whiteSpace *> mainClass) <*> many normalClass <* eof

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
        BlockStatement <$> braces (many statement)
    <|> IfStatement <$> (reserved "if" *> parens expression) <*> statement <*> optionMaybe (reserved "else" >> statement)
    <|> WhileStatement <$> (reserved "while" *> parens expression) <*> statement
    <|> PrintStatement <$> (reserved "System.out.println" *> parens expression <* semi)
    <|> ExpressionStatement <$> expression <* semi

expression :: Parser Exp
expression = buildExpressionParser operatorTable primaryExpression

operatorTable :: OperatorTable String () Identity Exp
operatorTable =
    [ [ Postfix (flip IndexExp <$> brackets expression)
      , Postfix (try $ (\m as o -> CallExp o m as) <$ symbol "." <*> identifier <*> parens (expression `sepBy` comma))
      , Postfix (flip MemberExp <$ symbol "." <*> identifier)
      ]
    , [ Prefix (NotExp <$ symbol "!") ]
    , [ Infix (binaryOps ["*", "/", "%"]) AssocLeft ]
    , [ Infix (binaryOps ["+", "-"]) AssocLeft ]
    , [ Infix (binaryOps ["<=", "<", ">=", ">"]) AssocLeft ]
    , [ Infix (binaryOps ["==", "!="]) AssocLeft ]
    , [ Infix (binaryOps ["&&"]) AssocLeft ]
    , [ Infix (binaryOps ["||"]) AssocLeft ]
    , [ Infix (AssignExpression <$ symbol "=") AssocRight ]
    ]

binaryOps ss = (\s e1 e2 -> BinaryExpression e1 s e2) <$> foldr1 (<|>) (map (try . symbol) ss)

primaryExpression :: Parser Exp
primaryExpression =
        IntLiteral . fromIntegral <$> integer
    <|> BooleanLiteral False <$ reserved "false"
    <|> BooleanLiteral True <$ reserved "true"
    <|> VarExp <$> identifier
    <|> ThisExp <$ reserved "this"
    <|> try (NewIntArrayExp <$> (reserved "new" *> reserved "int" *> brackets expression))
    <|> NewObjectExp <$> (reserved "new" *> identifier <* parens nothing)
    <|> parens expression

nothing :: Parser ()
nothing = return ()
