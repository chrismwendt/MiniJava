module Parser where

import Text.Parsec hiding ((<|>), many, try)
import Text.Parsec.Prim hiding ((<|>), many)
import Text.Parsec.String
import Text.Parsec.Expr
import Data.Functor
import Data.Functor.Identity
import Control.Applicative
import Data.Maybe
import Lexer
import AST
import Control.Monad.Except
import Control.Error.Util (hoistEither)
import Data.Bifunctor

parseString :: String -> Except String Program
parseString str = hoistEither $ first show $ parse program "" str

program :: Parser Program
program = Program <$> (whiteSpace *> mainClass) <*> many normalClass <* eof

mainClass :: Parser Class
mainClass = do
    reserved "class"
    name <- identifier
    braces $ do
        reserved "public" >> reserved "static" >> reserved "void" >> reserved "main"
        parens $ symbol "String" >> brackets nothing >> identifier
        braces $ do
            s <- statement
            return $ Class name "Object" [] [Method TypeInt "main" [] [] [s] (LiteralInt 0)]

normalClass :: Parser Class
normalClass = do
    reserved "class"
    name <- identifier
    extends <- optionMaybe (reserved "extends" >> identifier)
    braces $ Class name (fromMaybe "Object" extends) <$> many variable <*> many method

typeSpecifier :: Parser Type
typeSpecifier =
        try (TypeIntArray <$ reserved "int" <* brackets nothing)
    <|> TypeBoolean <$ reserved "boolean"
    <|> TypeInt <$ reserved "int"
    <|> TypeObject <$> identifier

variable :: Parser Variable
variable = Variable <$> typeSpecifier <*> identifier <* semi

method :: Parser Method
method = do
    reserved "public"
    t <- typeSpecifier
    name <- identifier
    params <- parens $ parameter `sepBy` comma
    braces $ Method t name params <$> many (try variable) <*> many statement <*> (reserved "return" *> expression) <* semi

parameter :: Parser Variable
parameter = Variable <$> typeSpecifier <*> identifier

statement :: Parser Statement
statement =
        Block <$> braces (many statement)
    <|> If <$> (reserved "if" *> parens expression) <*> statement <*> optionMaybe (reserved "else" >> statement)
    <|> While <$> (reserved "while" *> parens expression) <*> statement
    <|> Print <$> (reserved "System.out.println" *> parens expression <* semi)
    <|> ExpressionStatement <$> expression <* semi

expression :: Parser Expression
expression = buildExpressionParser operatorTable primary

operatorTable :: OperatorTable String () Identity Expression
operatorTable =
    [ [ Postfix (flip IndexGet <$> brackets expression)
      , Postfix (try $ (\m as o -> Call o m as) <$ symbol "." <*> identifier <*> parens (expression `sepBy` comma))
      , Postfix (flip MemberGet <$ symbol "." <*> identifier)
      ]
    , [ Prefix (Not <$ symbol "!") ]
    , [ Infix (binaryOps [("*", Mul), ("/", Div), ("%", Mod)]) AssocLeft ]
    , [ Infix (binaryOps [("+", Plus), ("-", Minus)]) AssocLeft ]
    , [ Infix (binaryOps [("<=", Le), ("<", Lt), (">=", Ge), (">", Gt)]) AssocLeft ]
    , [ Infix (binaryOps [("==", Eq), ("!=", Ne)]) AssocLeft ]
    , [ Infix (binaryOps [("&&", And)]) AssocLeft ]
    , [ Infix (binaryOps [("||", Or)]) AssocLeft ]
    , [ Infix (Assignment <$ symbol "=") AssocRight ]
    ]

binaryOps :: [(String, BinaryOperator)] -> Parser (Expression -> Expression -> Expression)
binaryOps ops = foldr1 (<|>) $ map (\(s, op) -> (\e1 e2 -> Binary e1 op e2) <$ try (symbol s)) ops

primary :: Parser Expression
primary =
        LiteralInt . fromIntegral <$> integer
    <|> (   LiteralBoolean False <$ reserved "false"
        <|> LiteralBoolean True <$ reserved "true"
        )
    <|> VariableGet <$> identifier
    <|> This <$ reserved "this"
    <|> try (NewIntArray <$> (reserved "new" *> reserved "int" *> brackets expression))
    <|> NewObject <$> (reserved "new" *> identifier <* parens nothing)
    <|> parens expression

nothing :: Parser ()
nothing = return ()
