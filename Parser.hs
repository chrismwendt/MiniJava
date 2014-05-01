-- TODO try using Parsec's operator table
module Parser where

import System.Environment
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec hiding ((<|>))
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Prim as P
import qualified Text.ParserCombinators.Parsec.Token as Token
import AST
import Data.Functor
import Control.Applicative
import Lexer

parseString :: String -> Program
parseString str = case parse pProgram "" str of
    Left e  -> error $ show e
    Right r -> r

pProgram :: Parser Program
pProgram = Program <$> (whiteSpace *> pMainClass) <*> P.many pClass

pMainClass :: Parser Statement
pMainClass = reserved "class" >> identifier >> braces (do
    reserved "public" >> reserved "static" >> reserved "void" >> reserved "main"
    parens $ symbol "String" >> brackets pEmpty >> identifier
    braces pStatement)

pClass :: Parser ClassDecl
pClass = do
    reserved "class"
    name <- identifier
    extends <- optionMaybe (reserved "extends" >> identifier)
    braces $ ClassDecl name extends <$> P.many pVarDecl <*> P.many pMethodDecl

pStatement :: Parser Statement
pStatement =
          pBlockStatement
    <|> pIfStatement
    <|> pWhileStatement
    <|> pPrintStatement
    <|> pExpressionStatement

pBlockStatement :: Parser Statement
pBlockStatement = BlockStatement <$> braces (P.many pStatement)

pIfStatement :: Parser Statement
pIfStatement = IfStatement <$> (reserved "if" *> parens pExp) <*> pStatement <*> optionMaybe (reserved "else" >> pStatement)

pWhileStatement :: Parser Statement
pWhileStatement = WhileStatement <$> (reserved "while" *> parens pExp) <*> pStatement

pPrintStatement :: Parser Statement
pPrintStatement = PrintStatement <$> (reserved "System.out.println" *> parens pExp <* semi)

pExpressionStatement :: Parser Statement
pExpressionStatement = ExpressionStatement <$> pExp <* semi

pExp :: Parser Exp
pExp = pAssignExpression

pAssignExpression :: Parser Exp
pAssignExpression = do
    target <- pLogicOp
    o <- optionMaybe (symbol "=" >> pAssignExpression)
    return $ case o of
        Just value -> AssignExpression target value
        Nothing -> target

pLogicOp :: Parser Exp
pLogicOp = chainl1 pCmpOp ((symbol "&&" <|> symbol "||") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pCmpOp :: Parser Exp
pCmpOp = chainl1 pAddOp (foldr1 (<|>) (map (P.try . symbol) $ words "<= < == != >= >") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pAddOp :: Parser Exp
pAddOp = chainl1 pMulOp ((symbol "+" <|> symbol "-") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pMulOp :: Parser Exp
pMulOp = chainl1 pUnaryOp ((symbol "*" <|> symbol "/" <|> symbol "%") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pUnaryOp :: Parser Exp
pUnaryOp = pNotExp <|> pPostfixOp

pNotExp :: Parser Exp
pNotExp = NotExp <$ symbol "!" <*> pUnaryOp

pPostfixOp :: Parser Exp
pPostfixOp = foldl (flip ($)) <$> pPrimaryExp <*> P.many (pIndexPostfixOp <|> pCallPostfixOp <|> pMemberPostfixOp)

pIndexPostfixOp :: Parser (Exp -> Exp)
pIndexPostfixOp = flip IndexExp <$> brackets pExp

pCallPostfixOp :: Parser (Exp -> Exp)
pCallPostfixOp = P.try $ (\m as o -> CallExp o m as) <$ symbol "." <*> identifier <*> parens pArgs

pMemberPostfixOp :: Parser (Exp -> Exp)
pMemberPostfixOp = flip MemberExp <$ symbol "." <*> identifier

pArgs :: Parser [Exp]
pArgs = pExp `sepBy` comma

pPrimaryExp :: Parser Exp
pPrimaryExp =
          pIntLiteral
    <|> pBooleanLiteral
    <|> VarExp <$> identifier
    <|> ThisExp <$ reserved "this"
    <|> P.try (NewIntArrayExp <$> (reserved "new" *> reserved "int" *> brackets pExp))
    <|> NewObjectExp <$> (reserved "new" *> identifier <* parens pEmpty)
    <|> parens pExp

pIntLiteral :: Parser Exp
pIntLiteral = IntLiteral <$> (fromIntegral <$> integer)

pBooleanLiteral :: Parser Exp
pBooleanLiteral =
          BooleanLiteral False <$ reserved "false"
    <|> BooleanLiteral True <$ reserved "true"

pType :: Parser Type
pType =
          P.try (IntArrayType <$ reserved "int" <* brackets pEmpty)
    <|> BooleanType <$ reserved "boolean"
    <|> IntType <$ reserved "int"
    <|> ObjectType <$> identifier

pVarDecl :: Parser VarDecl
pVarDecl = VarDecl <$> pType <*> identifier <* semi

pParameters :: Parser [Parameter]
pParameters = pParameter `sepBy` comma

pParameter :: Parser Parameter
pParameter = Parameter <$> pType <*> identifier

pMethodDecl :: Parser MethodDecl
pMethodDecl = do
    reserved "public"
    t <- pType
    name <- identifier
    params <- parens pParameters
    braces $ MethodDecl t name params <$> P.many (P.try pVarDecl) <*> P.many pStatement <*> (reserved "return" *> pExp) <* semi

pEmpty :: Parser ()
pEmpty = return ()
