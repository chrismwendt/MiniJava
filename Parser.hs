module Parser where

import System.Environment
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Prim as P
import qualified Text.ParserCombinators.Parsec.Token as Token
import AST
import Data.Functor
import Control.Applicative

parseString :: String -> Program
parseString str = case parse pProgram "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> IO Program
parseFile file = do
    program <- readFile file
    case parse pProgram "" program of
        Left e  -> print e >> fail "parse error"
        Right r -> return r

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
    P.<|> pIfStatement
    P.<|> pWhileStatement
    P.<|> pPrintStatement
    P.<|> pExpressionStatement

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
pLogicOp = chainl1 pCmpOp ((symbol "&&" P.<|> symbol "||") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pCmpOp :: Parser Exp
pCmpOp = chainl1 pAddOp (foldr1 (P.<|>) (map (P.try . symbol) $ words "<= < == != >= >") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pAddOp :: Parser Exp
pAddOp = chainl1 pMulOp ((symbol "+" P.<|> symbol "-") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pMulOp :: Parser Exp
pMulOp = chainl1 pUnaryOp ((symbol "*" P.<|> symbol "/" P.<|> symbol "%") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pUnaryOp :: Parser Exp
pUnaryOp = pNotExp P.<|> pPostfixOp

pNotExp :: Parser Exp
pNotExp = NotExp <$ symbol "!" <*> pUnaryOp

pPostfixOp :: Parser Exp
pPostfixOp = foldl (flip ($)) <$> pPrimaryExp <*> P.many (pIndexPostfixOp P.<|> pCallPostfixOp P.<|> pMemberPostfixOp)

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
    P.<|> pBooleanLiteral
    P.<|> VarExp <$> identifier
    P.<|> ThisExp <$ reserved "this"
    P.<|> P.try (NewIntArrayExp <$> (reserved "new" *> reserved "int" *> brackets pExp))
    P.<|> NewObjectExp <$> (reserved "new" *> identifier <* parens pEmpty)
    P.<|> parens pExp

pIntLiteral :: Parser Exp
pIntLiteral = IntLiteral <$> (fromIntegral <$> integer)

pBooleanLiteral :: Parser Exp
pBooleanLiteral =
          BooleanLiteral False <$ reserved "false"
    P.<|> BooleanLiteral True <$ reserved "true"

pType :: Parser Type
pType =
          P.try (IntArrayType <$ reserved "int" <* brackets pEmpty)
    P.<|> BooleanType <$ reserved "boolean"
    P.<|> IntType <$ reserved "int"
    P.<|> ObjectType <$> identifier

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

languageDef = emptyDef
    { Token.commentStart    = "/*"
    , Token.commentEnd      = "*/"
    , Token.commentLine     = "//"
    , Token.identStart      = letter
    , Token.identLetter     = alphaNum P.<|> char '_'
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
comma      = Token.comma      lexer
symbol     = Token.symbol     lexer

pEmpty :: Parser ()
pEmpty = return ()
