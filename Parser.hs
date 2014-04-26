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

pClass :: Parser ClassDecl
pClass = do
    reserved "class"
    name <- identifier
    extends <- optionMaybe (reserved "extends" >> identifier)
    braces $ do
        fields <- many pVarDecl
        methods <- many pMethodDecl
        return $ ClassDecl name extends fields methods

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
    condition <- parens pExp
    ifTrue <- pStatement
    ifFalse <- optionMaybe (reserved "else" >> pStatement)
    return $ IfStatement condition ifTrue ifFalse

pWhileStatement :: Parser Statement
pWhileStatement = do
    reserved "while"
    condition <- parens pExp
    s <- pStatement
    return $ WhileStatement condition s

pPrintStatement :: Parser Statement
pPrintStatement = do
    reserved "System.out.println"
    e <- parens pExp
    semi
    return $ PrintStatement e

pExpressionStatement :: Parser Statement
pExpressionStatement = do
    e <- pExp
    semi
    return $ ExpressionStatement e

pExp :: Parser Exp
pExp = pAssignExpression

-- TODO try cleaning this up with chainl1 and <|>
pAssignExpression :: Parser Exp
pAssignExpression = do
    target <- pLogicOp
    o <- optionMaybe (symbol "=" >> pAssignExpression)
    case o of
        Just value -> return $ AssignExpression target value
        Nothing -> return target

pLogicOp :: Parser Exp
pLogicOp = chainl1 pCmpOp ((symbol "&&" <|> symbol "||") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pCmpOp :: Parser Exp
pCmpOp = chainl1 pAddOp (foldr1 (<|>) (map (P.try . symbol) $ words "<= < == != >= >") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pAddOp :: Parser Exp
pAddOp = chainl1 pMulOp ((symbol "+" <|> symbol "-") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pMulOp :: Parser Exp
pMulOp = chainl1 pUnaryOp ((symbol "*" <|> symbol "/" <|> symbol "%") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pUnaryOp :: Parser Exp
pUnaryOp = (do
    symbol "!"
    e <- pUnaryOp
    return $ NotExp e)
    <|>
    pPostfixOp

pPostfixOp :: Parser Exp
pPostfixOp = do
    e <- pPrimaryExp
    ops <- many (pIndexPostfixOp <|> pCallPostfixOp <|> pMemberPostfixOp)
    return $ foldl (.) id ops $ e

pIndexPostfixOp :: Parser (Exp -> Exp)
pIndexPostfixOp = brackets $ do
    index <- pExp
    return $ \array -> IndexExp array index

pCallPostfixOp :: Parser (Exp -> Exp)
pCallPostfixOp = P.try $ do
    symbol "."
    method <- identifier
    args <- parens pArgs
    return $ \object -> CallExp object method args

pMemberPostfixOp :: Parser (Exp -> Exp)
pMemberPostfixOp = do
    symbol "."
    field <- identifier
    return $ \object -> MemberExp object field

pArgs :: Parser [Exp]
pArgs = chainr (pExp >>= \a -> return [a]) (comma >> return (++)) []

pPrimaryExp :: Parser Exp
pPrimaryExp =
        pIntLiteral
    <|> pBooleanLiteral
    <|> (identifier >>= return . VarExp)
    <|> (reserved "this" >> return ThisExp)
    <|> (P.try $ do
            reserved "new"
            reserved "int"
            size <- brackets pExp
            return $ NewIntArrayExp size)
    <|> (do
            reserved "new"
            name <- identifier
            parens empty
            return $ NewObjectExp name)
    <|> parens pExp

pIntLiteral :: Parser Exp
pIntLiteral = do
    i <- integer
    return $ IntLiteral (fromIntegral i)

pBooleanLiteral :: Parser Exp
pBooleanLiteral =
        (reserved "false" >> return (BooleanLiteral False))
    <|> (reserved "true" >> return (BooleanLiteral True))

pType :: Parser Type
pType =
        (P.try $ reserved "int" >> brackets empty >> return IntArrayType)
    <|> (reserved "boolean" >> return BooleanType)
    <|> (reserved "int" >> return IntType)
    <|> (identifier >>= return . ObjectType)

pVarDecl :: Parser VarDecl
pVarDecl = do
    t <- pType
    name <- identifier
    semi
    return $ VarDecl t name

pParams :: Parser [Parameter]
pParams = chainr (pParam >>= \p -> return [p]) (comma >> return (++)) []

pParam :: Parser Parameter
pParam = do
    t <- pType
    name <- identifier
    return $ Parameter t name

pMethodDecl :: Parser MethodDecl
pMethodDecl = do
    reserved "public"
    t <- pType
    name <- identifier
    params <- parens pParams
    braces $ do
        fields <- many $ P.try pVarDecl
        body <- many pStatement
        reserved "return"
        retExp <- pExp
        semi
        return $ MethodDecl t name params fields body retExp

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
comma      = Token.comma      lexer
symbol     = Token.symbol     lexer

empty :: Parser ()
empty = return ()
