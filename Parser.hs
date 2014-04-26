module Parser where

import System.Environment
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Prim as P
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Printf
import Data.List
import AST

soMany f as = concatMap (" " ++) $ map f as
sExpProgram (Program m cs) = printf "(%s %s%s)" "Program" (sExpMainClass m) (soMany sExpClass cs) :: String
sExpMainClass (MainClass s) = printf "(%s %s)" "Main" (sExpStatement s) :: String
sExpClass (Class name (Just extends) vs ms) = printf "(%s %s %s%s%s)" "ClassDecl" (show name) (show extends) (soMany sExpVarDeclaration vs) (soMany sExpMethodDeclaration ms) :: String
sExpClass (Class name Nothing vs ms) = printf "(%s %s %s%s%s)" "ClassDecl" (show name) "null" (soMany sExpVarDeclaration vs) (soMany sExpMethodDeclaration ms) :: String
sExpVarDeclaration (VarDeclaration t name) = printf "(%s %s %s)" "VarDecl" (sExpType t) (show name) :: String
sExpMethodDeclaration (MethodDeclaration t name ps vs ss ret) = printf "(%s %s %s (Parameters%s) (VarDecls%s) (Statements%s) (Return %s))" "MethodDecl" (sExpType t) (show name) (soMany sExpParameter ps) (soMany sExpVarDeclaration vs) (soMany sExpStatement ss) (sExpExpression ret) :: String
sExpParameter (Parameter t name) = printf "(%s %s %s)" "Parameter" (sExpType t) (show name) :: String
sExpType BooleanType = printf "(%s)" "TypeBoolean" :: String
sExpType IntType = printf "(%s)" "TypeInt" :: String
sExpType IntArrayType = printf "(%s)" "TypeIntArray" :: String
sExpType (ObjectType name) = printf "(Type %s)" (show name) :: String
sExpStatement (BlockStatement ss) = printf "(%s%s)" "BlockStatement" (soMany sExpStatement ss) :: String
sExpStatement (IfStatement e s (Just s2)) = printf "(%s %s %s %s)" "IfStatement" (sExpExpression e) (sExpStatement s) (sExpStatement s2) :: String
sExpStatement (IfStatement e s Nothing) = printf "(%s %s %s)" "IfStatement" (sExpExpression e) (sExpStatement s) :: String
sExpStatement (WhileStatement e s) = printf "(%s %s %s)" "WhileStatement" (sExpExpression e) (sExpStatement s) :: String
sExpStatement (PrintStatement e) = printf "(%s %s)" "PrintStatement" (sExpExpression e) :: String
sExpStatement (ExpressionStatement e) = printf "(%s %s)" "ExpStatement" (sExpExpression e) :: String
sExpExpression (IntLiteral v) = printf "(%s %s)" "int" (show v) :: String
sExpExpression (BooleanLiteral v) = printf "(%s %s)" "boolean" (if v then "true" else "false") :: String
sExpExpression (AssignExpression e1 e2) = printf "(%s %s %s)" "AssignExp" (sExpExpression e1) (sExpExpression e2) :: String
sExpExpression (BinaryExpression e1 op e2) = printf "(%s %s %s)" op (sExpExpression e1) (sExpExpression e2) :: String
sExpExpression (NotExp e1) = printf "(%s %s)" "NotExp" (sExpExpression e1) :: String
sExpExpression (IndexExp e1 e2) = printf "(%s %s %s)" "IndexExp" (sExpExpression e1) (sExpExpression e2) :: String
sExpExpression (CallExp e1 name args) = printf "(%s %s %s%s)" "CallExp" (sExpExpression e1) (show name) (soMany sExpExpression args) :: String
sExpExpression (MemberExp e1 name) = printf "(%s %s %s)" "MemberExp" (sExpExpression e1) (show name) :: String
sExpExpression (VarExp name) = printf "(%s %s)" "VarExp" (show name) :: String
sExpExpression ThisExp = printf "(%s)" "ThisExp" :: String
sExpExpression (NewIntArrayExp e1) = printf "(%s %s)" "NewIntArrayExp" (sExpExpression e1) :: String
sExpExpression (NewObjectExp e1) = printf "(%s %s)" "new" (show e1) :: String

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
    name <- identifier
    extends <- optionMaybe (reserved "extends" >> identifier)
    braces $ do
        fields <- many pVarDeclaration
        methods <- many pMethodDeclaration
        return $ Class name extends fields methods

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
pExpression = pAssignExpression

-- TODO try cleaning this up with chainl1 and <|>
pAssignExpression :: Parser Expression
pAssignExpression = do
    target <- pLogicOp
    o <- optionMaybe (symbol "=" >> pAssignExpression)
    case o of
        Just value -> return $ AssignExpression target value
        Nothing -> return target

pLogicOp :: Parser Expression
pLogicOp = chainl1 pCmpOp ((symbol "&&" <|> symbol "||") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pCmpOp :: Parser Expression
pCmpOp = chainl1 pAddOp (foldr1 (<|>) (map (P.try . symbol) $ words "<= < == != >= >") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pAddOp :: Parser Expression
pAddOp = chainl1 pMulOp ((symbol "+" <|> symbol "-") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pMulOp :: Parser Expression
pMulOp = chainl1 pUnaryOp ((symbol "*" <|> symbol "/" <|> symbol "%") >>= \op -> return (\e1 e2 -> BinaryExpression e1 op e2))

pUnaryOp :: Parser Expression
pUnaryOp = (do
    symbol "!"
    e <- pUnaryOp
    return $ NotExp e)
    <|>
    pPostfixOp

pPostfixOp :: Parser Expression
pPostfixOp = do
    e <- pPrimaryExp
    ops <- many (pIndexPostfixOp <|> pCallPostfixOp <|> pMemberPostfixOp)
    return $ foldl (.) id ops $ e

pIndexPostfixOp :: Parser (Expression -> Expression)
pIndexPostfixOp = brackets $ do
    index <- pExpression
    return $ \array -> IndexExp array index

pCallPostfixOp :: Parser (Expression -> Expression)
pCallPostfixOp = P.try $ do
    symbol "."
    method <- identifier
    args <- parens pArgs
    return $ \object -> CallExp object method args

pMemberPostfixOp :: Parser (Expression -> Expression)
pMemberPostfixOp = do
    symbol "."
    field <- identifier
    return $ \object -> MemberExp object field

pArgs :: Parser [Expression]
pArgs = chainr (pExpression >>= \a -> return [a]) (comma >> return (++)) []

pPrimaryExp :: Parser Expression
pPrimaryExp =
        pIntLiteral
    <|> pBooleanLiteral
    <|> (identifier >>= return . VarExp)
    <|> (reserved "this" >> return ThisExp)
    <|> (P.try $ do
            reserved "new"
            reserved "int"
            size <- brackets pExpression
            return $ NewIntArrayExp size)
    <|> (do
            reserved "new"
            name <- identifier
            parens empty
            return $ NewObjectExp name)
    <|> parens pExpression

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
        (P.try $ reserved "int" >> brackets empty >> return IntArrayType)
    <|> (reserved "boolean" >> return BooleanType)
    <|> (reserved "int" >> return IntType)
    <|> (identifier >>= return . ObjectType)

pVarDeclaration :: Parser VarDeclaration
pVarDeclaration = do
    t <- pType
    name <- identifier
    semi
    return $ VarDeclaration t name

pParams :: Parser [Parameter]
pParams = chainr (pParam >>= \p -> return [p]) (comma >> return (++)) []

pParam :: Parser Parameter
pParam = do
    t <- pType
    name <- identifier
    return $ Parameter t name

pMethodDeclaration :: Parser MethodDeclaration
pMethodDeclaration = do
    reserved "public"
    t <- pType
    name <- identifier
    params <- parens pParams
    braces $ do
        fields <- many $ P.try pVarDeclaration
        body <- many pStatement
        reserved "return"
        retExp <- pExpression
        semi
        return $ MethodDeclaration t name params fields body retExp

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
