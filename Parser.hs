module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Prim as P
import qualified Text.ParserCombinators.Parsec.Token as Token

main :: IO ()
main = parseFile "examples/Arg.java" >>= print

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

data Program = Program MainClass [Class] deriving (Show)

data MainClass = MainClass Statement deriving (Show)

data Class = Class String (Maybe String) [VarDeclaration] [MethodDeclaration] deriving (Show)

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
    | AssignExpression Expression Expression
    | BinaryExpression Expression String Expression
    | NotExp Expression
    | IndexExp Expression Expression
    | CallExp Expression String [Expression]
    | MemberExp Expression String
    | VarExp String
    | ThisExp
    | NewIntArrayExp Expression
    | NewObjectExp String
    deriving (Show)

data Type =
      BooleanType
    | IntType
    | IntArrayType
    | ObjectType String
    deriving (Show)

data Parameter = Parameter Type String deriving (Show)

data VarDeclaration = VarDeclaration Type String deriving (Show)

data MethodDeclaration = MethodDeclaration Type String [Parameter] [VarDeclaration] [Statement] Expression deriving (Show)

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
