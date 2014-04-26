module AST where

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
