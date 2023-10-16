module AST where

data Expr
    = N Int
    | B Bool
    | Unit
    | Var Variable
    | App Expr Expr
    | Abs Variable LevelT Expr
    | Seq Expr Expr
    | Let Variable LevelT Expr
    | BO BinOper Expr Expr
    | Rec [(Label, Expr)]
    | Proj Expr Label
    | IfThenElse Expr Expr Expr
    | IfThen Expr Expr
    | While Expr Expr
    | For Variable Expr Expr Expr
    | Loc Location
    | Ref Expr
    | Deref Expr
    | Assign Variable Expr
    deriving (Eq, Show)

newtype Variable = V String deriving (Eq, Ord)

instance Show Variable where
    show (V x) = x

newtype Location = L Int deriving (Eq)

instance Show Location where
    show (L l) = show l

data Label
    = LabelS String
    | LabelI Int
    deriving (Eq, Show)

data BinOper = Add | Sub | Mul | Div
    deriving (Eq, Show)

data LevelT
    = TInt Int
    | TAbs LevelT LevelT
    deriving (Eq)

instance Show LevelT where
    show (TInt n) = show n
    show (TAbs l1 l2) = show l1 ++ "->" ++ show l2

instance Ord LevelT where
    compare (TInt n) (TInt m) = compare n m
    compare _ _ = undefined
