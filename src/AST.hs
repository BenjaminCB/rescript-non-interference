module AST where

data Expr = N Int
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

newtype Variable = V String
                 deriving (Eq, Ord, Show)

newtype Location = L Int
                 deriving (Eq, Show)

data Label = LabelS String
           | LabelI Int
           deriving (Eq, Show)

data BinOper = Add | Sub | Mul | Div
             deriving (Eq, Show)

data LevelT = TInt Int
            | TAbs LevelT LevelT
            deriving (Eq, Show)

instance Ord LevelT where
    compare _ _ = undefined
