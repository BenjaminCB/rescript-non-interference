module AST where

data Expr = N Int
          | B Bool
          | Unit
          | Var Variable
          | App Expr Expr
          | Abs Variable LevelT Expr
          | Seq Expr Expr
          | Let Variable LevelT Expr
          deriving (Eq, Show)
