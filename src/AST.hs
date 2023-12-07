module AST where

import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE

data Expr
    = N Int
    | B Bool
    | Unit
    | Var Variable
    | App Expr Expr
    | Abs Variable LevelT Expr
    | Seq Expr Expr
    | Let Variable LevelT Expr
    | LetInf Variable Expr
    | BO BinOper Expr Expr
    | Rec (NE.NonEmpty (Label, Expr))
    | Proj Expr Label
    | IfThenElse Expr Expr Expr
    | IfThen Expr Expr
    | While Expr Expr
    | For Variable Expr Expr Expr
    | Loc Location
    | Ref Expr
    | Deref Expr
    | Assign Variable Expr
    deriving (Eq)

instance Show Expr where
    show (N n) = show n
    show (B b) = show b
    show Unit = "()"
    show (Var x) = show x
    show (App e1 e2) = show e1 ++ "(" ++ show e2 ++ ")"
    show (Abs x t e) = "(\\" ++ show x ++ "." ++ show t ++ "->" ++ show e ++ ")"
    show (Seq e1 e2) = show e1 ++ ";" ++ show e2
    show (Let x t e) = "let " ++ show x ++ "." ++ show t ++ " = " ++ show e
    show (LetInf x e) = "let " ++ show x ++ " = " ++ show e
    show (BO op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (Rec fs) = "{" ++ intercalate ", " (NE.toList $ NE.map (\(a, b) -> show a ++ " = " ++ show b) fs) ++ "}"
    show (Proj e l) = show e ++ "." ++ show l
    show (IfThenElse e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3 ++ ")"
    show (IfThen e1 e2) = "(if " ++ show e1 ++ " then " ++ show e2 ++ ")"
    show (While e1 e2) = "(while " ++ show e1 ++ " do " ++ show e2 ++ ")"
    show (For x e1 e2 e3) = "for " ++ show x ++ " in " ++ show e1 ++ " to " ++ show e2 ++ " do " ++ show e3
    show (Loc l) = "<" ++ show l ++ ">"
    show (Ref e) = "*" ++ show e
    show (Deref e) = "!" ++ show e
    show (Assign x e) = show x ++ " := " ++ show e

newtype Variable = V String deriving (Eq, Ord)

instance Show Variable where
    show (V x) = x

newtype Location = L Int deriving (Eq)

instance Show Location where
    show (L l) = show l

data Label
    = LabelS String
    | LabelI Int
    deriving (Eq)

instance Show Label where
    show (LabelS s) = s
    show (LabelI i) = show i

data BinOper = Add | Sub | Mul | Div | Eq
    deriving (Eq)

instance Show BinOper where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Eq = "=="

data LevelT
    = Low
    | High
    | TAbs LevelT LevelT
    | TEffect LevelT LevelT
    | TEmpty
    deriving (Eq)

(@) :: LevelT -> LevelT -> LevelT
(@) = TEffect

infixr 5 @

(-->) :: LevelT -> LevelT -> LevelT
(-->) = TAbs

infixr 6 -->

instance Show LevelT where
    show Low = "L"
    show High = "H"
    show (TAbs l1 l2) = show l1 ++ "->" ++ show l2
    show (TEffect l1 l2) = show l1 ++ "@" ++ show l2
    show TEmpty = "()"

instance Ord LevelT where
    compare Low Low = EQ
    compare Low High = LT
    compare High Low = GT
    compare High High = EQ
    compare (TAbs _ n) (TAbs _ m) = compare n m
    compare TEmpty _ = GT
    compare _ TEmpty = LT
    compare (TEffect n _) m = compare n m
    compare n (TEffect m _) = compare n m
    compare n m = error $ "Cannot compare " ++ show n ++ " and " ++ show m
