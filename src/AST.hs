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

data Sec
    = Low
    | High
    deriving (Eq, Ord)

instance Show Sec where
    show High = "H"
    show Low = "L"

data LevelT
    = TSec Sec
    | TAbs LevelT LevelT
    | TRec (NE.NonEmpty (Label, LevelT))
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
    show (TSec s) = show s
    show (TAbs l1 l2) = show l1 ++ "->" ++ show l2
    show (TRec fs) = "{" ++ intercalate ", " (map (\(a, b) -> show a ++ ": " ++ show b) $ NE.toList fs) ++ "}"
    show (TEffect l1 l2) = show l1 ++ "@" ++ show l2
    show TEmpty = "()"

instance Ord LevelT where
    compare (TSec s1) (TSec s2) = compare s1 s2
    compare (TAbs x1 y1) (TAbs x2 y2) = case (compare x1 x2, compare y1 y2) of
        (LT, LT) -> error "(LT, LT) in comparison of TAbs and TAbs"
        (GT, GT) -> error "(GT, GT) in comparison of TAbs and TAbs"
        (EQ, EQ) -> EQ
        (LT, _) -> LT
        (EQ, GT) -> LT
        (GT, _) -> GT
        (EQ, LT) -> GT
    compare (TRec fs) (TRec fs') = case (all (`elem` fs') fs, all (`elem` fs) fs') of
        (True, True) -> EQ
        (True, False) -> LT
        (False, True) -> GT
        (False, False) -> error "incomparable TRec and TRec"
    compare l@(TSec _) (TRec fs) = compare l (maximum . map snd $ NE.toList fs)
    compare (TRec fs) l@(TSec _) = compare (maximum . map snd $ NE.toList fs) l
    compare TEmpty TEmpty = EQ
    compare TEmpty _ = LT
    compare _ TEmpty = GT
    compare t t' = error $ "incomparable types: " ++ show t ++ " and " ++ show t'
