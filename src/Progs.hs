module Progs (
    one,
    bindX,
    bindLowToHigh,
    bindHighToLowRef,
    bindLowToHighRef,
    forLoopAccum,
    assignInFunction,
    whileLoop,
    ifThen,
    abstraction,
    abstraction1,
    abstraction2,
    abstraction3,
    ifHighThenLow,
) where

import AST
import Prelude hiding (seq)

tabs :: [Int] -> LevelT
tabs = foldr1 TAbs . map TInt

high :: Expr -> Expr
high = Let (V "h") (TInt 1)

low :: Expr -> Expr
low = Let (V "l") (TInt 0)

seq :: [Expr] -> Expr
seq = foldl1 Seq

add :: Expr -> Expr -> Expr
add = BO Add

var :: String -> Expr
var = Var . V

one :: Expr
one = N 1

bindX :: Int -> Expr
bindX n = seq [Let (V "x") (TInt n) (N 1), var "x"]

bindLowToHigh :: Expr
bindLowToHigh = Seq h l
    where
        h = high $ N 1
        l = low . Var $ V "h"

bindHighToLowRef :: Expr
bindHighToLowRef = seq [l, h, assign]
    where
        l = low . Ref $ N 2
        h = high . Var $ V "l"
        assign = Assign (V "h") (N 4)

bindLowToHighRef :: Expr
bindLowToHighRef = seq [h, l, assign]
    where
        h = high . Ref $ N 2
        l = low . Var $ V "h"
        assign = Assign (V "h") (N 4)

forLoopAccum :: Expr
forLoopAccum = seq [h, l, forLoop]
    where
        h = high . N $ 5
        l = low . Ref . N $ 0
        forLoop = For (V "x") (N 1) (var "h") (Assign (V "l") (add (Deref $ var "l") (N 1)))

abstraction :: Expr
abstraction = Abs (V "x") (TInt 0) (BO Add (var "x") (N 1))


abstraction1 :: Expr
abstraction1 = Let (V "func") (TInt 0) $ Abs (V "x") (TInt 0) (BO Add (var "x") (N 1))

abstraction2 :: Expr
abstraction2 = Let (V "func") (tabs [0,0]) $ Abs (V "x") (TInt 0) (BO Add (var "x") (N 1))

abstraction3 :: Expr
abstraction3 = Let (V "func") (tabs [0,0,0]) $ Abs (V "x") (TInt 0) (BO Add (var "x") (N 1))



assignInFunction :: Expr
assignInFunction = seq [h, l, func, application]
    where
        h = high . Ref . N $ 1
        l = low . Ref . N $ 0
        func = Let (V "func") (tabs [0,0,0]) $ Abs (V "x") (TInt 0) (Abs (V "y") (TInt 0) (Assign (V "x") (Ref $ var "y")))
        application = App (App (var "func") (var "l")) (var "h")

whileLoop :: Expr
whileLoop = seq [h, l, while]
    where
        h = high . Ref . B $ True
        l = low . Ref . B $ False
        while = While (Deref $ var "h") (Assign (V "l") (B True))

ifThen :: Expr
ifThen = seq [h, l, ifT]
    where
        h = high . Ref . B $ True
        l = low . Ref . B $ False
        ifT = IfThen (Deref $ var "h") (Assign (V "l") (B True))

ifHighThenLow :: Expr
ifHighThenLow = seq [h, ifT2]
    where
        h = high . Ref . B $ True
        ifT2 = IfThenElse (Deref $ var "h") (N 2) (N 4)
