module Progs (
    one,
    bindX,
    bindLowToHigh,
    bindHighToLowRef,
    bindLowToHighRef,
    forLoopAccum,
    assignInFunction,
    assignInFunction2,
    assignInFunction3,
    whileLoop,
    ifThen,
    abstraction,
    abstraction1,
    abstraction2,
    abstraction3,
    ifHighThenLow,
    nestedBO,
    nestedBO2,
    implicitFlow,
    bindAndProjectRecord1,
    bindAndProjectRecord2,
    bindAndProjectRecord3,
    bindAndProjectRecord4,
    ifInFunc,
    appInIf,
    okIf,
) where

import AST
import Data.List.NonEmpty (NonEmpty (..))
import Prelude hiding (seq)

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
abstraction2 = Let (V "func") (TInt 0 --> TInt 0) $ Abs (V "x") (TInt 0) (BO Add (var "x") (N 1))

abstraction3 :: Expr
abstraction3 = Let (V "func") (TInt 0 --> TInt 0 --> TInt 0) $ Abs (V "x") (TInt 0) (BO Add (var "x") (N 1))

assignInFunction :: Expr
assignInFunction = seq [h, l, func, application]
    where
        h = high . Ref . N $ 1
        l = low . Ref . N $ 0
        func =
            Let
                (V "func")
                (TInt 0 --> TInt 0 --> TInt 0)
                $ Abs
                    (V "x")
                    (TInt 0)
                    (Abs (V "y") (TInt 0) (Assign (V "x") (Ref $ var "y")))
        application = App (App (var "func") (var "l")) (var "h")

-- Assign high param to low variable defined outside the function body. (Should fail)
assignInFunction2 :: Expr
assignInFunction2 = seq [h, l, func, application]
    where
        h = high . Ref . N $ 1
        l = low . Ref . N $ 0
        func =
            Let
                (V "func")
                (TInt 1 --> TInt 0)
                $ Abs
                    (V "x")
                    (TInt 0)
                    (Assign (V "l") (Ref $ var "x"))
        application = App (var "func") (var "h")

-- Assign number to low variable defined outside the function body and calling it in a high context. (Should fail)
assignInFunction3 :: Expr
assignInFunction3 = seq [h, l, func, ifstmt]
    where
        h = high . B $ True
        l = low . Ref . N $ 0
        func =
            Let
                (V "func")
                (TInt 0 --> TInt 0)
                $ Abs
                    (V "x")
                    (TInt 0)
                    (seq [Assign (V "l") (N 3), var "x"])
        application = App (var "func") (N 2)
        ifstmt = IfThenElse (var "h") application (N 3)

ifInFunc :: Expr
ifInFunc = seq [h, l, func, application]
    where
        h = high . B $ True
        l = low . Ref . N $ 0
        func =
            Let
                (V "func")
                (TInt 1 --> TInt 0)
                $ Abs
                    (V "x")
                    (TInt 1)
                    (IfThen (var "x") (Assign (V "l") (N 1)))
        application = App (var "func") (var "h")

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

nestedBO :: Expr
nestedBO = BO Add (BO Mul (N 1) (N 2)) (BO Sub (N 3) (N 4))

nestedBO2 :: Expr
nestedBO2 = BO Add (BO Mul (N 1) (N 2)) (BO Sub (var "x") (N 4))

implicitFlow :: Expr
implicitFlow = seq [h1, h2, l, ifT]
    where
        h1 = Let (V "h1") (TInt 1) (N 1)
        h2 = Let (V "h2") (TInt 1) (N 1)
        l = Let (V "l") (TInt 0) (Ref $ N 0)
        ifT = IfThenElse (BO Eq (var "h1") (var "h2")) (Assign (V "l") (N 1)) (Assign (V "l") (N 2))

bindAndProjectRecord1 :: Expr
bindAndProjectRecord1 = seq [record, proj]
    where
        t = TRec $ (LabelS "x", TInt 0) :| [(LabelS "y", TInt 1)]
        r = Rec $ (LabelS "x", N 1) :| [(LabelS "y", N 2)]
        record = Let (V "record") t r
        proj = Proj (var "record") (LabelS "x")

bindAndProjectRecord2 :: Expr
bindAndProjectRecord2 = seq [record, proj]
    where
        t = TRec $ (LabelS "x", TInt 0) :| [(LabelS "y", TInt 1)]
        r = Rec $ (LabelS "x", N 1) :| [(LabelS "y", N 2)]
        record = Let (V "record") t r
        proj = Proj (var "record") (LabelS "y")

bindAndProjectRecord3 :: Expr
bindAndProjectRecord3 = seq [record, proj]
    where
        t = TRec $ (LabelS "y", TInt 0) :| [(LabelS "x", TInt 1)]
        r = Rec $ (LabelS "y", N 1) :| [(LabelS "x", N 2)]
        record = Let (V "record") t r
        proj = Proj (var "record") (LabelS "y")

bindAndProjectRecord4 :: Expr
bindAndProjectRecord4 = seq [record, proj]
    where
        t = TRec $ (LabelS "y", TInt 0) :| [(LabelS "x", TInt 1)]
        r = Rec $ (LabelS "y", N 1) :| [(LabelS "x", N 2)]
        record = Let (V "record") t r
        proj = Proj (var "record") (LabelS "x")

appInIf :: Expr
appInIf = seq [h, l, func, ifT]
    where
        h = high . Ref . B $ True
        l = low . N $ 2
        func =
            Let
                (V "func")
                (TInt 0 --> TInt 0)
                $ Abs
                    (V "x")
                    (TInt 0)
                    (Seq (Assign (V "l") (N 3)) (var "x"))
        ifT = IfThenElse (Deref $ var "h") (App (var "func") (var "l")) (N 3)

okIf :: Expr
okIf = seq [h, ifTE]
    where
        h = high . B $ True
        ifTE = IfThenElse (var "h") (N 2) (N 3)
