module Progs (bindLowToHigh) where

import AST

bindLowToHigh :: Expr
bindLowToHigh = Seq high low
    where
        high = Let (V "h") (TInt 1) $ N 1
        low = Let (V "l") (TInt 0) (Var $ V "h")
