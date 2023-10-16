module Main where

import AST
import TypeChecker
import qualified Data.Map as M

high :: Expr
high = Let (V "h") (TInt 1) $ N 1

low :: Expr
low = Let (V "l") (TInt 0) (Var $ V "h")

prog :: Expr
prog = Seq high low

main :: IO ()
main = do
    print $ N 1
    print $ check M.empty (TInt 0) (N 1)
    print prog
    print $ check M.empty (TInt 0) prog
