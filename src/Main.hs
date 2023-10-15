module Main where

import AST
import TypeChecker
import qualified Data.Map as M

main :: IO ()
main = do
    print $ N 1
    print $ check M.empty (TInt 0) (N 1)
