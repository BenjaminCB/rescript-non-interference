module Main where

import AST
import Data.Map qualified as M
import Progs
import TypeChecker

main :: IO ()
main = do
    print $ N 1
    print $ check M.empty (TInt 0) (N 1)
    print bindLowToHigh
    print $ check M.empty (TInt 0) bindLowToHigh
