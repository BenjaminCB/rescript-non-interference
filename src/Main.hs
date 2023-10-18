module Main where

import AST
import Data.Map qualified as M
import Progs
import TypeChecker

main :: IO ()
main = do
    checkDefault one
    checkLevel (bindX 0) 1
    checkLevel (bindX 1) 1
    checkLevel (bindX 1) 0
    checkDefault bindLowToHigh
    checkDefault bindHighToLowRef
    checkDefault bindLowToHighRef
    checkDefault forLoopAccum
    checkDefault assignInFunction
    checkDefault whileLoop
    checkDefault ifThen
    checkDefault abstraction
    checkDefault abstraction1
    checkDefault abstraction2
    checkDefault abstraction3

checkLevel :: Expr -> Int -> IO ()
checkLevel e n = do
    print e
    putStrLn $ "Initial program counter: " ++ show n
    print $ check M.empty (TInt n) e
    putStrLn ""

checkDefault :: Expr -> IO ()
checkDefault = flip checkLevel 0
