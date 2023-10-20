module Main where

import AST
import Data.Map qualified as M
import Progs
import StateEither
import Prooftree
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
    checkDefault ifHighThenLow
    checkDefault nestedBO
    checkDefault nestedBO2

checkLevel :: Expr -> Int -> IO ()
checkLevel e n = do
    print e
    putStrLn $ "Initial program counter: " ++ show n
    let res = runStateEither (check M.empty (TInt n) e) [Error "Initial state"]
    case res of
        Left (err, s) -> do
            putStrLn "Error:"
            print err
            putStrLn "ProofTree:"
            prettyPrintTree $ head s
        Right (t, s) -> do
            putStrLn "State:"
            print t
            putStrLn "ProofTree:"
            prettyPrintTree $ head s
    putStrLn ""

checkDefault :: Expr -> IO ()
checkDefault = flip checkLevel 0
