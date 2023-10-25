module Main where

import AST
import Control.Monad.State.Lazy
import Data.Map qualified as M
import Progs
import Prooftree
import StateEither
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
    checkDefault implicitFlow

checkLevel :: Expr -> Int -> IO ()
checkLevel e n = do
    print e
    putStrLn $ "Initial program counter: " ++ show n
    let res = runStateEither (check M.empty (TInt n) e) []
    case res of
        Left (err, trace) -> do
            putStrLn "Error:"
            print err
            putStrLn "Trace:"
            mapM_ putStrLn trace
        Right (t, _) -> do
            putStrLn "State:"
            print t
    putStrLn ""

checkDefault :: Expr -> IO ()
checkDefault = flip checkLevel 0
