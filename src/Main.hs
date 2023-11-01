module Main where

import AST
import Data.Map qualified as M
import Progs
import StateEither
import TypeChecker

main :: IO ()
main = do
    -- checkDefault "one" one
    -- checkLevel "bindX 0" (bindX 0) 1
    -- checkLevel "bindX 1" (bindX 1) 1
    -- checkLevel "bindX 1" (bindX 1) 0
    checkDefault "bindLowToHigh" bindLowToHigh
    checkDefault "bindHighToLowRef" bindHighToLowRef
    checkDefault "bindLowToHighRef" bindLowToHighRef
    checkDefault "ifThen" ifThen
    checkDefault "whileLoop" whileLoop
    checkDefault "assignInFunction" assignInFunction
    checkDefault "ifInFunc" ifInFunc
    checkDefault "forLoopAccum" forLoopAccum
    checkDefault "appInIf" appInIf
    checkDefault "okIf" okIf
    -- checkDefault "ifHighThenLow" ifHighThenLow
    -- checkDefault "abstraction" abstraction
    -- checkDefault "abstraction1" abstraction1
    -- checkDefault "abstraction2" abstraction2
    -- checkDefault "abstraction3" abstraction3
    -- checkDefault "nestedBO" nestedBO
    -- checkDefault "nestedBO2" nestedBO2
    -- checkDefault "implicitFlow" implicitFlow
    -- checkDefault "assignInFunction2" assignInFunction2
    -- checkDefault "assignInFunction3" assignInFunction3
    -- checkDefault "bindAndProjectRecord1" bindAndProjectRecord1
    -- checkDefault "bindAndProjectRecord2" bindAndProjectRecord2
    -- checkDefault "bindAndProjectRecord3" bindAndProjectRecord3
    -- checkDefault "bindAndProjectRecord4" bindAndProjectRecord4

checkLevel :: String -> Expr -> Int -> IO ()
checkLevel name e n = do
    print name
    print e
    putStrLn $ "Initial program counter: " ++ show n
    let res = runStateEither (check M.empty (TInt n) e) []
    case res of
        Left (err, trace) -> do
            putStr $ "Error: " ++ show err
            putStrLn ""
            putStrLn "Trace:"
            mapM_ (putStrLn . ("    " ++)) trace
        Right (t, _) -> do
            putStrLn "State:"
            print t
    putStrLn ""

checkDefault :: String -> Expr -> IO ()
checkDefault name e = checkLevel name e 0
