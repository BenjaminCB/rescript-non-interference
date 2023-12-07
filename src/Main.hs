module Main where

import AST
import Data.Map qualified as M
import StateEither
import TypeChecker

main :: IO ()
main = putStrLn "Hello, Haskell!"

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
