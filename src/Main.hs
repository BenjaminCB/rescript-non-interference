module Main where

import AST
import Data.Map qualified as M
import StateEither
import TypeChecker
import System.Environment
import Parser

main :: IO ()
main = do
    (path:_) <- getArgs
    contents <- readFile path
    putStrLn "File Content: "
    mapM_ putStrLn $ lines contents
    putStrLn "Parsed Content: "
    case parseInput contents of
        Left err -> print err
        Right e -> do
            print e

checkLevel :: String -> Expr -> Int -> IO ()
checkLevel name e n = do
    print name
    print e
    putStrLn $ "Initial program counter: " ++ show n
    let res = runStateEither (check M.empty Low e) []
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
