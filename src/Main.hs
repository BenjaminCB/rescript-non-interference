module Main where

import AST
import Data.Map qualified as M
import Parser
import StateEither
import System.Environment
import TypeChecker

main :: IO ()
main = do
    (path : _) <- getArgs
    contents <- readFile path
    putStrLn "File Content:"
    mapM_ putStrLn $ lines contents
    putStrLn "Parsed Content:"
    case parseInput contents of
        Left err -> print err
        Right e -> do
            print e
            putStrLn "Type checking:"
            let res = runStateEither (check M.empty (LH Low) e) []
            case res of
                Left (err, trace) -> do
                    putStr $ "Error: " ++ show err
                    putStrLn ""
                    putStrLn "Trace:"
                    mapM_ (putStrLn . ("    " ++)) trace
                Right (t, _) -> do
                    putStrLn "State:"
                    print t
