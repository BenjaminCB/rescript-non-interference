module Test where

import AST
import Data.Map qualified as M
import Env
import Progs
import StateEither
import Test.HUnit
import Prooftree
import TypeChecker

checkN :: Expr -> Int -> Either String (Env, LevelT)
checkN e n = evalStateEither (check M.empty (TInt n) e) [T "root" []]

main :: IO ()
main = runTestTT tests >>= print

tests :: Test
tests =
    test
        [ "bindX 0" ~: Left "LetTInt: l < pc" ~=? checkN (bindX 0) 1
        , "bindX 1" ~: Right (M.fromList [(V "x", TInt 1)], TInt 1) ~=? checkN (bindX 1) 1
        , "bindX 2" ~: checkN (bindX 2) 0 ~=? Right (M.fromList [(V "x", TInt 2)], TInt 2)
        ]
