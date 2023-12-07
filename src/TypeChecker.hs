module TypeChecker (
    check,
) where

import AST
import Control.Monad.State.Lazy
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Env
import StateEither

elookup :: (Ord k, Show k) => k -> M.Map k a -> StateEither [String] a
elookup var env = case M.lookup var env of
    Just t -> return t
    Nothing -> do
        let msg = "Variable " ++ show var ++ " not found in environment"
        modify (++ [msg])
        fail msg

nelookup :: (Eq a, Show a) => a -> NE.NonEmpty (a, b) -> StateEither [String] b
nelookup a xs = case lookup a $ NE.toList xs of
    Just b -> return b
    Nothing -> do
        let msg = "Label " ++ show a ++ " not found in record"
        modify (++ [msg])
        fail msg

sat :: Bool -> String -> StateEither [String] ()
sat True _ = return ()
sat False e = do
    modify (++ [e])
    fail e

trace :: String -> StateEither [String] a -> StateEither [String] a
trace s m = do
    t <- get
    put $ t ++ [s]
    a <- m
    put t
    return a

check :: Env -> LevelT -> Expr -> StateEither [String] (Env, LevelT, LevelT)
check env pc expr = case expr of
    (LetInf x e) -> trace ("LetInf: " ++ show expr) $ do
        (_, l, eff) <- check env pc e
        case l of
            t1 :-> (t2 :@ eff') -> do
                sat (pc == Low) "NotSat: pc == Low"
                return (M.insert x (t1 :-> (t2 :@ eff')) env, Low, eff)
            t1 -> do
                sat (t1 `elem` [Low, High]) "NotSat: t1 `elem` [Low, High]"
                sat (pc <= t1) "NotSat: pc <= t1"
                return (M.insert x t1 env, Low, max t1 eff)
    (Let x t1 e) -> trace ("Let: " ++ show expr) $ do
        (_, t2, t3) <- check env pc e
        sat (t1 `elem` [Low, High]) "NotSat: t1 `elem` [Low, High]"
        sat (t2 `elem` [Low, High]) "NotSat: t2 `elem` [Low, High]"
        sat (t2 <= t1) "NotSat: t2 <= t1"
        sat (pc <= t1) "NotSat: pc <= t1"
        return (M.insert x t1 env, Low, min t1 t3)
    (IfThenElse e1 e2 e3) -> trace ("IfThenElse: " ++ show expr) $ do
        (_, l1, eff1) <- check env pc e1
        sat (l1 `elem` [Low, High]) "NotSat: l1 `elem` [Low, High]"
        let pc' = max l1 pc
        (_, l2, eff2) <- check env pc' e2
        (_, l3, eff3) <- check env pc' e3
        return (env, maximum [l1, l2, l3], minimum [eff1, eff2, eff3])
    (Seq e1 e2) -> trace ("Seq: " ++ show expr) $ do
        (env1, _, eff1) <- check env pc e1
        (env2, l2, eff2) <- check env1 pc e2
        return (env2, l2, min eff1 eff2)
    (While e1 e2) -> trace ("While: " ++ show expr) $ do
        (_, l1, eff1) <- check env pc e1
        sat (l1 `elem` [Low, High]) "NotSat: l1 `elem` [Low, High]"
        let pc' = max l1 pc
        (_, _, eff2) <- check env pc' e2
        return (env, Low, min eff1 eff2)
    (For x e1 e2 e3) -> trace ("For: " ++ show expr) $ do
        (_, l1, eff1) <- check env pc e1
        sat (l1 `elem` [Low, High]) "NotSat: l1 `elem` [Low, High]"
        (_, l2, eff2) <- check env pc e2
        sat (l2 `elem` [Low, High]) "NotSat: l2 `elem` [Low, High]"
        let pc' = maximum [l1, l2, pc]
        (_, _, eff3) <- check (M.insert x pc' env) pc' e3
        return (env, Low, minimum [eff1, eff2, eff3])
    (Var x) -> trace ("Var: " ++ show expr) $ do
        l <- elookup x env
        return (env, l, Empty)
    (B _) -> trace ("Bool: " ++ show expr) $ do
        return (env, Low, Empty)
    (N _) -> trace ("Num: " ++ show expr) $ do
        return (env, Low, Empty)
    Unit -> trace ("Unit: " ++ show expr) $ do
        return (env, Low, Empty)
    (Abs x l e) -> trace ("Abs: " ++ show expr) $ do
        (_, l', eff') <- check (M.insert x l env) pc e
        sat (pc <= eff') "NotSat: pc <= eff'"
        return (env, l :-> (l' :@ eff'), eff')
    (App e1 e2) -> trace ("App: " ++ show expr) $ do
        (_, l1, eff1) <- check env pc e1
        (_, l2, eff2) <- check env pc e2
        case l1 of
            (l1' :-> (l2' :@ eff3)) -> do
                let eff = minimum [eff1, eff2, eff3]
                sat (eff >= pc) "NotSat: eff >= pc"
                sat (l1' == l2) "NotSat: l1' == l2"
                return (env, l2', eff)
            t -> fail $ "App: not a function type: " ++ show t
    (BO _ e1 e2) -> trace ("BO: " ++ show expr) $ do
        (_, l1, eff1) <- check env pc e1
        (_, l2, eff2) <- check env pc e2
        sat (l1 `elem` [Low, High]) "NotSat: l1 `elem` [Low, High]"
        sat (l2 `elem` [Low, High]) "NotSat: l2 `elem` [Low, High]"
        return (env, max l1 l2, min eff1 eff2)
    (IfThen {}) -> undefined
    (Rec {}) -> undefined
    (Proj {}) -> undefined
    (Loc _) -> undefined
    (Ref _) -> undefined
    (Deref _) -> undefined
    (Assign {}) -> undefined
