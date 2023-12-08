module TypeChecker (
    check,
) where

import AST
import Control.Monad.State.Lazy
import Data.Map qualified as M
import StateEither
import Algebra.Lattice

elookup :: (Ord k, Show k) => k -> M.Map k a -> StateEither [String] a
elookup var env = case M.lookup var env of
    Just t -> return t
    Nothing -> do
        let msg = "Variable " ++ show var ++ " not found in environment"
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

check :: Env -> LevelT -> Expr -> StateEither [String] LevelTEnv
check env pc expr = case expr of
    (LetInf x e) -> trace ("LetInf: " ++ show expr) $ do
        l :@ eff :|> _ <- check env pc e
        case l of
            t1 :-> (t2 :@ eff') -> do
                sat (pc == Low) "NotSat: pc == Low"
                return $ Low :@ eff :|> M.insert x (t1 :-> (t2 :@ eff')) env
            t1 -> do
                sat (t1 `elem` [Low, High]) "NotSat: t1 `elem` [Low, High]"
                sat (pc `joinLeq` t1) "NotSat: pc <= t1"
                return $ Low :@ (t1 /\ eff) :|> M.insert x t1 env
    (Let x t1 e) -> trace ("Let: " ++ show expr) $ do
        t2 :@ t3 :|> _ <- check env pc e
        sat (t1 `elem` [Low, High]) "NotSat: t1 `elem` [Low, High]"
        sat (t2 `elem` [Low, High]) "NotSat: t2 `elem` [Low, High]"
        sat (t2 `joinLeq` t1) "NotSat: t2 <= t1"
        sat (pc `joinLeq` t1) "NotSat: pc <= t1"
        return $ Low :@ (t1 /\ t3) :|> M.insert x t1 env
    (IfThenElse e1 e2 e3) -> trace ("IfThenElse: " ++ show expr) $ do
        l1 :@ eff1 :|> _ <- check env pc e1
        sat (l1 `elem` [Low, High]) "NotSat: l1 `elem` [Low, High]"
        let pc' = l1 \/ pc
        l2 :@ eff2 :|> _ <- check env pc' e2
        l3 :@ eff3 :|> _ <- check env pc' e3
        return $ joins [l1, l2, l3] :@ meets [eff1, eff2, eff3] :|> env
    (Seq e1 e2) -> trace ("Seq: " ++ show expr) $ do
        _ :@ eff1 :|> env1 <- check env pc e1
        l2 :@ eff2 :|> env2 <- check env1 pc e2
        return $ l2 :@ (eff1 /\ eff2) :|> env2
    (While e1 e2) -> trace ("While: " ++ show expr) $ do
        l1 :@ eff1 :|> _ <- check env pc e1
        sat (l1 `elem` [Low, High]) "NotSat: l1 `elem` [Low, High]"
        let pc' = l1 \/ pc
        _ :@ eff2 :|> _ <- check env pc' e2
        return $ Low :@ (eff1 /\ eff2) :|> env
    (For x e1 e2 e3) -> trace ("For: " ++ show expr) $ do
        l1 :@ eff1 :|> _ <- check env pc e1
        sat (l1 `elem` [Low, High]) "NotSat: l1 `elem` [Low, High]"
        l2 :@ eff2 :|> _ <- check env pc e2
        sat (l2 `elem` [Low, High]) "NotSat: l2 `elem` [Low, High]"
        let pc' = joins [l1, l2, pc]
        _ :@ eff3 :|> _ <- check (M.insert x pc' env) pc' e3
        return $ Low :@ meets [eff1, eff2, eff3] :|> env
    (Var x) -> trace ("Var: " ++ show expr) $ do
        l <- elookup x env
        return $ l :@ Empty :|> env
    (B _) -> trace ("Bool: " ++ show expr) $ do
        return $ Low :@ Empty :|> env
    (N _) -> trace ("Num: " ++ show expr) $ do
        return $ Low :@ Empty :|> env
    Unit -> trace ("Unit: " ++ show expr) $ do
        return $ Low :@ Empty :|> env
    (Abs x l e) -> trace ("Abs: " ++ show expr) $ do
        l' :@ eff' :|> _ <- check (M.insert x l env) pc e
        sat (pc `joinLeq` eff') "NotSat: pc <= eff'"
        return $ (l :-> (l' :@ eff')) :@ eff' :|> env
    (App e1 e2) -> trace ("App: " ++ show expr) $ do
        l1 :@ eff1 :|> _ <- check env pc e1
        l2 :@ eff2 :|> _ <- check env pc e2
        case l1 of
            (l1' :-> (l2' :@ eff3)) -> do
                let eff = meets [eff1, eff2, eff3]
                sat (pc `joinLeq` eff) "NotSat: eff >= pc"
                sat (l1' == l2) "NotSat: l1' == l2"
                return $ l2' :@ eff :|> env
            t -> fail $ "App: not a function type: " ++ show t
    (BO _ e1 e2) -> trace ("BO: " ++ show expr) $ do
        l1 :@ eff1 :|> _ <- check env pc e1
        l2 :@ eff2 :|> _ <- check env pc e2
        sat (l1 `elem` [Low, High]) "NotSat: l1 `elem` [Low, High]"
        sat (l2 `elem` [Low, High]) "NotSat: l2 `elem` [Low, High]"
        return $ (l1 \/ l2) :@ (eff1 /\ eff2) :|> env
    (IfThen {}) -> error "IfThen: not implemented"
    (Rec {}) -> error "Rec: not implemented"
    (Proj {}) -> error "Proj: not implemented"
    (Loc _) -> error "Loc: not implemented"
    (Ref _) -> error "Ref: not implemented"
    (Deref _) -> error "Deref: not implemented"
    (Assign {}) -> error "Assign: not implemented"
