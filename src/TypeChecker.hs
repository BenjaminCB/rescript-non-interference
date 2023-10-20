module TypeChecker (check) where

import AST
import Control.Monad.State.Lazy
import Data.Map qualified as M
import Env
import StateEither
import Tree

elookup :: Variable -> Env -> StateEither [Tree String] LevelT
elookup var env = case M.lookup var env of
    Just t -> return t
    Nothing -> fail $ "Variable " ++ show var ++ " not found in environment"

sat :: Bool -> String -> StateEither [Tree String] ()
sat True _ = return ()
sat False e = fail e

check :: Env -> LevelT -> Expr -> StateEither [Tree String] (Env, LevelT)
check env pc expr = case expr of
    (N n) -> do
        modify (T ("N " ++ show n) [] :)
        return (env, TInt 0)
    (B b) -> do
        modify (T ("B " ++ show b) [] :)
        return (env, TInt 0)
    Unit -> do
        modify (T "Unit" [] :)
        return (env, TInt 0)
    (Var x) -> do
        l <- elookup x env
        sat (l >= pc) "Var: l < pc"
        modify (T ("Var " ++ show x) [] :)
        return (env, l)
    (BO _ e1 e2) -> do
        (_, l1) <- check env pc e1
        (_, l2) <- check env pc e2
        (t1 : t2 : ts) <- get
        put (T "BO" [t2, t1] : ts)
        return (env, max l1 l2)
    (IfThenElse e1 e2 e3) -> do
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        (_, l2) <- check env pc' e2
        (_, l3) <- check env pc' e3
        let l = maximum [l1, l2, l3]
        (t1 : t2 : t3 : ts) <- get
        put (T "IfThenElse" [t3, t2, t1] : ts)
        return (env, l)
    (IfThen e1 e2) -> do
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        (_, l2) <- check env pc' e2
        (t1 : t2 : ts) <- get
        put (T "IfThen" [t2, t1] : ts)
        return (env, max l1 l2)
    (While e1 e2) -> do
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        (_, l2) <- check env pc' e2
        (t1 : t2 : ts) <- get
        put (T "While" [t2, t1] : ts)
        return (env, max l1 l2)
    (For x e1 e2 e3) -> do
        (_, l1) <- check env pc e1
        (_, l2) <- check env pc e2
        let pc' = maximum [l1, l2, pc]
        (_, l3) <- check (M.insert x pc' env) pc' e3 -- TODO check this
        (t1 : t2 : t3 : ts) <- get
        put (T "For" [t3, t2, t1] : ts)
        return (env, maximum [l1, l2, l3])
    (Let x l@(TInt _) e) -> do
        (_, l') <- check env pc e
        sat (l >= l') "LetTInt: l < l'"
        sat (l >= pc) "LetTInt: l < pc"
        (t : ts) <- get
        put (T "LetTInt" [t] : ts)
        return (M.insert x l env, l)
    (Let x l e) -> do
        (_, l') <- check env pc e
        sat (l == l') "LetTAbs: l /= l'"
        sat (l >= pc) "LetTAbs: l < pc"
        (t : ts) <- get
        put (T "LetTAbs" [t] : ts)
        return (M.insert x l env, l)
    (Seq e1 e2) -> do
        (env1, l1) <- check env pc e1
        (env2, l2) <- check env1 pc e2
        (t1 : t2 : ts) <- get
        put (T "Seq" [t2, t1] : ts)
        return (env2, max l1 l2)
    (Abs x l e) -> do
        (_, l') <- check (M.insert x l env) pc e
        (t : ts) <- get
        put (T "Abs" [t] : ts)
        return (env, TAbs l l')
    (App e1 e2) -> do
        (_, l1) <- check env pc e1
        (_, l2) <- check env pc e2
        case l1 of
            (TAbs l1' l2') -> do
                sat (l1' == l2) "App: l1' /= l2"
                (t1 : t2 : ts) <- get
                put (T "App" [t2, t1] : ts)
                return (env, l2')
            _ -> fail "App: not a function type"
    (Rec fs) -> do
        ls <- traverse (fmap snd . check env pc . snd) fs
        ts <- get
        let (tsFst, tsSnd) = splitAt (length fs) ts
        put (T "Rec" (reverse tsFst) : tsSnd)
        return (env, maximum ls)
    (Proj e _) -> do
        (_, l) <- check env pc e
        (t : ts) <- get
        put (T "Proj" [t] : ts)
        return (env, l)
    (Loc _) -> return (env, TInt 0)
    (Ref e) -> do
        (_, l) <- check env pc e
        (t : ts) <- get
        put (T "Ref" [t] : ts)
        return (env, l)
    (Deref e) -> do
        (_, l) <- check env pc e
        (t : ts) <- get
        put (T "Deref" [t] : ts)
        return (env, l)
    (Assign x e) -> do
        (_, l') <- check env pc e
        l <- elookup x env
        sat (l >= l') "Assign: l < l'"
        sat (l >= pc) "Assign: l < pc"
        (t : ts) <- get
        put (T "Assign" [t] : ts)
        return (env, l)
