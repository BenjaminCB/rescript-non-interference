module TypeChecker (check) where

import AST
import Data.Map qualified as M
import Tree
import StateEither
import Control.Monad.State.Lazy

-- i don't think env needs locations, at least not for now
-- type Env = M.Map (Either Variable Location) LevelT
type Env = M.Map Variable LevelT

elookup :: Variable -> Env -> StateEither [Tree String] LevelT
elookup var env = case M.lookup var env of
    Just t -> return t
    Nothing -> fail $ "Variable " ++ show var ++ " not found in environment"

sat :: Bool -> String -> StateEither [Tree String] ()
sat True _ = return ()
sat False e = fail e

check :: Env -> LevelT -> Expr -> StateEither [Tree String] (Env, LevelT)
check env pc expr = case expr of
    (N _) -> do
        put [T "N" []]
        return (env, TInt 0)
    (B _) -> do
        put [T "B" []]
        return (env, TInt 0)
    Unit -> do
        put [T "Unit" []]
        return (env, TInt 0)
    (Var x) -> do
        l <- elookup x env
        sat (l >= pc) "Var: l < pc"
        put [T "Var" []]
        return (env, l)
    (BO _ e1 e2) -> do
        (_, l1) <- check env pc e1
        (_, l2) <- check env pc e2
        modify (\ts -> [T "BO" ts])
        return (env, max l1 l2)
    (IfThenElse e1 e2 e3) -> do
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        (_, l2) <- check env pc' e2
        (_, l3) <- check env pc' e3
        let l = max l2 l3
        sat (pc <= l1) "IfThenElse: pc > l1"
        sat (pc' <= l2) "IfThenElse: pc' > l2"
        sat (pc' <= l3) "IfThenElse: pc' > l3"
        modify (\ts -> [T "IfThenElse" ts])
        return (env, l)
    (IfThen e1 e2) -> do
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        (_, l2) <- check env pc' e2
        sat (pc <= l1) "IfThen: pc > l1"
        sat (pc' <= l2) "IfThen: pc' > l2"
        modify (\ts -> [T "IfThen" ts])
        return (env, l2)
    (While e1 e2) -> do
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        (_, l2) <- check env pc' e2
        sat (pc <= l1) "While: pc > l1"
        sat (pc' <= l2) "While: pc' > l2"
        modify (\ts -> [T "While" ts])
        return (env, max l1 l2)
    (For x e1 e2 e3) -> do
        (_, l1) <- check env pc e1
        (_, l2) <- check env pc e2
        let pc' = maximum [l1, l2, pc]
        (_, l3) <- check (M.insert x pc' env) pc' e3 -- TODO check this
        sat (pc <= l1) "For: pc > l1"
        sat (pc <= l2) "For: pc > l2"
        sat (pc' <= l3) "For: pc' > l3"
        modify (\ts -> [T "For" ts])
        return (env, maximum [l1, l2, l3])
    (Let x l@(TInt _) e) -> do
        (_, l') <- check env pc e
        sat (l >= l') "LetTInt: l < l'"
        sat (l >= pc) "LetTInt: l < pc"
        modify (\ts -> [T "LetTInt" ts])
        return (M.insert x l env, l)
    (Let x l e) -> do
        (_, l') <- check env pc e
        sat (l == l') "LetTAbs: l /= l'"
        sat (l >= pc) "LetTAbs: l < pc"
        modify (\ts -> [T "LetTAbs" ts])
        return (M.insert x l env, l)
    (Seq e1 e2) -> do
        (env1, l1) <- check env pc e1
        (env2, l2) <- check env1 pc e2
        sat (l1 >= pc) "Seq: l1 < pc"
        sat (l2 >= pc) "Seq: l2 < pc"
        modify (\ts -> [T "Seq" ts])
        return (env2, max l1 l2)
    (Abs x l e) -> do
        (_, l') <- check (M.insert x l env) pc e
        modify (\ts -> [T "Abs" ts])
        return (env, TAbs l l')
    (App e1 e2) -> do
        (_, l1) <- check env pc e1
        (_, l2) <- check env pc e2
        case l1 of
            (TAbs l1' l2') -> do
                sat (l1' == l2) "App: l1' /= l2"
                modify (\ts -> [T "App" ts])
                return (env, l2')
            _ -> fail "App: not a function type"
    (Rec fs) -> do
        ls <- traverse (fmap snd . check env pc . snd) fs
        modify (\ts -> [T "Rec" ts])
        return (env, maximum ls)
    (Proj e _) -> do
        (_, l) <- check env pc e
        modify (\ts -> [T "Proj" ts])
        return (env, l)
    (Loc _) -> return (env, TInt 0)
    (Ref e) -> do
        (_, l) <- check env pc e
        modify (\ts -> [T "Ref" ts])
        return (env, l)
    (Deref e) -> do
        (_, l) <- check env pc e
        modify (\ts -> [T "Deref" ts])
        return (env, l)
    (Assign x e) -> do
        (_, l') <- check env pc e
        l <- elookup x env
        sat (l >= l') "Assign: l < l'"
        sat (l >= pc) "Assign: l < pc"
        modify (\ts -> [T "Assign" ts])
        return (env, l)
