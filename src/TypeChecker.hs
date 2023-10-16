module TypeChecker (check) where

import AST
import Data.Map qualified as M

-- i don't think env needs locations, at least not for now
-- type Env = M.Map (Either Variable Location) LevelT
type Env = M.Map Variable LevelT

elookup :: Variable -> Env -> Either String LevelT
elookup var env = case M.lookup var env of
    Just t -> Right t
    Nothing -> Left $ "Variable " ++ show var ++ " not found in environment"

sat :: Bool -> a -> Either a ()
sat True _ = Right ()
sat False a = Left a

-- This can probably be done with state monad

check :: Env -> LevelT -> Expr -> Either String (Env, LevelT)
check env pc expr = case expr of
    (N _) -> Right (env, TInt 0)
    (B _) -> Right (env, TInt 0)
    Unit -> Right (env, TInt 0)
    (Var x) -> do
        l <- elookup x env
        sat (l >= pc) "error"
        return (env, l)
    (BO _ e1 e2) -> do
        (_, l1) <- check env pc e1
        (_, l2) <- check env pc e2
        return (env, max l1 l2)
    (IfThenElse e1 e2 e3) -> do
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        (_, l2) <- check env pc' e2
        (_, l3) <- check env pc' e3
        let l = max l2 l3
        sat (pc <= l1) "error"
        sat (pc' <= l2) "error"
        sat (pc' <= l3) "error"
        return (env, l)
    (IfThen e1 e2) -> do
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        (_, l2) <- check env pc' e2
        sat (pc <= l1) "error"
        sat (pc' <= l2) "error"
        return (env, l2)
    (While e1 e2) -> do
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        (_, l2) <- check env pc' e2
        sat (pc <= l1) "error"
        sat (pc' <= l2) "error"
        return (env, max l1 l2)
    (For x e1 e2 e3) -> do
        (_, l1) <- check env pc e1
        (_, l2) <- check env pc e2
        let pc' = maximum [l1, l2, pc]
        (_, l3) <- check (M.insert x pc' env) pc' e3 -- TODO check this
        sat (pc <= l1) "error"
        sat (pc <= l2) "error"
        sat (pc' <= l3) "error"
        return (env, maximum [l1, l2, l3])
    (Let x l e) -> do
        (_, l') <- check env pc e
        sat (l >= l') "error"
        sat (l >= pc) "error"
        return (M.insert x l env, l)
    (Seq e1 e2) -> do
        (env1, l1) <- check env pc e1
        (env2, l2) <- check env1 pc e2
        sat (l1 >= pc) "error"
        sat (l2 >= pc) "error"
        return (env2, max l1 l2)
    (Abs x l@(TAbs l1 l2) e) -> do
        (_, l') <- check (M.insert x l1 env) pc e
        sat (l2 == l') "error"
        return (env, l)
    (Abs {}) -> Left "error"
    (App e1 e2) -> do
        (_, l1) <- check env pc e1
        (_, l2) <- check env pc e2
        case l1 of
            (TAbs l1' l2') -> do
                sat (l1' == l2) "error"
                return (env, l2')
            _ -> Left "error"
    (Rec fs) -> do
        ls <- traverse (fmap snd . check env pc . snd) fs
        return (env, maximum ls)
    (Proj e _) -> do
        (_, l) <- check env pc e
        return (env, l)
    (Loc _) -> Right (env, TInt 0)
    (Ref e) -> do
        (_, l) <- check env pc e
        return (env, l)
    (Deref e) -> do
        (_, l) <- check env pc e
        return (env, l)
    (Assign x e) -> do
        (_, l') <- check env pc e
        l <- elookup x env
        sat (l == l') "error"
        sat (l >= pc) "error"
        return (env, l)
