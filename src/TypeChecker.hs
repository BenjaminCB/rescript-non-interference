module TypeChecker (
    check,
) where

import AST
import Control.Monad.State.Lazy
import Data.Map qualified as M
import Data.Bifunctor
import Env
import StateEither

elookup :: Variable -> Env -> StateEither ([String], LevelT) LevelT
elookup var env = case M.lookup var env of
    Just t -> return t
    Nothing -> do
        let msg = "Variable " ++ show var ++ " not found in environment"
        modifyFst (++ [msg])
        fail msg

sat :: Bool -> String -> StateEither ([String], LevelT) ()
sat True _ = return ()
sat False e = do
    modifyFst (++ [e])
    fail e

modifyFst :: MonadState (t, b) m => (t -> t) -> m ()
modifyFst = modify . first

modifySnd :: MonadState (a, t) m => (t -> t) -> m ()
modifySnd = modify . second


check :: Env -> LevelT -> Expr -> StateEither ([String], LevelT) (Env, LevelT)
check env pc expr = case expr of
    (N _) -> do
        modifyFst (++ ["Num: " ++ show expr])
        return (env, TInt 0)
    (B _) -> do
        modifyFst (++ ["Bool: " ++ show expr])
        return (env, TInt 0)
    Unit -> do
        modifyFst (++ ["Unit: " ++ show expr])
        return (env, TInt 0)
    (Var x) -> do
        modifyFst (++ ["Var: " ++ show expr])
        l <- elookup x env
        return (env, l)
    (BO _ e1 e2) -> do
        modifyFst (++ ["BO: " ++ show expr])
        trace <- get
        (_, l1) <- check env pc e1
        put trace
        (_, l2) <- check env pc e2
        return (env, max l1 l2)
    (IfThenElse e1 e2 e3) -> do
        modifyFst (++ ["IfThenElse: " ++ show expr])
        trace <- get
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        put trace
        (_, l2) <- check env pc' e2
        put trace
        (_, l3) <- check env pc' e3
        return (env, maximum [l1, l2, l3])
    (IfThen e1 e2) -> do
        modifyFst (++ ["IfThen: " ++ show expr])
        trace <- get
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        put trace
        (_, l2) <- check env pc' e2
        return (env, max l1 l2)
    (While e1 e2) -> do
        modifyFst (++ ["While: " ++ show expr])
        trace <- get
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        put trace
        (_, l2) <- check env pc' e2
        return (env, max l1 l2)
    (For x e1 e2 e3) -> do
        modifyFst (++ ["For: " ++ show expr])
        trace <- get
        (_, l1) <- check env pc e1
        put trace
        (_, l2) <- check env pc e2
        let pc' = maximum [l1, l2, pc]
        put trace
        (_, l3) <- check (M.insert x pc' env) pc' e3 -- TODO check this
        return (env, maximum [l1, l2, l3])
    (Let x l@(TInt _) e) -> do
        modifyFst (++ ["LetTInt: " ++ show expr])
        (_, l') <- check env pc e
        sat (l >= l') "NotSat: l >= l'"
        sat (l >= pc) "NotSat: l >= pc"
        return (M.insert x l env, l)
    (Let x l e) -> do
        modifyFst (++ ["LetTAbs: " ++ show expr])
        (_, l') <- check env pc e
        sat (l == l') "NotSat: l == l'"
        sat (l >= pc) "NotSat: l >= pc"
        return (M.insert x l env, l)
    (Seq e1 e2) -> do
        modifyFst (++ ["Seq: " ++ show expr])
        trace <- get
        (env1, l1) <- check env pc e1
        put trace
        (env2, l2) <- check env1 pc e2
        return (env2, max l1 l2)
    (Abs x l e) -> do
        modifyFst (++ ["Abs: " ++ show expr])
        (_, l') <- check (M.insert x l env) pc e
        return (env, TAbs (TInt 0) l l')
    (App e1 e2) -> do
        modifyFst (++ ["App: " ++ show expr])
        trace <- get
        (_, l1) <- check env pc e1
        put trace
        (_, l2) <- check env pc e2
        put trace
        case l1 of
            (TAbs _ l1' l2') -> do
                sat (l1' == l2) "NotSat: l1' == l2"
                return (env, l2')
            _ -> fail "App: not a function type"
    (Rec fs) -> do
        modifyFst (++ ["Rec: " ++ show expr]) -- TODO implement proper trace
        ls <- traverse (fmap snd . check env pc . snd) fs
        return (env, maximum ls)
    (Proj e _) -> do
        modifyFst (++ ["Proj: " ++ show expr])
        (_, l) <- check env pc e
        return (env, l)
    (Loc _) -> do
        modifyFst (++ ["Loc: " ++ show expr])
        return (env, TInt 0)
    (Ref e) -> do
        modifyFst (++ ["Ref: " ++ show expr])
        (_, l) <- check env pc e
        return (env, l)
    (Deref e) -> do
        modifyFst (++ ["Deref: " ++ show expr])
        (_, l) <- check env pc e
        return (env, l)
    (Assign x e) -> do
        modifyFst (++ ["Assign: " ++ show expr])
        trace <- get
        (_, l') <- check env pc e
        put trace
        l <- elookup x env
        sat (l >= l') "NotSat: l >= l'"
        sat (l >= pc) "NotSat: l >= pc"
        return (env, l)
