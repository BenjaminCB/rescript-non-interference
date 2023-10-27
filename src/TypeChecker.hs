module TypeChecker (
    check,
) where

import AST
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as M
import Env
import StateEither

elookup :: (Ord k, Show k) => k -> M.Map k a -> StateEither ([String], LevelT) a
elookup var env = case M.lookup var env of
    Just t -> return t
    Nothing -> do
        let msg = "Variable " ++ show var ++ " not found in environment"
        modifyFst (++ [msg])
        fail msg

nelookup :: (Eq a, Show a) => a -> NE.NonEmpty (a, b) -> StateEither ([String], LevelT) b
nelookup a xs = case lookup a $ NE.toList xs of
    Just b -> return b
    Nothing -> do
        let msg = "Label " ++ show a ++ " not found in record"
        modifyFst (++ [msg])
        fail msg

sat :: Bool -> String -> StateEither ([String], LevelT) ()
sat True _ = return ()
sat False e = do
    modifyFst (++ [e])
    fail e

modifyFst :: (MonadState (t, b) m) => (t -> t) -> m ()
modifyFst = modify . first

getFst :: (MonadState (t, b) m) => m t
getFst = gets fst

putFst :: (MonadState (t, b) m) => t -> m ()
putFst t = do
    (_, b) <- get
    put (t, b)

modifySnd :: (MonadState (a, t) m) => (t -> t) -> m ()
modifySnd = modify . second

getSnd :: (MonadState (a, t) m) => m t
getSnd = gets snd

putSnd :: (MonadState (a, t) m) => t -> m ()
putSnd t = do
    (a, _) <- get
    put (a, t)

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
        trace <- getFst
        (_, l1) <- check env pc e1
        putFst trace
        (_, l2) <- check env pc e2
        return (env, max l1 l2)
    (IfThenElse e1 e2 e3) -> do
        modifyFst (++ ["IfThenElse: " ++ show expr])
        trace <- getFst
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        putFst trace
        (_, l2) <- check env pc' e2
        putFst trace
        (_, l3) <- check env pc' e3
        return (env, maximum [l1, l2, l3])
    (IfThen e1 e2) -> do
        modifyFst (++ ["IfThen: " ++ show expr])
        trace <- getFst
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        putFst trace
        (_, l2) <- check env pc' e2
        return (env, max l1 l2)
    (While e1 e2) -> do
        modifyFst (++ ["While: " ++ show expr])
        trace <- getFst
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        putFst trace
        (_, l2) <- check env pc' e2
        return (env, max l1 l2)
    (For x e1 e2 e3) -> do
        modifyFst (++ ["For: " ++ show expr])
        trace <- getFst
        (_, l1) <- check env pc e1
        putFst trace
        (_, l2) <- check env pc e2
        let pc' = maximum [l1, l2, pc]
        putFst trace
        (_, l3) <- check (M.insert x pc' env) pc' e3 -- TODO check this
        return (env, maximum [l1, l2, l3])
    (Let x l@(TInt _) e) -> do
        modifyFst (++ ["LetTInt: " ++ show expr])
        (_, l') <- check env pc e
        sat (l >= l') "NotSat: l >= l'"
        sat (l >= pc) "NotSat: l >= pc"
        return (M.insert x l env, l)
    (Let x l@(TAbs {}) e) -> do
        modifyFst (++ ["LetTAbs: " ++ show expr])
        trace <- getFst
        (_, l') <- check env pc e
        putFst trace
        sat (l == l') "NotSat: l == l'"
        sat (l >= pc) "NotSat: l >= pc"
        return (M.insert x l env, l)
    (Let x l@(TRec {}) e) -> do
        modifyFst (++ ["LetTRec: " ++ show expr])
        trace <- getFst
        (_, l') <- check env pc e
        putFst trace
        sat (l >= l') "NotSat: l >= l'"
        sat (l >= pc) "NotSat: l >= pc"
        return (M.insert x l env, l)
    (Seq e1 e2) -> do
        modifyFst (++ ["Seq: " ++ show expr])
        trace <- getFst
        (env1, l1) <- check env pc e1
        putFst trace
        (env2, l2) <- check env1 pc e2
        return (env2, max l1 l2)
    (Abs x l e) -> do
        modifyFst (++ ["Abs: " ++ show expr])
        putSnd $ TInt 1000
        (_, l') <- check (M.insert x l env) pc e
        pc' <- getSnd
        return (env, TAbs pc' l l')
    (App e1 e2) -> do
        modifyFst (++ ["App: " ++ show expr])
        trace <- getFst
        (_, l1) <- check env pc e1
        putFst trace
        (_, l2) <- check env pc e2
        putFst trace
        case l1 of
            (TAbs pc' l1' l2') -> do
                sat (pc' >= pc) "NotSat: pc' >= pc"
                sat (l1' == l2) "NotSat: l1' == l2"
                return (env, l2')
            _ -> fail "App: not a function type"
    (Rec fs) -> do
        modifyFst (++ ["Rec: " ++ show expr]) -- TODO implement proper trace
        trace <- getFst
        ls <-
            traverse
                ( \(label, e) -> do
                    putFst trace
                    (_, l') <- check env pc e
                    return (label, l')
                )
                fs
        return (env, TRec ls)
    (Proj e label) -> do
        modifyFst (++ ["Proj: " ++ show expr])
        trace <- getFst
        (_, l) <- check env pc e
        putFst trace
        case l of
            (TRec ls) -> do
                l' <- nelookup label ls
                return (env, l')
            _ -> do
                modifyFst (++ ["Proj: not a record type"])
                fail "Proj: not a record type"
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
        trace <- getFst
        (_, l') <- check env pc e
        putFst trace
        l <- elookup x env
        modifySnd (min l)
        sat (l >= l') "NotSat: l >= l'"
        sat (l >= pc) "NotSat: l >= pc"
        return (env, l)
