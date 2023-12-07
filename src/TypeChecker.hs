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

check :: Env -> LevelT -> Expr -> StateEither [String] (Env, LevelT, LevelT)
check env pc expr = case expr of
    (N _) -> do
        modify (++ ["Num: " ++ show expr])
        return (env, Low, Empty)
    (B _) -> do
        modify (++ ["Bool: " ++ show expr])
        return (env, Low, Empty)
    Unit -> do
        modify (++ ["Unit: " ++ show expr])
        return (env, Low, Empty)
    (Var x) -> do
        modify (++ ["Var: " ++ show expr])
        l <- elookup x env
        return (env, l, Empty)
    (BO _ e1 e2) -> do
        modify (++ ["BO: " ++ show expr])
        trace <- get
        (_, l1, eff1) <- check env pc e1
        put trace
        (_, l2, eff2) <- check env pc e2
        return (env, max l1 l2, min eff1 eff2)
    (IfThenElse e1 e2 e3) -> do
        modify (++ ["IfThenElse: " ++ show expr])
        trace <- get
        (_, l1, eff1) <- check env pc e1
        let pc' = max l1 pc
        put trace
        (_, l2, eff2) <- check env pc' e2
        put trace
        (_, l3, eff3) <- check env pc' e3
        return (env, maximum [l1, l2, l3], minimum [eff1, eff2, eff3])
    (IfThen e1 e2) -> do
        modify (++ ["IfThen: " ++ show expr])
        trace <- get
        (_, l1, eff1) <- check env pc e1
        let pc' = max l1 pc
        put trace
        (_, l2, eff2) <- check env pc' e2
        return (env, max l1 l2, min eff1 eff2)
    (While e1 e2) -> do
        modify (++ ["While: " ++ show expr])
        trace <- get
        (_, l1, eff1) <- check env pc e1
        let pc' = max l1 pc
        put trace
        (_, l2, eff2) <- check env pc' e2
        return (env, max l1 l2, min eff1 eff2)
    (For x e1 e2 e3) -> do
        modify (++ ["For: " ++ show expr])
        trace <- get
        (_, l1, eff1) <- check env pc e1
        put trace
        (_, l2, eff2) <- check env pc e2
        let pc' = maximum [l1, l2, pc]
        put trace
        (_, l3, eff3) <- check (M.insert x pc' env) pc' e3 -- TODO check this
        return (env, maximum [l1, l2, l3], minimum [eff1, eff2, eff3])
    (Let x l e) -> do
        modify (++ ["LetTInt: " ++ show expr])
        trace <- get
        (_, l', eff) <- check env pc e
        put trace
        sat (l >= l') "NotSat: l >= l'"
        sat (l >= pc) "NotSat: l >= pc"
        return (M.insert x l env, Low, min l eff)
    (LetInf x e) -> do
        modify (++ ["LetInf: " ++ show expr])
        trace <- get
        (_, l, eff) <- check env pc e
        put trace
        case l of
            t1 :-> (t2 :@ eff') -> do
                sat (pc == Low) "NotSat: pc == Low"
                return (M.insert x (t1 :-> (t2 :@ eff')) env, Low, eff)
            t1 -> do
                sat (t1 `elem` [Low, High]) "NotSat: t1 `elem` [Low, High]"
                sat (pc <= t1) "NotSat: pc <= t1"
                return (M.insert x t1 env, Low, max t1 eff)
    -- (Let x l@(TAbs {}) e) -> do
    --     modify (++ ["LetTAbs: " ++ show expr])
    --     trace <- get
    --     (_, l', _) <- check env pc e
    --     case l' of
    --         TEffect l'' eff -> do
    --             put trace
    --             sat (l == l'') "NotSat: l == l''"
    --             sat (l >= pc) "NotSat: l >= pc"
    --             return (M.insert x (l @ eff) env, Low, min l eff)
    --         _ -> do
    --             modify (++ ["LetTAbs: not an effect type"])
    --             fail "LetTAbs: not an effect type"
    -- (Let {}) -> error "dont make weird bindings"
    (Seq e1 e2) -> do
        modify (++ ["Seq: " ++ show expr])
        trace <- get
        (env1, _, eff1) <- check env pc e1
        put trace
        (env2, l2, eff2) <- check env1 pc e2
        return (env2, l2, min eff1 eff2)
    (Abs x l e) -> do
        modify (++ ["Abs: " ++ show expr])
        (_, l', eff') <- check (M.insert x l env) pc e
        return (env, l :-> (l' :@ eff'), Empty)
    (App e1 e2) -> do
        modify (++ ["App: " ++ show expr])
        trace <- get
        (_, l1, eff1) <- check env pc e1
        put trace
        (_, l2, eff2) <- check env pc e2
        put trace
        case l1 of
            ((l1' :-> l2') :@ eff3) -> do
                let eff = minimum [eff1, eff2, eff3]
                sat (eff >= pc) "NotSat: eff >= pc"
                sat (l1' == l2) "NotSat: l1' == l2"
                return (env, l2', eff)
            t -> fail $ "App: not a function type: " ++ show t
    (Rec {}) -> undefined
    (Proj {}) -> undefined
    (Loc _) -> do
        modify (++ ["Loc: " ++ show expr])
        return (env, Low, Empty)
    (Ref e) -> do
        modify (++ ["Ref: " ++ show expr])
        (_, l, eff) <- check env pc e
        return (env, l, eff)
    (Deref e) -> do
        modify (++ ["Deref: " ++ show expr])
        (_, l, eff) <- check env pc e
        return (env, l, eff)
    (Assign x e) -> do
        modify (++ ["Assign: " ++ show expr])
        trace <- get
        (_, l', eff') <- check env pc e
        put trace
        l <- elookup x env
        sat (l >= l') "NotSat: l >= l'"
        sat (l >= pc) "NotSat: l >= pc"
        return (env, Low, min l eff')
