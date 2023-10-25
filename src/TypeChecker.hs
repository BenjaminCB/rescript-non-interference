module TypeChecker (
    check,
    prooftree,
) where

import AST
import Control.Monad.State.Lazy
import Data.List.NonEmpty hiding (length, reverse, splitAt)
import Data.Map qualified as M
import Env
import Prooftree
import StateEither

elookup :: Variable -> Env -> StateEither [String] LevelT
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

check :: Env -> LevelT -> Expr -> StateEither [String] (Env, LevelT)
check env pc expr = case expr of
    (N _) -> do
        modify (++ ["Num: " ++ show expr])
        return (env, TInt 0)
    (B _) -> do
        modify (++ ["Bool: " ++ show expr])
        return (env, TInt 0)
    Unit -> do
        modify (++ ["Unit: " ++ show expr])
        return (env, TInt 0)
    (Var x) -> do
        modify (++ ["Var: " ++ show expr])
        l <- elookup x env
        return (env, l)
    (BO _ e1 e2) -> do
        modify (++ ["BO: " ++ show expr])
        trace <- get
        (_, l1) <- check env pc e1
        put trace
        (_, l2) <- check env pc e2
        return (env, max l1 l2)
    (IfThenElse e1 e2 e3) -> do
        modify (++ ["IfThenElse: " ++ show expr])
        trace <- get
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        put trace
        (_, l2) <- check env pc' e2
        put trace
        (_, l3) <- check env pc' e3
        return (env, maximum [l1, l2, l3])
    (IfThen e1 e2) -> do
        modify (++ ["IfThen: " ++ show expr])
        trace <- get
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        put trace
        (_, l2) <- check env pc' e2
        return (env, max l1 l2)
    (While e1 e2) -> do
        modify (++ ["While: " ++ show expr])
        trace <- get
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        put trace
        (_, l2) <- check env pc' e2
        return (env, max l1 l2)
    (For x e1 e2 e3) -> do
        modify (++ ["For: " ++ show expr])
        trace <- get
        (_, l1) <- check env pc e1
        put trace
        (_, l2) <- check env pc e2
        let pc' = maximum [l1, l2, pc]
        put trace
        (_, l3) <- check (M.insert x pc' env) pc' e3 -- TODO check this
        return (env, maximum [l1, l2, l3])
    (Let x l@(TInt _) e) -> do
        modify (++ ["LetTInt: " ++ show expr])
        (_, l') <- check env pc e
        sat (l >= l') "NotSat: l >= l'"
        sat (l >= pc) "NotSat: l >= pc"
        return (M.insert x l env, l)
    (Let x l e) -> do
        modify (++ ["LetTAbs: " ++ show expr])
        (_, l') <- check env pc e
        sat (l == l') "NotSat: l == l'"
        sat (l >= pc) "NotSat: l >= pc"
        return (M.insert x l env, l)
    (Seq e1 e2) -> do
        modify (++ ["Seq: " ++ show expr])
        trace <- get
        (env1, l1) <- check env pc e1
        put trace
        (env2, l2) <- check env1 pc e2
        return (env2, max l1 l2)
    (Abs x l e) -> do
        modify (++ ["Abs: " ++ show expr])
        (_, l') <- check (M.insert x l env) pc e
        return (env, TAbs l l')
    (App e1 e2) -> do
        modify (++ ["App: " ++ show expr])
        trace <- get
        (_, l1) <- check env pc e1
        put trace
        (_, l2) <- check env pc e2
        put trace
        case l1 of
            (TAbs l1' l2') -> do
                sat (l1' == l2) "NotSat: l1' == l2"
                return (env, l2')
            _ -> fail "App: not a function type"
    (Rec fs) -> do
        modify (++ ["Rec: " ++ show expr]) -- TODO implement proper trace
        ls <- traverse (fmap snd . check env pc . snd) fs
        return (env, maximum ls)
    (Proj e _) -> do
        modify (++ ["Proj: " ++ show expr])
        (_, l) <- check env pc e
        return (env, l)
    (Loc _) -> do
        modify (++ ["Loc: " ++ show expr])
        return (env, TInt 0)
    (Ref e) -> do
        modify (++ ["Ref: " ++ show expr])
        (_, l) <- check env pc e
        return (env, l)
    (Deref e) -> do
        modify (++ ["Deref: " ++ show expr])
        (_, l) <- check env pc e
        return (env, l)
    (Assign x e) -> do
        modify (++ ["Assign: " ++ show expr])
        trace <- get
        (_, l') <- check env pc e
        put trace
        l <- elookup x env
        sat (l >= l') "NotSat: l >= l'"
        sat (l >= pc) "NotSat: l >= pc"
        return (env, l)

getFst :: State (s, a) s
getFst = do
    (s, _) <- get
    return s

getSnd :: State (a, s) s
getSnd = do
    (_, s) <- get
    return s

putSnd :: s -> State (a, s) ()
putSnd s = do
    (a, _) <- get
    put (a, s)

modifyFst :: (s -> s) -> State (s, a) ()
modifyFst f = do
    (s, a) <- get
    put (f s, a)

prooftree :: Expr -> State (Env, LevelT) (Prooftree Rule)
prooftree expr = case expr of
    (N n) -> return . Base $ Rule "N" (show n) []
    (B b) -> return . Base $ Rule "B" (show b) []
    Unit -> return . Base $ Rule "Unit" "()" []
    (Var x) -> do
        (env, _) <- get
        case M.lookup x env of
            Just t -> return . Base $ Rule "Var" (show env ++ "|-" ++ show x ++ ":" ++ show t) []
            Nothing -> return . Error $ "Variable " ++ show x ++ " not found in environment" ++ show env
    bo@(BO _ e1 e2) -> do
        t1 <- prooftree e1
        l1 <- getSnd
        t2 <- prooftree e2
        l2 <- getSnd
        putSnd (max l1 l2)
        return $ Infer (Rule "BOP" (show bo) []) (t2 :| [t1])
    ifTE@(IfThenElse e1 e2 e3) -> do
        pc <- getSnd
        t1 <- prooftree e1
        l1 <- getSnd
        putSnd $ max l1 pc
        t2 <- prooftree e2
        l2 <- getSnd
        putSnd $ max l1 pc
        t3 <- prooftree e3
        l3 <- getSnd
        putSnd $ maximum [l1, l2, l3]
        return $ Infer (Rule "IfThenElse" (show ifTE) []) (t3 :| [t2, t1])
    ifT@(IfThen e1 e2) -> do
        pc <- getSnd
        t1 <- prooftree e1
        l1 <- getSnd
        putSnd $ max l1 pc
        t2 <- prooftree e2
        l2 <- getSnd
        putSnd $ max l1 l2
        return $ Infer (Rule "IfThen" (show ifT) []) (t2 :| [t1])
    w@(While e1 e2) -> do
        pc <- getSnd
        t1 <- prooftree e1
        l1 <- getSnd
        putSnd $ max l1 pc
        t2 <- prooftree e2
        l2 <- getSnd
        putSnd $ max l1 l2
        return $ Infer (Rule "While" (show w) []) (t2 :| [t1])
    f@(For x e1 e2 e3) -> do
        pc <- getSnd
        t1 <- prooftree e1
        l1 <- getSnd
        t2 <- prooftree e2
        l2 <- getSnd
        let pc' = maximum [l1, l2, pc]
        putSnd pc'
        modifyFst (M.insert x pc')
        t3 <- prooftree e3
        l3 <- getSnd
        putSnd $ maximum [l1, l2, l3]
        return $ Infer (Rule "For" (show f) []) (t3 :| [t2, t1])
    let'@(Let x l@(TInt _) e) -> do
        pc <- getSnd
        t1 <- prooftree e
        l' <- getSnd
        let t
                | l < l' = Error "LetTInt: l < l'"
                | l < pc = Error "LetTInt: l < pc"
                | otherwise = Infer (Rule "LetTInt" (show let') []) (t1 :| [])
        putSnd l
        modifyFst (M.insert x l)
        return t
    let'@(Let x l e) -> do
        pc <- getSnd
        t1 <- prooftree e
        l' <- getSnd
        let t
                | l /= l' = Error "LetTAbs: l /= l'"
                | l < pc = Error "LetTAbs: l < pc"
                | otherwise = Infer (Rule "LetTAbs" (show let') []) (t1 :| [])
        putSnd l
        modifyFst (M.insert x l)
        return t
    s@(Seq e1 e2) -> do
        t1 <- prooftree e1
        l1 <- getSnd
        t2 <- prooftree e2
        l2 <- getSnd
        putSnd $ max l1 l2
        return $ Infer (Rule "Seq" (show s) []) (t2 :| [t1])
    a@(Abs x l e) -> do
        env <- getFst
        modifyFst (M.insert x l)
        t <- prooftree e
        l' <- getSnd
        put (env, TAbs l l')
        return $ Infer (Rule "Abs" (show a) []) (t :| [])
    a@(App e1 e2) -> do
        t1 <- prooftree e1
        l1 <- getSnd
        t2 <- prooftree e2
        l2 <- getSnd
        case l1 of
            (TAbs l1' l2') -> do
                let t
                        | l1' /= l2 = Error "App: l1' /= l2"
                        | otherwise = Infer (Rule "App" (show a) []) (t2 :| [t1])
                putSnd l2'
                return t
            _ -> return $ Error "App: not a function type"
    (Rec _) -> undefined
    -- (Rec fs) -> do -- TODO check this
    --     ts <- traverse (fmap snd . prooftree . snd) fs
    --     let (tsFst, tsSnd) = splitAt (length fs) ts
    --     putSnd $ maximum ts
    --     return $ Infer "Rec" (fromList $ reverse tsFst)
    p@(Proj e _) -> do
        t <- prooftree e
        return $ Infer (Rule "Proj" (show p) []) (t :| [])
    loc@(Loc _) -> return . Base $ Rule "Loc" (show loc) []
    r@(Ref e) -> do
        t <- prooftree e
        return $ Infer (Rule "Ref" (show r) []) (t :| [])
    d@(Deref e) -> do
        t <- prooftree e
        return $ Infer (Rule "Deref" (show d) []) (t :| [])
    a@(Assign x e) -> do
        (env, pc) <- get
        t <- prooftree e
        l' <- getSnd
        case M.lookup x env of
            Just l -> do
                let tree
                        | l < pc = Error "Assign: l < pc"
                        | l < l' = Error "Assign: l < l'"
                        | otherwise = Infer (Rule "Assign" (show a) []) (t :| [])
                putSnd l
                return tree
            Nothing -> return . Error $ "Variable " ++ show x ++ " not found in environment"
