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

elookup :: Variable -> Env -> StateEither [Prooftree String] LevelT
elookup var env = case M.lookup var env of
    Just t -> return t
    Nothing -> fail $ "Variable " ++ show var ++ " not found in environment"

sat :: Bool -> String -> StateEither [Prooftree String] ()
sat True _ = return ()
sat False e = fail e

check :: Env -> LevelT -> Expr -> StateEither [Prooftree String] (Env, LevelT)
check env pc expr = case expr of
    (N n) -> do
        modify (Base ("N " ++ show n) :)
        return (env, TInt 0)
    (B b) -> do
        modify (Base ("B " ++ show b) :)
        return (env, TInt 0)
    Unit -> do
        modify (Base "Unit" :)
        return (env, TInt 0)
    (Var x) -> do
        l <- elookup x env
        modify (Base ("Var " ++ show x) :)
        return (env, l)
    (BO _ e1 e2) -> do
        (_, l1) <- check env pc e1
        (_, l2) <- check env pc e2
        (t1 : t2 : ts) <- get
        put (Infer "BO" (t2 :| [t1]) : ts)
        return (env, max l1 l2)
    (IfThenElse e1 e2 e3) -> do
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        (_, l2) <- check env pc' e2
        (_, l3) <- check env pc' e3
        let l = maximum [l1, l2, l3]
        (t1 : t2 : t3 : ts) <- get
        put (Infer "IfThenElse" (t3 :| [t2, t1]) : ts)
        return (env, l)
    (IfThen e1 e2) -> do
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        (_, l2) <- check env pc' e2
        (t1 : t2 : ts) <- get
        put (Infer "IfThen" (t2 :| [t1]) : ts)
        return (env, max l1 l2)
    (While e1 e2) -> do
        (_, l1) <- check env pc e1
        let pc' = max l1 pc
        (_, l2) <- check env pc' e2
        (t1 : t2 : ts) <- get
        put (Infer "While" (t2 :| [t1]) : ts)
        return (env, max l1 l2)
    (For x e1 e2 e3) -> do
        (_, l1) <- check env pc e1
        (_, l2) <- check env pc e2
        let pc' = maximum [l1, l2, pc]
        (_, l3) <- check (M.insert x pc' env) pc' e3 -- TODO check this
        (t1 : t2 : t3 : ts) <- get
        put (Infer "For" (t3 :| [t2, t1]) : ts)
        return (env, maximum [l1, l2, l3])
    (Let x l@(TInt _) e) -> do
        (_, l') <- check env pc e
        sat (l >= l') "LetTInt: l < l'"
        sat (l >= pc) "LetTInt: l < pc"
        (t : ts) <- get
        put (Infer "LetTInt" (t :| []) : ts)
        return (M.insert x l env, l)
    (Let x l e) -> do
        (_, l') <- check env pc e
        sat (l == l') "LetTAbs: l /= l'"
        sat (l >= pc) "LetTAbs: l < pc"
        (t : ts) <- get
        put (Infer "LetTAbs" (t :| []) : ts)
        return (M.insert x l env, l)
    (Seq e1 e2) -> do
        (env1, l1) <- check env pc e1
        (env2, l2) <- check env1 pc e2
        (t1 : t2 : ts) <- get
        put (Infer "Seq" (t2 :| [t1]) : ts)
        return (env2, max l1 l2)
    (Abs x l e) -> do
        (_, l') <- check (M.insert x l env) pc e
        (t : ts) <- get
        put (Infer "Abs" (t :| []) : ts)
        return (env, TAbs l l')
    (App e1 e2) -> do
        (_, l1) <- check env pc e1
        (_, l2) <- check env pc e2
        case l1 of
            (TAbs l1' l2') -> do
                sat (l1' == l2) "App: l1' /= l2"
                (t1 : t2 : ts) <- get
                put (Infer "App" (t2 :| [t1]) : ts)
                return (env, l2')
            _ -> fail "App: not a function type"
    (Rec fs) -> do
        ls <- traverse (fmap snd . check env pc . snd) fs
        ts <- get
        let (tsFst, tsSnd) = splitAt (length fs) ts
        put (Infer "Rec" (fromList $ reverse tsFst) : tsSnd)
        return (env, maximum ls)
    (Proj e _) -> do
        (_, l) <- check env pc e
        (t : ts) <- get
        put (Infer "Proj" (t :| []) : ts)
        return (env, l)
    (Loc _) -> return (env, TInt 0)
    (Ref e) -> do
        (_, l) <- check env pc e
        (t : ts) <- get
        put (Infer "Ref" (t :| []) : ts)
        return (env, l)
    (Deref e) -> do
        (_, l) <- check env pc e
        (t : ts) <- get
        put (Infer "Deref" (t :| []) : ts)
        return (env, l)
    (Assign x e) -> do
        (_, l') <- check env pc e
        l <- elookup x env
        sat (l >= l') "Assign: l < l'"
        sat (l >= pc) "Assign: l < pc"
        (t : ts) <- get
        put (Infer "Assign" (t :| []) : ts)
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
                        | l < l' = Error "Assign: l < l'"
                        | l < pc = Error "Assign: l < pc"
                        | otherwise = Infer (Rule "Assign" (show a) []) (t :| [])
                putSnd l
                return tree
            Nothing -> return . Error $ "Variable " ++ show x ++ " not found in environment"
