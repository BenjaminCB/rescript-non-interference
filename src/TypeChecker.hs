module TypeChecker (
    prooftree,
) where

import AST
import Control.Monad.State.Lazy
import Data.List.NonEmpty hiding (length, reverse, splitAt)
import Data.Map qualified as M
import Env
import Prooftree

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
