module StateEither where

import Control.Monad.State.Lazy

newtype StateEither s a = StateEither { runStateEither :: s -> Either (String, s) (a, s) }

instance Functor (StateEither s) where
    fmap f (StateEither g) = StateEither $ \s -> case g s of
        Left (e, s') -> Left (e, s')
        Right (a, s') -> Right (f a, s')

instance Applicative (StateEither s) where
    pure a = StateEither $ \s -> Right (a, s)
    StateEither f <*> StateEither g = StateEither $ \s -> case f s of
        Left (e, s') -> Left (e, s')
        Right (f', s') -> case g s' of
            Left (e, s'') -> Left (e, s'')
            Right (g', s'') -> Right (f' g', s'')

instance Monad (StateEither s) where
    return = pure
    StateEither f >>= g = StateEither $ \s -> case f s of
        Left (e, s') -> Left (e, s')
        Right (a, s') -> runStateEither (g a) s'

instance MonadState s (StateEither s) where
    get = StateEither $ \s -> Right (s, s)
    put s = StateEither $ \_ -> Right ((), s)

instance MonadFail (StateEither s) where
    fail e = StateEither $ \s -> Left (e, s)

evalStateEither :: StateEither s a -> s -> Either String a
evalStateEither (StateEither f) s = case f s of
    Left (e, _) -> Left e
    Right (a, _) -> Right a
