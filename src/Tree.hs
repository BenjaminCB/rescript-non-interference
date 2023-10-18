module Tree where

data Tree a = T a [Tree a]
    deriving (Show, Eq, Functor, Foldable, Traversable)
