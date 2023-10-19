module Tree where

data Tree a = T a [Tree a]
    deriving (Show, Eq, Functor, Foldable, Traversable)

prettyPrintTree :: (Show a) => Tree a -> IO ()
prettyPrintTree tree = putStr (prettyString tree 0)

prettyString :: (Show a) => Tree a -> Int -> String
prettyString (T value children) depth =
    replicate (depth * 2) ' '
        ++ show value -- Indentation
        ++ "\n"
        ++ concatMap (\child -> prettyString child (depth + 1)) children -- Process children recursively
