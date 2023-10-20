module Prooftree where

import Data.List.NonEmpty

data Prooftree a = Base a
                 | Rule a (NonEmpty (Prooftree a))
                 | Error String
    deriving (Show, Eq, Functor, Foldable, Traversable)

(<|>) :: Prooftree a -> Prooftree a -> Prooftree a
(Error x) <|> _ = Error x
_ <|> (Error x) = Error x
(Base x) <|> _ = Base x
(Rule x children) <|> _ = Rule x children

prettyPrintTree :: Show a => Prooftree a -> IO ()
prettyPrintTree tree = putStr (prettyString tree 0)

prettyString :: Show a => Prooftree a -> Int -> String
prettyString (Base value) depth = replicate (depth * 2) ' ' ++ show value ++ "\n"
prettyString (Rule value children) depth =
    replicate (depth * 2) ' '
        ++ show value -- Indentation
        ++ "\n"
        ++ concatMap (\child -> prettyString child (depth + 1)) children -- Process children recursively
prettyString (Error message) depth = replicate (depth * 2) ' ' ++ "Error: " ++ message ++ "\n"
