{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification #-}

module HParse where

import qualified Debug.Trace
import qualified Data.Foldable
import qualified Data.List
import qualified Data.Set

-- Miscellaneous helpers

appFirst :: forall x y b. (x -> y) -> (x, b) -> (y, b)
appFirst f (a, b) = (f a, b)

appSecond :: forall x y a. (x -> y) -> (a, x) -> (a, y)
appSecond f (a, b) = (a, f b)

appBoth :: forall x y. (x -> y) -> (x, x) -> (y, y)
appBoth f (a, b) = (f a, f b)

dup :: forall a. a -> (a, a)
dup x = (x, x)

allJust :: forall a. [Maybe a] -> Maybe [a]
allJust xs = go xs []
  where
    go :: [Maybe a] -> [a] -> Maybe [a]
    go [] [] = Nothing  -- no Just [] in the end
    go [] as = Just $ reverse as
    go (Nothing:xs) _ = Nothing
    go ((Just a):xs) as = go xs (a:as)


-- tokenize

tokenize :: String -> [String]
tokenize s = reverse $ go s Nothing []
  where
        optAppendS :: Maybe String -> [String] -> [String]
        optAppendS Nothing ts  = ts
        optAppendS (Just t) ts = (reverse t):ts

        optAppendC :: Char -> Maybe String -> String
        optAppendC c Nothing = [c]
        optAppendC c (Just t) = c:t

        whitespace :: String
        whitespace = " \t\r\n"

        brackets :: String
        brackets = "()"

        go :: String -> Maybe String -> [String] -> [String] 
        -- args:
        --   remaining characters
        --   current token
        --   current emitted tokens (NB will be reversed!)
        -- returns:
        --   emitted tokens (NB will be reversed!)
        go (c:s) mt ts | elem c whitespace = go s Nothing (optAppendS mt ts)
                       | elem c brackets = go s Nothing ([c]:(optAppendS mt ts))
                       | otherwise =  go s (Just $ optAppendC c mt) ts
        go [] mt ts = optAppendS mt ts


-- isBalanced

isBalanced :: [String] -> Bool
isBalanced [] = False
isBalanced (t:ts) = (t == "(") && (go 1 ts)
  where go c (t:ts) | (t == "(") = go (c+1) ts
                    | (t == ")") = go (c-1) ts
                    | otherwise = go c ts
        go 0 [] = True
        go _ _ = False


-- Tree and instances

data Tree a = Leaf a | Branch [Tree a] deriving (Show, Eq)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Branch xs) = Branch $ map (fmap f) xs

instance Data.Foldable.Foldable Tree where
  foldr f z (Leaf x) = f x z
  foldr f z (Branch xs) = go f z (reverse xs)
    where
      go :: (a -> b -> b) -> b -> [Tree a] -> b
      go f z [] = z
      go f z (mt:mts) = go f (Data.Foldable.foldr f z mt) mts
  foldl f z (Leaf x) = f z x
  foldl f z (Branch xs) = go f z xs
    where
      go :: (b -> a -> b) -> b -> [Tree a] -> b
      go f z [] = z
      go f z (mt:mts) = go f (Data.Foldable.foldl f z mt) mts

-- useful tree functions

-- unMaybeTree :: forall a. Tree (Maybe a) -> Maybe (Tree a)
-- unMaybeTree (Leaf Nothing) = Nothing
-- unMaybeTree (Leaf (Just y)) = Just $ Leaf y
-- unMaybeTree (Branch xs) = case allJust (map unMaybeTree xs) of
--       Nothing -> Nothing
--       Just xs -> Just $ Branch xs 
-- 
-- maybeTree :: forall a b. (a -> Maybe b) -> Tree a -> Maybe (Tree b)
-- maybeTree = undefined


-- parse

parse :: [String] -> Maybe (Tree String)
parse ts = case go ts 0 [] of
    Just ([], [mt]) -> Just mt
    _ -> Nothing
  where
    go :: [String] -> Int -> [Tree String] -> Maybe ([String], [Tree String])
    go [] 0 [] = Nothing  -- empty branch
    go [] 0 mts = Just ([], mts)
    go [] _ _ = Nothing
    go (")":ys) 0 _ = Nothing  -- close w/o open
    go (")":ys) _ [] = Nothing  -- empty branch
    go (")":ys) i mts = Just (ys, mts)
    go ("(":ys) i mts = case go ys (i+1) [] of
      Just (xs, mts2) -> go xs i ((Branch $ reverse $ mts2):mts)
      Nothing -> Nothing
    go (y:ys) i mts = go ys i ((Leaf y):mts)


-- unparse

unparse :: Tree String -> [String]
unparse (Leaf a) = [a]
unparse (Branch xs) = ["("] ++ (Data.List.foldl' (++) [] (map unparse xs)) ++ [")"]


-- untokenize

untokenize :: [String] -> String
untokenize ts = go ts False ""
  where
    go :: [String] -> Bool -> String -> String
    go [] _ s = s
    go ("(":ts) True s = go ts False (s ++ " (")
    go ("(":ts) False s = go ts False (s ++ "(")
    go (")":ts) _ s = go ts True (s ++ ")")
    go (t:ts) True s = go ts True (s ++ " " ++ t)
    go (t:ts) False s = go ts True (s ++ t)


-- Grammar!

data Grammar a = Combinator a [a] | Terminal a | Production a (Tree a) deriving (Show, Eq)

type GrammarFunction a = Tree a -> Maybe (Grammar a)

makeProduction :: forall a. (Eq a) => a -> GrammarFunction a 
makeProduction ident = go
  where
    go (Branch [Leaf mident, Leaf name, produced]) | mident == ident = Just $ Production name produced
    go _ = Nothing

makeTerminal :: forall a. (Eq a) => a -> GrammarFunction a 
makeTerminal ident = go
  where
    go (Branch [Leaf mident, Leaf name]) | mident == ident = Just $ Terminal name
    go _ = Nothing

makeCombinator :: forall a. (Eq a) => a -> GrammarFunction a
makeCombinator ident = go
  where
    go (Branch [Leaf mident, Leaf name, Branch xs]) | mident == ident = check name (go2 xs)
    go _ = Nothing
    go2 [] = (True, [])
    go2 ((Leaf y):xs) = case go2 xs of
      (True, ys) -> (True, y:ys)
      (False, _) -> (False, [])
    go2 _ = (False, [])
    check name (True, ys) = Just $ Combinator name ys
    check _ _ = Nothing

-- maybeToList :: forall a. Maybe a -> [a]
-- maybeToList (Just x) = [x]
-- maybeToList _ = []
-- 
-- maybeOr :: forall a. Maybe a -> Maybe a -> Maybe a
-- maybeOr Nothing y = y
-- maybeOr x _ = x

maybeOrList :: [Maybe a] -> Maybe a
maybeOrList [] = Nothing
maybeOrList (Nothing:xs) = maybeOrList xs
maybeOrList (x:_) = x

mergeGrammarFunctions :: [GrammarFunction String] -> GrammarFunction String
mergeGrammarFunctions fs = \x -> maybeOrList (map (\f -> f x) fs) 

parseGrammarFunctions :: [GrammarFunction String]
parseGrammarFunctions = [makeProduction "production", makeTerminal "terminal", makeCombinator "combinator"]

parseGrammar :: GrammarFunction String
parseGrammar = mergeGrammarFunctions parseGrammarFunctions

unionize :: (Ord a, Data.Foldable.Foldable f) => f (Data.Set.Set a) -> Data.Set.Set a
unionize xs = Data.Foldable.foldl Data.Set.union Data.Set.empty xs

declarations :: forall a. Ord a => Grammar a -> Data.Set.Set a
declarations (Combinator name _) = Data.Set.singleton name
declarations (Terminal name) = Data.Set.singleton name
declarations (Production name _) = Data.Set.singleton name

references :: forall a. Ord a => Grammar a -> Data.Set.Set a
references (Production name produced) = unionize (fmap Data.Set.singleton produced)
references _ = Data.Set.empty

terminals :: forall a. Ord a => Grammar a -> Data.Set.Set a
terminals (Terminal name) = Data.Set.singleton name
terminals _ = Data.Set.empty

combinators :: forall a. Ord a => Grammar a -> Data.Set.Set (a, Int)
combinators (Combinator name xs) = Data.Set.singleton (name, length xs)
combinators _ = Data.Set.empty

gcollect :: forall a b. Ord b => (Grammar a -> Data.Set.Set b) -> [Grammar a] -> Data.Set.Set b
gcollect f gs = unionize (map f gs)

missingRefs :: forall a. Ord a => [Grammar a] -> Data.Set.Set a
missingRefs gs = Data.Set.difference refs decls
  where
    decls = gcollect declarations gs
    refs = gcollect references gs

type GrammarValidator a = [Grammar a] -> [String] 

validateMissingRefs :: (Ord a, Show a) => GrammarValidator a
validateMissingRefs gs = merrs
  where
    mrefs = missingRefs gs
    merrs = map (\x -> "Missing ref: " ++ (show x)) (Data.Set.toList mrefs)

validateGrammar :: (Ord a, Show a) => GrammarValidator a
validateGrammar = validateMissingRefs -- for now





