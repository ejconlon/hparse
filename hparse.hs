#!/usr/bin/env runhaskell

{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification #-}

import Debug.Trace
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit ()
import Test.QuickCheck
import Test.HUnit

main = hspec mySpecs

traceEq :: forall a. (Show a, Eq a) => a -> a -> Bool
traceEq x y | x == y = True
            | otherwise = trace ("T1. " ++ (show x)) $ trace ("T2. " ++ (show y)) $ False

runCase :: forall a b. (Show b, Eq b) => (a -> b) -> a -> b -> Bool
runCase f x y = traceEq (f x) y

runCases :: forall a b. (Show b, Eq b) => (a -> b) -> [(a, b)] -> Bool
runCases f cs = all id $ map (\(x, y) -> runCase f x y) cs

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

type TokenizeCase = (String, [String])

tokenizeCases :: [TokenizeCase]
tokenizeCases = [
              ("()", ["(", ")"])
            , ("(abc)", ["(", "abc", ")"])
            , ("(a b c)", ["(", "a", "b", "c", ")"])
            , ("(a \tb)", ["(", "a", "b", ")"])
            , ("( )(\n) ()", ["(", ")", "(", ")", "(", ")"])
            ]

isBalanced :: [String] -> Bool
isBalanced [] = False
isBalanced (t:ts) = (t == "(") && (go 1 ts)
  where go c (t:ts) | (t == "(") = go (c+1) ts
                    | (t == ")") = go (c-1) ts
                    | otherwise = go c ts
        go 0 [] = True
        go _ _ = False

type IsBalancedCase = ([String], Bool)

appFirst :: forall x y b. (x -> y) -> (x, b) -> (y, b)
appFirst f (a, b) = (f a, b)

appSecond :: forall x y a. (x -> y) -> (a, x) -> (a, y)
appSecond f (a, b) = (a, f b)

isBalancedCases :: [IsBalancedCase]
isBalancedCases = map (appFirst tokenize) [
                ("()", True)
              , ("(()())", True)
              , ("(()", False)
              , (")", False)
              ]

data Tree a = Leaf a | Branch [Tree a] deriving (Show, Eq)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x) = Leaf $ f x
  fmap f (Branch xs) = Branch $ map (fmap f) xs

allJust :: forall a. [Maybe a] -> Maybe [a]
allJust xs = go xs []
  where
    go :: [Maybe a] -> [a] -> Maybe [a]
    go [] [] = Nothing  -- no Just [] in the end
    go [] as = Just as
    go (Nothing:xs) _ = Nothing
    go ((Just a):xs) as = go xs (a:as)

unMaybeTree :: forall a. Tree (Maybe a) -> Maybe (Tree a)
unMaybeTree (Leaf Nothing) = Nothing
unMaybeTree (Leaf (Just y)) = Just $ Leaf y
unMaybeTree (Branch xs) = case allJust (map unMaybeTree xs) of
      Nothing -> Nothing
      Just xs -> Just $ Branch xs 

maybeTree :: forall a b. (a -> Maybe b) -> Tree a -> Maybe (Tree b)
maybeTree = undefined

parse :: [String] -> Maybe (Tree String)
parse ts = case go ts 0 [] of
    Just ([], [mt]) -> Just mt
    _ -> Nothing
  where
    go :: [String] -> Int -> [Tree String] -> Maybe ([String], [Tree String])
    go [] 0 [] = trace "HERE1" Nothing  -- empty branch
    go [] 0 mts = Just ([], mts)
    go [] _ _ = trace "HERE2" Nothing
    go (")":ys) 0 _ = Nothing  -- close w/o open
    go (")":ys) _ [] = Nothing  -- empty branch
    go (")":ys) i mts = Just (ys, mts)
    go ("(":ys) i mts = case go ys (i+1) [] of
      Just (xs, mts2) -> go xs i ((Branch $ reverse $ mts2):mts)
      Nothing -> Nothing
    go (y:ys) i mts = go ys i ((Leaf y):mts)

type ParseCase = ([String], Maybe (Tree String))

parseCases :: [ParseCase]
parseCases = map (appFirst tokenize) [
            ("(a)", Just $ Branch [Leaf "a"])
          , ("(abc)", Just $ Branch [Leaf "abc"])
          , ("((a))", Just $ Branch [Branch [Leaf "a"]])
          , ("((a) b)", Just $ Branch [Branch [Leaf "a"], Leaf "b"])
          , ("(a (b))", Just $ Branch [Leaf "a", Branch [Leaf "b"]])
          , ("(a b c)", Just $ Branch [Leaf "a", Leaf "b", Leaf "c"])
          , ("(a (b c))", Just $ Branch [Leaf "a", Branch [Leaf "b", Leaf "c"]])
          , ("((a b) (c d))", Just $ Branch [Branch [Leaf "a", Leaf "b"], Branch [Leaf "c", Leaf "d"]])
          , ("((a (b c)) ((d e) f))", Just $ Branch [Branch [Leaf "a", Branch [Leaf "b", Leaf "c"]], Branch [Branch [Leaf "d", Leaf "e"], Leaf "f"]])
          ]

mySpecs = describe "stuff" $ do
  it "tokenize" $ runCases tokenize tokenizeCases
  it "isBalanced" $ runCases isBalanced isBalancedCases
  it "parse" $ runCases parse parseCases

