#!/usr/bin/env runhaskell

{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification #-}

import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit ()
import Test.QuickCheck
import Test.HUnit

main = hspec mySpecs

class Case a where
  runCase :: a -> Bool
  runCases :: [a] -> Bool
  runCases cs = all id $ map runCase cs

tokenize :: String -> [String]
tokenize s = reverse $ go s Nothing []
  where
        optAppendS :: Maybe String -> [String] -> [String]
        optAppendS Nothing ts  = ts
        optAppendS (Just t) ts = t:ts

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

data TokenizeCase = TokenizeCase (String, [String])

instance Case (TokenizeCase) where
  runCase (TokenizeCase (c, rs)) = (tokenize c) == rs

tokenizeCases :: [TokenizeCase]
tokenizeCases = map TokenizeCase [
              ("()", ["(", ")"])
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

data IsBalancedCase = IsBalancedCase ([String], Bool)

instance Case (IsBalancedCase) where
  runCase (IsBalancedCase (ts, r)) = (isBalanced ts) == r

appFirst :: forall x y b. (x -> y) -> (x, b) -> (y, b)
appFirst f (a, b) = (f a, b)

appSecond :: forall x y a. (x -> y) -> (a, x) -> (a, y)
appSecond f (a, b) = (a, f b)

isBalancedCases :: [IsBalancedCase]
isBalancedCases = map (IsBalancedCase . appFirst tokenize) [
                ("()", True)
              , ("(()())", True)
              , ("(()", False)
              , (")", False)
              ]

data Tree a = Leaf a | Branch [Tree a] deriving (Show, Eq)

allJust :: forall a. [Maybe a] -> Maybe [a]
allJust xs = go xs []
  where
    go :: [Maybe a] -> [a] -> Maybe [a]
    go [] [] = Nothing  -- no Just [] in the end
    go [] as = Just as
    go (Nothing:xs) _ = Nothing
    go ((Just a):xs) as = go xs (a:as)

unrecurse :: forall a. Tree (Maybe a) -> Maybe (Tree a)
unrecurse (Leaf Nothing) = Nothing
unrecurse (Leaf (Just y)) = Just $ Leaf y
unrecurse (Branch xs) = treeified
  where
    applied :: [Maybe (Tree a)]
    applied = map unrecurse xs
   
    allJustApplied :: Maybe [Tree a]
    allJustApplied = allJust applied

    treeified :: Maybe (Tree a)
    treeified = case allJustApplied of
      Nothing -> Nothing
      Just xs -> Just $ Branch xs 

parse :: [String] -> Maybe (Tree String)
parse ts = go ts
  where
    go :: [String] -> Maybe (Tree String)
    go _ = Nothing


data ParseCase = ParseCase ([String], Maybe (Tree String))
instance Case (ParseCase) where
  runCase (ParseCase (ts, mb)) = (parse ts) == mb

parseCases :: [ParseCase]
parseCases = map (ParseCase . appFirst tokenize) [
            ("(a)", Just $ Branch [Leaf "a"])
          ]

mySpecs = describe "stuff" $ do
  it "tokenize" $ runCases tokenizeCases
  it "isBalanced" $ runCases isBalancedCases
  it "parse" $ runCases parseCases

