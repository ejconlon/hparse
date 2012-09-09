#!/usr/bin/env runhaskell
{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification #-}

import Debug.Trace
import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit ()
import Test.QuickCheck
import Test.HUnit

import HParse

main = hspec hparseSpecs

traceEq :: forall a. (Show a, Eq a) => a -> a -> Bool
traceEq x y | x == y = True
            | otherwise = trace ("T1. " ++ (show x)) $ trace ("T2. " ++ (show y)) $ False

runCase :: forall a b. (Show b, Eq b) => (a -> b) -> a -> b -> Bool
runCase f x y = traceEq (f x) y

runCases :: forall a b. (Show b, Eq b) => (a -> b) -> [(a, b)] -> Bool
runCases f cs = all id $ map (\(x, y) -> runCase f x y) cs


tokenizeCases :: [(String, [String])]
tokenizeCases = [
              ("()", ["(", ")"])
            , ("(abc)", ["(", "abc", ")"])
            , ("(a b c)", ["(", "a", "b", "c", ")"])
            , ("(a \tb)", ["(", "a", "b", ")"])
            , ("( )(\n) ()", ["(", ")", "(", ")", "(", ")"])
            ]


isBalancedCases :: [([String], Bool)]
isBalancedCases = map (appFirst tokenize) [
                ("()", True)
              , ("(()())", True)
              , ("(()", False)
              , (")", False)
              ]


parseCases :: [([String], Maybe (Tree String))]
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

-- ugh
unJust :: forall a. (Show a) => Maybe a -> a
unJust (Just x) = x
unJust m = error "Some example is wrong"

expressions :: [String]
expressions = ["(a)", "(abc)", "(a b c)", "((a))", "((a) b)",
               "(a (b))", "(a (b c))", "((a b) (c d))"]

unparseCases :: [(Tree String, [String])]
unparseCases = map ((appFirst $ unJust . parse) . dup . tokenize) expressions

untokenizeCases :: [([String], String)]
untokenizeCases = map ((appFirst $ unparse . unJust . parse . tokenize) . dup) expressions

treeFmapCases :: [(Tree String, Tree String)]
treeFmapCases = map (appBoth $ unJust . parse . tokenize) [
              ("(a)", "(aa)")
            , ("(abc)", "(abcabc)")
            , ("((a b) (c d))", "((aa bb) (cc dd))")
            ]

hparseSpecs = describe "HParse" $ do
  it "tokenize" $ runCases tokenize tokenizeCases
  it "isBalanced" $ runCases isBalanced isBalancedCases
  it "parse" $ runCases parse parseCases
  it "unparse" $ runCases unparse unparseCases
  it "untokenize" $ runCases untokenize untokenizeCases
  it "tree fmap" $ runCases (fmap (\x -> x ++ x)) treeFmapCases

