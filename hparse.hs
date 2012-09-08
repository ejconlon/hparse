#!/usr/bin/env runhaskell

{-# LANGUAGE MultiParamTypeClasses #-}

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
tokenizeCases = map TokenizeCase [ ("()", ["(", ")"])
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

isBalancedCases :: [IsBalancedCase]
isBalancedCases = map IsBalancedCase [ (tokenize "()", True)
              , (tokenize "(()())", True)
              , (tokenize "(()", False)
              , (tokenize ")", False)
              ]

mySpecs = describe "stuff" $ do
  it "tokenize" $ runCases tokenizeCases
  it "isBalanced" $ runCases isBalancedCases

