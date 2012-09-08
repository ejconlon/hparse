#!/usr/bin/env runhaskell
{-- Stuff --}

import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit ()
import Test.QuickCheck
import Test.HUnit

main = hspec mySpecs

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

type TokenizeCase = (String, [String])

tcaseList :: [TokenizeCase]
tcaseList = [ ("()", ["(", ")"])
            , ("(a b c)", ["(", "a", "b", "c", ")"])
            , ("(a \tb)", ["(", "a", "b", ")"])
            , ("( )(\n) ()", ["(", ")", "(", ")", "(", ")"])
            ]

tcaseRunner :: TokenizeCase -> Bool
tcaseRunner (c, rs) = tokenize c == rs

tcaseListRunner :: [TokenizeCase] -> Bool
tcaseListRunner tcl = all id $ map tcaseRunner tcl

hasBalancedParens :: [String] -> Bool
hasBalancedParens [] = False
hasBalancedParens (t:ts) = (t == "(") and (go 1 ts)
  where go c (t:ts) | t == "(" = go (c+1) ts
                    | t == ")" = go (c-1) ts
                    | otherwise = go c ts
        go 0 [] = True
        go _ _ = False






mySpecs = describe "stuff" $ do
  it "tokenizes" $ tcaseListRunner tcaseList
