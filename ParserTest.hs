#!/usr/bin/env runhaskell

module ParserTest where

import Cases
import Loader
import HParse

import Debug.Trace

traceRet :: (Show a) => String -> a -> a
traceRet m x = trace (m ++ ": " ++ (show x)) x

parts = [ 
    "(terminal lcurly)"
  , "(terminal colon)"
  , "(terminal comma)"
  , "(terminal rcurly)"
  , "(terminal dquote)"
  , "(combinator not (x))"
  , "(combinator star (x))"
  , "(combinator maybe (x))"
  , "(combinator or (x y))"
  , "(production string (dquote (star (not dquote)) dquote))"
  , "(production pair (string colon string))"
  , "(production pairlist (or (pair) (pair comma pairlist)))"
  , "(production object (lcurly (maybe pairlist) rcurly))"
  ]


readGrammar :: [String] -> Maybe [Grammar String]
readGrammar ss = parsed
  where
    tokenized = map tokenize ss
    maybePt = map parse tokenized
    allJustPt = allJust maybePt
    parsed = (fmap (map parseGrammar) allJustPt) >>= allJust

partGrammar = readGrammar parts

main = do
  putStrLn "Grammar:"
  putStrLn $ show $ partGrammar
  putStrLn ""
  putStrLn "Declarations:"
  putStrLn $ show $ fmap (collectMap declarations) partGrammar
  putStrLn ""
  putStrLn "References:"
  putStrLn $ show $ fmap (collectMap references) partGrammar
  putStrLn ""
  putStrLn "Terminals:"
  putStrLn $ show $ fmap (collectMap terminals) partGrammar
  putStrLn ""
  putStrLn "Combinators:"
  putStrLn $ show $ fmap (collectMap combinators) partGrammar
  putStrLn ""
  putStrLn "Errors:"
  putStrLn $ show $ fmap (validateGrammar) partGrammar
  putStrLn ""
