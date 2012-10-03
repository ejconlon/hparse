#!/usr/bin/env runhaskell

module LoaderTest where

import Cases
import Loader
import HParse

import Debug.Trace

traceRet :: (Show a) => String -> a -> a
traceRet m x = trace (m ++ ": " ++ (show x)) x

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

-- allJust :: [Maybe a] => Maybe [a]
-- allJust mxs = go mxs []
--   where
--     go [] [] = Nothing
--     go [] ys = Just $ reverse ys
--     go ((Just y):xs) ys = go xs (y:ys)
--     go _ _ = Nothing

maybeApp :: (a -> b) -> b -> Maybe a -> b
maybeApp f z Nothing = z
maybeApp f z (Just x) = f x

readJsonSimple :: String -> Maybe [Grammar String]
readJsonSimple s = parsed2
  where
    tokenized = map tokenize (lines s) 
    maybePt = map parse tokenized
    allJustPt = allJust maybePt
    parsed = (fmap (map parseGrammar) allJustPt) >>= allJust
    --parsed2 = traceRet "parsed" parsed
    parsed2 = parsed

checkGrammar :: Maybe [Grammar String] -> [String]
checkGrammar Nothing = ["BAD GRAMMAR - could not parse"]
checkGrammar (Just gs) = validateGrammar gs

runAssertion :: [String] -> IO ()
runAssertion [] = print "ok"
runAssertion xs = print (foldl (\x y -> x ++ ", " ++ y) "FAIL: " xs)

runJsonSimple :: IO ()
runJsonSimple = readFile "../data/jsonsimple.gram" >>= (\x -> return $ checkGrammar $ readJsonSimple x) >>= runAssertion

main = runJsonSimple
