#!/usr/bin/env runhaskell
{-# LANGUAGE MultiParamTypeClasses, ExistentialQuantification, FlexibleInstances #-}

module Loader where

import Data.Functor.Identity

import qualified Data.Map as M

data FSTree a = FSTEmpty | FSTDir FilePath [FSTree a] | FSTFile FilePath a

unlead :: Char -> String -> String
unlead x s@(c:cs) | x == c = unlead x cs
                  | otherwise = s
unlead _ _ = ""

wordsOn :: Char -> String -> [String]
wordsOn c s =  case dropWhile (== c) s of
                 "" -> []
                 s' -> w : wordsOn c s''
                   where (w, s'') = break (== c) s'

unwordsOn :: Char -> [String] -> String
unwordsOn c [] = ""
unwordsOn c [x] = x
unwordsOn c (x:xs) = x ++ [c] ++ (unwordsOn c xs)

justMap :: (a -> Maybe b) -> [a] -> Maybe b
justMap f [] = Nothing
justMap f (x:xs) = case (f x) of
                     Just y -> Just y
                     Nothing -> justMap f xs 

fstLookup :: FSTree a -> FilePath -> Maybe a
fstLookup FSTEmpty _ = Nothing
fstLookup (FSTFile p x) q | p == (unlead '/' q) = Just x
                          | otherwise = Nothing
fstLookup (FSTDir p ts) q = go (wordsOn '/' (unlead '/' q)) p ts
  where
    go :: [String] -> String -> [FSTree a] -> Maybe a
    go [] _ _ = Nothing
    go _ "" _ = Nothing
    go _ _ [] = Nothing
    go (q:qs) p ts | p == q = justMap (\t -> fstLookup t (unwordsOn '/' qs)) ts

instance Functor FSTree where
  fmap f FSTEmpty = FSTEmpty
  fmap f (FSTFile p s) = FSTFile p (f s) 
  fmap f (FSTDir p ts) = FSTDir p (map (fmap f) ts)

class (Functor f) => FSReader c f a where
  fsmReadFile :: c -> FilePath -> f (Maybe a)

instance FSReader (FSTree a) Identity a where
  fsmReadFile fst p = Identity $ fstLookup fst p

instance FSReader () IO String where
  -- TODO actually handle file not found here
  fsmReadFile _ path = (readFile path >>= (\x -> return $ Just x))


