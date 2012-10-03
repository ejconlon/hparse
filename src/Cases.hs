{-# LANGUAGE ExistentialQuantification #-}

module Cases where

import Debug.Trace

traceEq :: forall a. (Show a, Eq a) => a -> a -> Bool
traceEq x y | x == y = True
            | otherwise = trace ("T1. " ++ (show x)) $ trace ("T2. " ++ (show y)) $ False

runCase :: forall a b. (Show b, Eq b) => (a -> b) -> a -> b -> Bool
runCase f x y = traceEq (f x) y

runCases :: forall a b. (Show b, Eq b) => (a -> b) -> [(a, b)] -> Bool
runCases f cs = all id $ map (\(x, y) -> runCase f x y) cs
