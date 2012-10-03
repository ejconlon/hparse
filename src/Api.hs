#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Snap.Core
import Snap.Http.Server

import ApiImpl

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop topHandler <|>
    route [("parse", parseHandler)]

