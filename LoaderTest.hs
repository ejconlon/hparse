#!/usr/bin/env runhaskell

module LoaderTest where

import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit ()
import Test.QuickCheck
import Test.HUnit

import Cases
import Loader

main = hspec loaderSpecs

fstLookupCases :: [((FSTree String, FilePath), Maybe String)]
fstLookupCases = [
               ((FSTEmpty, "foo"), Nothing)
             , ((FSTFile "foo" "bar", "foo"), Just "bar")
             , ((FSTFile "foo" "bag", "/foo"), Just "bag")
             , ((FSTFile "foo" "bar", "baz"), Nothing)
             , ((FSTDir "foo" [], "foo"), Nothing)
             , ((FSTDir "foo" [(FSTFile "monkey" "flap"), (FSTFile "too" "bap")], "foo/too"), Just "bap")
             , ((FSTDir "foo" [(FSTDir "zoo" [(FSTFile "too" "bip")])], "foo/zoo/too"), Just "bip")
             ]

loaderSpecs = describe "Loader" $ do
  it "fstree" $ runCases (\(x, y) -> fstLookup x y) fstLookupCases
