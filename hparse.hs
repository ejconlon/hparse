#!/usr/bin/env runhaskell
{-- Stuff --}

import Test.Hspec.Monadic
import Test.Hspec.QuickCheck
import Test.Hspec.HUnit ()
import Test.QuickCheck
import Test.HUnit

main = hspec mySpecs

square x = x * x

mySpecs = describe "stuff" $ do
  it "stuff2" $
     square 2 == 4
  it "stuff3" $
     pending "stuff3 not done"
