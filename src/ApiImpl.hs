{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module ApiImpl where

import Control.Applicative
import Data.Monoid
import Snap.Core
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Vector as V
import qualified Data.Foldable as F

import HParse

openHash :: V.Vector (BS.ByteString, BS.ByteString)
openHash = V.empty

closeHash :: V.Vector (BS.ByteString, BS.ByteString) -> BS.ByteString
closeHash pvec = "{ " `mappend` body `mappend` "}"
  where body = F.foldr mappend mempty (fmap (\(x, y) -> "\"" `mappend` x `mappend` "\": \"" `mappend` y `mappend` "\" ") pvec) 

insertHash :: BS.ByteString -> BS.ByteString -> V.Vector (BS.ByteString, BS.ByteString) -> V.Vector (BS.ByteString, BS.ByteString)
insertHash k v pvec = V.cons (k, v) pvec

jsonError :: BS.ByteString -> BS.ByteString
jsonError msg = closeHash $ insertHash "error" msg $ openHash

allDeclarations = fmap (collectMap declarations)
allReferences = fmap (collectMap references)
allTerminals = fmap (collectMap terminals)
allCombinators = fmap (collectMap combinators)
allErrors = fmap (validateGrammar)

topHandler :: Snap ()
topHandler = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeBS $ jsonError "Use parse?program=\\\"(program ...)\\\""

parseHandler :: Snap ()
parseHandler = do
  programString <- getParam "program"
  modifyResponse $ setHeader "Content-Type" "application/json"
  let 
   j :: Maybe String
   j = BS.toString <$> programString
   k :: Maybe [Grammar String]
   k = maybe Nothing readGrammarString j
   x :: BS.ByteString
   x = "foo" 
   in
     writeBS x

