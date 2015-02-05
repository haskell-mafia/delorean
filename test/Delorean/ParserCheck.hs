{-# LANGUAGE NoImplicitPrelude #-}
module Delorean.ParserCheck where

import           Data.Attoparsec.Text
import           Data.String (String)
import           Data.Text (Text)

import           P

import           Test.QuickCheck

symmetric :: (Arbitrary a, Eq a, Show a) => Parser a -> (a -> Text) -> a -> Property
symmetric p r a =
  (parseOnly p . r) a === Right a

parserAlias :: (Arbitrary a, Eq a, Show a) => Parser a -> (a -> Text) -> (Text -> Either String a) -> a -> Property
parserAlias p r alias a =
  (parseOnly p . r) a === (alias . r) a
