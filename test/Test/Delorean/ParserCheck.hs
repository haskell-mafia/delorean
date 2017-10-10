{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Delorean.ParserCheck where

import           Data.Attoparsec.Text

import           P

import           Test.QuickCheck

symmetric p r a =
  (parseOnly p . r) a === Right a

parserAlias p r alias a =
  (parseOnly p . r) a === (alias . r) a
