{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Delorean.Duration where

import           Delorean.Duration
import           Delorean.ParserCheck

import           P

import           System.IO

import           Test.Delorean.Arbitrary ()
import           Test.QuickCheck

prop_consistency :: Int -> Property
prop_consistency n = (n >= 0 && n < 100) ==>
  let h = Hours $ n
      m = Minutes . (*60) $ n
      s = Seconds . (*60) . (*60) $ n
   in h == m && h == s

prop_ord :: Duration -> Duration -> Property
prop_ord a b =
  compare a b === on compare durationToSeconds a b

prop_symmetricDuration :: Duration -> Property
prop_symmetricDuration =
  symmetric durationParser renderDuration

return []
tests :: IO Bool
tests = $quickCheckAll
