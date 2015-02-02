{-# LANGUAGE TemplateHaskell #-}
module DeloreanTest where

import           Delorean

import           Test.QuickCheck

import           System.IO

prop_associative :: Int -> Int -> Int -> Property
prop_associative x y z =
  add (add x y) z === add x (add y z)

return []
tests :: IO Bool
tests = $quickCheckAll
