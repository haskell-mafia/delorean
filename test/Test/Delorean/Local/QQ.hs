{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.Delorean.Local.QQ where

import           Delorean.Local

import           P

import           System.IO

import           Test.Delorean.Arbitrary ()
import           Test.QuickCheck

{-  COMPILATION TESTS -}

prop_qdate :: Bool
prop_qdate =
  all (const True) $ [
      [qdate|2014-01-01|]
    , [qdate|2015-02-11|]
--    , [qdate|2015-as-11|]
    ]


prop_qtime :: Bool
prop_qtime =
  all (const True) $ [
      [qtime|23:10:01|]
    , [qtime|00:00:00|]
    , [qtime|23:59:59|]
--  , [qtime|23:59:xx|]
    ]

prop_qdatetime :: Bool
prop_qdatetime =
  all (const True) $ [
      [qdatetime|2014-08-13 23:10:01|]
    , [qdatetime|2014-12-01 00:00:00|]
    , [qdatetime|2015-02-11 23:59:59|]
--  , [qdatetime|2011-03-28 23:59:xx|]
--  , [qdatetime|2011-03-28x23:59:00|]
    ]

prop_qhod :: Bool
prop_qhod =
  all (const True) $ [
      [qhod|0|]
    , [qhod|23|]
    , [qhod|23|]
--    , [qhod|24|]
    ]

prop_qmoh :: Bool
prop_qmoh =
  all (const True) $ [
      [qmoh|0|]
    , [qmoh|23|]
    , [qmoh|59|]
--    , [qmoh|60|]
    ]

prop_qsom :: Bool
prop_qsom =
  all (const True) $ [
      [qsom|0|]
    , [qsom|23|]
    , [qsom|59|]
--    , [qsom|60|]
    ]

prop_qdom :: Bool
prop_qdom =
  all (const True) $ [
      [qdom|1|]
    , [qdom|15|]
    , [qdom|30|]
    , [qdom|31|]
    ]

return []
tests :: IO Bool
tests = $quickCheckAll
