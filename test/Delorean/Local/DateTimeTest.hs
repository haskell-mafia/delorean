{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Delorean.Local.DateTimeTest where

import           Delorean.Arbitrary ()
import           Delorean.Local.Date
import           Delorean.Local.Time
import           Delorean.Local.DateTime
import           Delorean.ParserCheck

import           P

import           System.IO

import           Test.QuickCheck

prop_dateTimeIsAlias :: Date -> Time -> Property
prop_dateTimeIsAlias d t =
  DateTime d t === dateTime (dateYear d) (dateMonth d) (dateDay d) (timeHour t) (timeMinute t) (timeSecond t)

prop_onDateId :: DateTime -> Property
prop_onDateId d =
  onDate id d === d

prop_onTimeId :: DateTime -> Property
prop_onTimeId d =
  onTime id d === d

prop_symmetricDateTime :: DateTime -> Property
prop_symmetricDateTime =
  symmetric dateTimeParser renderDateTime

prop_parseDateTime :: DateTime -> Property
prop_parseDateTime =
  parserAlias dateTimeParser renderDateTime parseDateTime

return []
tests :: IO Bool
tests = $quickCheckAll
