{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Delorean.Local.TimeTest where

import           Delorean.Arbitrary ()
import           Delorean.Local.Time
import           Delorean.ParserCheck

import           P

import           System.IO

import           Test.QuickCheck

prop_roundTripHourOfDay :: HourOfDay -> Property
prop_roundTripHourOfDay h =
  (hourOfDayFromInt . hourOfDayToInt) h === pure h

prop_roundTripMinuteOfHour :: MinuteOfHour -> Property
prop_roundTripMinuteOfHour m =
  (minuteOfHourFromInt . minuteOfHourToInt) m === pure m

prop_roundTripSecondOfMinute :: SecondOfMinute -> Property
prop_roundTripSecondOfMinute s =
  (secondOfMinuteFromInt . secondOfMinuteToInt) s === pure s

prop_midnight :: Property
prop_midnight =
  pure midnight === hourOfDayFromInt 0

prop_symmetricMinuteOfHour :: MinuteOfHour -> Property
prop_symmetricMinuteOfHour =
  symmetric minuteOfHourParser renderMinuteOfHour

prop_symmetricTime :: Time -> Property
prop_symmetricTime =
  symmetric timeParser renderTime

prop_parseMinuteOfHour :: MinuteOfHour -> Property
prop_parseMinuteOfHour =
  parserAlias minuteOfHourParser renderMinuteOfHour parseMinuteOfHour

prop_parseTime :: Time -> Property
prop_parseTime =
  parserAlias timeParser renderTime parseTime

return []
tests :: IO Bool
tests = $quickCheckAll
