{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Delorean.Arbitrary where

import           Delorean.Duration
import           Delorean.Local.Date
import           Delorean.Local.Time
import           Delorean.Local.DateTime

import           P

import           Test.QuickCheck

instance Arbitrary DayOfWeek where
  arbitrary =
    elements [Sunday .. Saturday]

instance Arbitrary DayOfMonth where
  arbitrary =
    elements [1..28] >>= maybe (fail "invariant violated: invalid day of month.") pure . dayOfMonthFromInt

instance Arbitrary HourOfDay where
  arbitrary =
    elements [0..23] >>= maybe (fail "invariant violated: invalid hour of day.") pure . hourOfDayFromInt

instance Arbitrary MinuteOfHour where
  arbitrary =
    elements [0..59] >>= maybe (fail "invariant violated: invalid minute of hour.") pure . minuteOfHourFromInt

instance Arbitrary SecondOfMinute where
  arbitrary =
    elements [0..59] >>= maybe (fail "invariant violated: second of minute.") pure . secondOfMinuteFromInt

instance Arbitrary WeekOfMonth where
  arbitrary =
    elements [FirstWeek .. LastWeek]

instance Arbitrary Month where
  arbitrary =
    elements [January .. December]

instance Arbitrary Year where
  arbitrary =
    elements [1900..2100] >>= maybe (fail "invariant violated: year.") pure . yearFromInt

instance Arbitrary Date where
  arbitrary =
    Date <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Time where
  arbitrary =
    Time <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary DateTime where
  arbitrary =
    DateTime <$> arbitrary <*> arbitrary

instance Arbitrary Duration where
  arbitrary = oneof [
      Seconds <$> elements [1 .. 10000]
    , Minutes <$> elements [1 .. 1000]
    , Hours <$> elements [1 .. 100]
    ]
