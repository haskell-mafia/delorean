{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Delorean.Local.Date where

import           Delorean.Local

import           P

import           System.IO

import           Test.Delorean.Arbitrary ()
import           Test.Delorean.ParserCheck
import           Test.QuickCheck

prop_roundTripYear :: Year -> Property
prop_roundTripYear y =
  (yearFromInt . yearToInt) y === pure y

prop_roundTripMonth :: Month -> Property
prop_roundTripMonth m =
  (monthFromInt . monthToInt) m === pure m

prop_roundTripWeekOfMonth :: WeekOfMonth -> Property
prop_roundTripWeekOfMonth w =
  (weekOfMonthFromInt . weekOfMonthToInt) w === pure w

prop_roundTripDayOfMonth :: DayOfMonth -> Property
prop_roundTripDayOfMonth d =
  (dayOfMonthFromInt . dayOfMonthToInt) d === pure d

prop_roundTripDayOfWeek :: DayOfWeek -> Property
prop_roundTripDayOfWeek d =
  (dayOfWeekFromInt . dayOfWeekToInt) d === pure d

prop_roundTripNextMonth :: Date -> Bool
prop_roundTripNextMonth m =
  (prevMonth . nextMonth) m == m &&
    (nextMonth . prevMonth) m == m

prop_roundTripNextDay :: Date -> Bool
prop_roundTripNextDay d =
  (nextDay . prevDay) d == d &&
    (prevDay . nextDay) d == d

prop_toDayOfMonth :: Bool
prop_toDayOfMonth =
  let match (y, m, w, d, e) = fromMaybe False $ do
        y' <- yearFromInt y
        m' <- monthFromInt m
        e' <- dayOfMonthFromInt e
        pure $ toDayOfMonth y' m' w d == e'
  in all match [
       (2015, 2, FirstWeek, Thursday, 5)
     , (2015, 2, SecondWeek, Wednesday, 11)
     , (2015, 1, SecondWeek, Wednesday, 14)
     ]

prop_roundTripGregorianDay :: Date -> Property
prop_roundTripGregorianDay d =
  (gregorianDayToDate . dateToGregorianDay) d === d

prop_symmetricDayOfWeek :: DayOfWeek -> Property
prop_symmetricDayOfWeek =
  symmetric dayOfWeekParser renderDayOfWeek

prop_symmetricWeekOfMonth :: WeekOfMonth -> Property
prop_symmetricWeekOfMonth =
  symmetric weekOfMonthParser renderWeekOfMonth

prop_symmetricDayOfMonth :: DayOfMonth -> Property
prop_symmetricDayOfMonth =
  symmetric dayOfMonthParser renderDayOfMonth

prop_symmetricDate :: Date -> Property
prop_symmetricDate =
  symmetric dateParser renderDate

prop_parseDayOfWeek :: DayOfWeek -> Property
prop_parseDayOfWeek =
  parserAlias dayOfWeekParser renderDayOfWeek parseDayOfWeek

prop_parseWeekOfMonth :: WeekOfMonth -> Property
prop_parseWeekOfMonth =
  parserAlias weekOfMonthParser renderWeekOfMonth parseWeekOfMonth

prop_parseDayOfMonth :: DayOfMonth -> Property
prop_parseDayOfMonth =
  parserAlias dayOfMonthParser renderDayOfMonth parseDayOfMonth

prop_parseDate :: Date -> Property
prop_parseDate =
  parserAlias dateParser renderDate parseDate

return []
tests :: IO Bool
tests = $quickCheckAll
