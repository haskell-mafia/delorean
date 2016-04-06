{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Delorean.Local.Date (
    Year
  , Month (..)
  , DayOfMonth
  , DayOfWeek (..)
  , WeekOfMonth  (..)
  , Date (..)
  , yearToInt
  , yearFromInt
  , monthToInt
  , monthFromInt
  , weekOfMonthToInt
  , weekOfMonthFromInt
  , dayOfMonthToInt
  , dayOfMonthFromInt
  , dayOfWeekToInt
  , dayOfWeekFromInt
  , nextYear
  , nextMonth
  , nextMonthAt
  , nextMonthOn
  , prevMonth
  , prevMonthAt
  , prevMonthOn
  , nextWeek
  , nextDayOfWeek
  , nextDay
  , prevDay
  , plusDays
  , minusDays
  , toDayOfMonth
  , dateToGregorianDay
  , gregorianDayToDate
  , renderDayOfWeek
  , renderWeekOfMonth
  , renderDayOfMonth
  , renderDate
  , parseDayOfWeek
  , parseWeekOfMonth
  , parseDayOfMonth
  , parseDate
  , dayOfWeekParser
  , weekOfMonthParser
  , dayOfMonthParser
  , dayOfMonthParser'
  , monthParser
  , yearParser
  , dateParser
  ) where

import           Control.Exception (AssertionFailed(..), throw)

import           Data.Attoparsec.Text (Parser, parseOnly)
import           Data.Attoparsec.Text (choice, char, digit, string, decimal)
import           Data.Data (Data)
import           Data.List (zip)
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (Day(..), fromGregorian, toGregorian, addDays)
import           Data.Time.Calendar.OrdinalDate (sundayStartWeek)
import           Data.Typeable (Typeable)

import           GHC.Generics (Generic)

import           P
import           Prelude (Enum (succ, pred))

import           Text.Printf (printf)

newtype Year =
  Year {
    unYear :: Int
  } deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

instance NFData Year

data Month =
    January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

instance NFData Month

newtype DayOfMonth =
  DayOfMonth {
    unDayOfMonth :: Int
  } deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

instance NFData DayOfMonth

data DayOfWeek =
    Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

instance NFData DayOfWeek

data WeekOfMonth =
    FirstWeek
  | SecondWeek
  | ThirdWeek
  | FourthWeek
  | LastWeek
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data, Generic)

instance NFData WeekOfMonth

data Date =
  Date {
    dateYear :: !Year
  , dateMonth :: !Month
  , dateDay :: !DayOfMonth
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

instance NFData Date

yearToInt :: Year -> Int
yearToInt =
  unYear

-- | Parses a valid year for the Gregorian calendar, i.e., in or after 1600.
yearFromInt :: Int -> Maybe Year
yearFromInt n =
  valueOrEmpty (n >= 1600) $ Year n

monthToInt :: Month -> Int
monthToInt January = 1
monthToInt February = 2
monthToInt March = 3
monthToInt April = 4
monthToInt May = 5
monthToInt June = 6
monthToInt July = 7
monthToInt August = 8
monthToInt September = 9
monthToInt October = 10
monthToInt November = 11
monthToInt December = 12

monthFromInt :: Int -> Maybe Month
monthFromInt n =
  let months = zip [1 .. 12] [January .. December]
   in snd <$> find ((== n) . fst) months

weekOfMonthToInt :: WeekOfMonth -> Int
weekOfMonthToInt FirstWeek = 1
weekOfMonthToInt SecondWeek = 2
weekOfMonthToInt ThirdWeek = 3
weekOfMonthToInt FourthWeek = 4
weekOfMonthToInt LastWeek = 5

weekOfMonthFromInt :: Int -> Maybe WeekOfMonth
weekOfMonthFromInt n =
  let months = zip [1 .. 5] [FirstWeek .. LastWeek]
   in snd <$> find ((== n) . fst) months

dayOfMonthToInt :: DayOfMonth -> Int
dayOfMonthToInt =
  unDayOfMonth

dayOfMonthFromInt :: Int -> Maybe DayOfMonth
dayOfMonthFromInt n =
  valueOrEmpty (n >= 1 && n <= 31) $ DayOfMonth n

dayOfWeekToInt :: DayOfWeek -> Int
dayOfWeekToInt Sunday = 0
dayOfWeekToInt Monday = 1
dayOfWeekToInt Tuesday = 2
dayOfWeekToInt Wednesday = 3
dayOfWeekToInt Thursday = 4
dayOfWeekToInt Friday = 5
dayOfWeekToInt Saturday = 6

dayOfWeekFromInt :: Int -> Maybe DayOfWeek
dayOfWeekFromInt n =
  let days = zip [0 .. 6] [Sunday .. Saturday]
   in snd <$> find ((== n) . fst) days

nextYear :: Date -> Date
nextYear (Date y m d) =
  Date (Year . (+1) . unYear $ y) m d

nextMonth :: Date -> Date
nextMonth (Date y m d) =
  if m == December
    then Date (Year . (+1) . unYear $ y) January d
    else Date y (succ m) d

nextMonthAt :: WeekOfMonth -> DayOfWeek -> Date -> Date
nextMonthAt wom dow (Date y m _) =
  if m == December
     then let y' = (Year . (+1) . unYear) y; m' = January in Date y' m' (toDayOfMonth y' m' wom dow)
     else let y' = y; m' = succ m in Date y' m' (toDayOfMonth y' m' wom dow)

nextMonthOn :: DayOfMonth -> Date -> Date
nextMonthOn dom d =
  let (Date y m _) = nextMonth d
   in gregorianDayToDate . dateToGregorianDay $ Date y m dom

prevMonth :: Date -> Date
prevMonth (Date y m d) =
  if m == January
    then Date (pred y) December d
    else Date y (pred m) d

prevMonthAt :: WeekOfMonth -> DayOfWeek -> Date -> Date
prevMonthAt wom dow (Date y m _) =
  if m == January
     then let y' = pred y; m' = December in Date y' m' (toDayOfMonth y' m' wom dow)
     else let y' = y; m' = pred m in Date y' m' (toDayOfMonth y' m' wom dow)

prevMonthOn :: DayOfMonth -> Date -> Date
prevMonthOn dom d =
  let (Date y m _) = prevMonth d
   in gregorianDayToDate . dateToGregorianDay $ Date y m dom

nextWeek :: Date -> Date
nextWeek = plusDays 7

nextDayOfWeek :: DayOfWeek -> Date -> Date
nextDayOfWeek dow x@(Date y m d) =
  let current = snd . sundayStartWeek . dateToGregorianDay $ Date y m d
   in if (dayOfWeekFromInt current) == Just dow then x else nextDayOfWeek dow (nextDay x)

nextDay :: Date -> Date
nextDay = plusDays 1

plusDays :: Integer -> Date -> Date
plusDays n =
  unsafeFromGregorian . toGregorian . addDays n . dateToGregorianDay

minusDays :: Integer -> Date -> Date
minusDays n = plusDays (-n)

prevDay :: Date -> Date
prevDay = minusDays 1

dateToGregorianDay :: Date -> Day
dateToGregorianDay (Date y m d) =
  fromGregorian (toInteger . unYear $ y) (monthToInt m) (dayOfMonthToInt d)

gregorianDayToDate :: Day -> Date
gregorianDayToDate =
  unsafeFromGregorian . toGregorian

toDayOfMonth :: Year -> Month -> WeekOfMonth -> DayOfWeek -> DayOfMonth
toDayOfMonth y m w d =
  let dow = snd . sundayStartWeek . dateToGregorianDay $ Date y m (DayOfMonth 0)
      week1 = if dow == 0 then 0 else 7 - dow
      weeks = (weekOfMonthToInt w - 1) * 7
      days = dayOfWeekToInt d + 1
   in DayOfMonth $ week1 + weeks + days

renderDayOfWeek :: DayOfWeek -> Text
renderDayOfWeek =
  T.pack . show

renderWeekOfMonth :: WeekOfMonth -> Text
renderWeekOfMonth =
  T.pack . show

renderDayOfMonth :: DayOfMonth -> Text
renderDayOfMonth =
  T.pack . show . unDayOfMonth

renderDate :: Date -> Text
renderDate d =
  T.pack $ printf "%04d-%02d-%02d" (yearToInt . dateYear $ d) (monthToInt . dateMonth $ d) (dayOfMonthToInt . dateDay $ d)

parseDayOfWeek :: Text -> Either String DayOfWeek
parseDayOfWeek =
  parseOnly dayOfWeekParser

parseWeekOfMonth :: Text -> Either String WeekOfMonth
parseWeekOfMonth =
  parseOnly weekOfMonthParser

parseDayOfMonth :: Text -> Either String DayOfMonth
parseDayOfMonth =
  parseOnly dayOfMonthParser

parseDate :: Text -> Either String Date
parseDate =
  parseOnly dateParser

dayOfWeekParser :: Parser DayOfWeek
dayOfWeekParser =
  let p v = string (renderDayOfWeek v) *> pure v
   in choice $ p <$> [Sunday .. Saturday]

weekOfMonthParser :: Parser WeekOfMonth
weekOfMonthParser =
  let p v = string (renderWeekOfMonth v) *> pure v
   in choice $ p <$> [FirstWeek .. LastWeek]

dayOfMonthParser :: Parser DayOfMonth
dayOfMonthParser =
  decimal >>= (\m -> maybe (fail $ "Not a valid day [" <> show m <> "].") pure . dayOfMonthFromInt $ m)

dayOfMonthParser' :: Parser DayOfMonth
dayOfMonthParser' =
  replicateM 2 digit >>= (\m -> maybe (fail $ "Not a valid day [" <> m <> "].") pure . (>>= dayOfMonthFromInt) . readMaybe $ m)

monthParser :: Parser Month
monthParser =
  replicateM 2 digit >>= (\m -> maybe (fail $ "Not a valid month [" <> m <> "].") pure . (>>= monthFromInt) . readMaybe $ m)

yearParser :: Parser Year
yearParser =
  replicateM 4 digit >>= (\y -> maybe (fail $ "Not a valid year [" <> y <> "].") pure . (>>= yearFromInt) . readMaybe $ y)

dateParser :: Parser Date
dateParser = do
  y <- yearParser
  _ <- char '-'
  m <- monthParser
  _ <- char '-'
  d <- dayOfMonthParser'
  pure $ Date y m d

unsafeFromGregorian :: (Integer, Int, Int) -> Date
unsafeFromGregorian (y, m, d) =
  let failure t v = throw $ AssertionFailed ("Invariant violated: not a valid gregorian triple, invalid " <> t <> " [" <> show v <> "].")
      y' = fromMaybe (failure "year" y) $ (yearFromInt . fromInteger) y
      m' = fromMaybe (failure "month" m) $ monthFromInt m
      d' = fromMaybe (failure "day" d) $ dayOfMonthFromInt d
   in Date y' m' d'
