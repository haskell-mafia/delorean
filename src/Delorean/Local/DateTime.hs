{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Delorean.Local.DateTime (
    DateTime (..)
  , dateTime
  , onDate
  , onTime
  , nextHour
  , fromZoned
  , local
  , loadTZFromDB
  , renderDateTime
  , parseDateTime
  , parseDateTime'
  , dateTimeParser
  ) where

import           Data.Attoparsec.Text
import           Data.Time hiding (midnight, parseTime, timeOfDayToTime)
import           Data.Time.Zones (TZ, utcToLocalTimeTZ, loadTZFromDB)
import           Data.Typeable (Typeable)
import           Data.Data (Data)
import           Data.String (String)
import           Data.Text (Text)

import           Delorean.Local.Date
import           Delorean.Local.Time

import           P

import           Prelude (Enum (..))

import           System.IO

data DateTime =
  DateTime {
    getDate :: !Date
  , getTime :: !Time
  } deriving (Eq, Show, Ord, Typeable, Data)

dateTime :: Year -> Month -> DayOfMonth -> HourOfDay -> MinuteOfHour -> SecondOfMinute -> DateTime
dateTime y m dom hod moh som =
 DateTime (Date y m dom) (Time hod moh som)

onDate :: (Date -> Date) -> DateTime -> DateTime
onDate f (DateTime d t) =
  DateTime (f d) t

onTime :: (Time -> Time) -> DateTime -> DateTime
onTime f (DateTime d t) =
  DateTime d (f t)

nextHour :: DateTime -> DateTime
nextHour (DateTime d (Time h m s)) =
  if hourOfDayToInt h >= 23
    then DateTime (nextDay d) (Time midnight m s)
    else DateTime d (Time (succ h) m s)

fromZoned :: TZ -> UTCTime -> DateTime
fromZoned z t =
  let (LocalTime d tod) = utcToLocalTimeTZ z t
   in DateTime (gregorianDayToDate d) (timeOfDayToTime tod)

local :: TZ -> IO DateTime
local tz =
  fromZoned tz <$> getCurrentTime

renderDateTime :: DateTime -> Text
renderDateTime (DateTime d t) =
  renderDate d <> " " <> renderTime t

parseDateTime :: Text -> Either String DateTime
parseDateTime =
  parseOnly dateTimeParser

parseDateTime' :: Text -> Maybe DateTime
parseDateTime' t =
  either (const Nothing) Just $ parseDateTime t

dateTimeParser :: Parser DateTime
dateTimeParser = do
  d <- dateParser
  _ <- char ' '
  t <- timeParser
  pure $ DateTime d t
