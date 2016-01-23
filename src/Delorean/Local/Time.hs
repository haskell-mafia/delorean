{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Delorean.Local.Time (
    HourOfDay
  , MinuteOfHour
  , SecondOfMinute
  , Time (..)
  , midnight
  , hourOfDayFromInt
  , hourOfDayToInt
  , minuteOfHourFromInt
  , minuteOfHourToInt
  , secondOfMinuteFromInt
  , secondOfMinuteToInt
  , timeOfDayToTime
  , renderMinuteOfHour
  , renderTime
  , parseMinuteOfHour
  , parseTime
  , minuteOfHourParser
  , timeParser
  ) where


import           Data.Attoparsec.Text
import           Data.Data (Data)
import           Data.Fixed (Pico)
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time hiding (midnight, parseTime, timeOfDayToTime)
import           Data.Typeable (Typeable)

import           P

import           Text.Printf

newtype HourOfDay =
  HourOfDay {
    unHourOfDay :: Int
  } deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data)

newtype MinuteOfHour =
  MinuteOfHour {
    unMinuteOfHour :: Int
  } deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data)

newtype SecondOfMinute =
  SecondOfMinute {
    unSecondOfMinute :: Int
  } deriving (Eq, Ord, Read, Show, Enum, Bounded, Typeable, Data)

data Time =
  Time {
    timeHour :: !HourOfDay
  , timeMinute :: !MinuteOfHour
  , timeSecond :: !SecondOfMinute
  } deriving (Eq, Ord, Read, Show, Typeable, Data)

hourOfDayFromInt :: Int -> Maybe HourOfDay
hourOfDayFromInt n =
  valueOrEmpty (n >= 0 && n < 24) $ HourOfDay n

hourOfDayToInt :: HourOfDay -> Int
hourOfDayToInt =
  unHourOfDay

minuteOfHourFromInt :: Int -> Maybe MinuteOfHour
minuteOfHourFromInt n =
  valueOrEmpty (n >= 0 && n < 60) $ MinuteOfHour n

minuteOfHourToInt :: MinuteOfHour -> Int
minuteOfHourToInt =
  unMinuteOfHour

secondOfMinuteFromInt :: Int -> Maybe SecondOfMinute
secondOfMinuteFromInt n =
  valueOrEmpty (n >= 0 && n < 60) $ SecondOfMinute n

secondOfMinuteToInt :: SecondOfMinute -> Int
secondOfMinuteToInt =
  unSecondOfMinute

timeOfDayToTime :: TimeOfDay -> Time
timeOfDayToTime tod =
  Time (HourOfDay $ todHour tod) (MinuteOfHour $ todMin tod) (SecondOfMinute $ picoToSeconds . todSec $ tod)

midnight :: HourOfDay
midnight =
  HourOfDay 0

renderMinuteOfHour :: MinuteOfHour -> Text
renderMinuteOfHour =
  T.pack . show . unMinuteOfHour

renderTime :: Time -> Text
renderTime (Time h m s) =
  T.pack $ printf "%02d:%02d:%02d" (hourOfDayToInt h) (minuteOfHourToInt m) (secondOfMinuteToInt s)

parseMinuteOfHour :: Text -> Either String MinuteOfHour
parseMinuteOfHour =
  parseOnly minuteOfHourParser

parseTime :: Text -> Either String Time
parseTime =
  parseOnly timeParser

minuteOfHourParser :: Parser MinuteOfHour
minuteOfHourParser =
  MinuteOfHour <$> decimal

timeParser :: Parser Time
timeParser = do
  h <- replicateM 2 digit
  _ <- char ':'
  m <- replicateM 2 digit
  _ <- char ':'
  s <- replicateM 2 digit
  maybe (fail $ "Invalid time values [" <> h <> ":" <>  m <> ":" <> s <> "].") pure $ do
    h' <- readMaybe h >>= hourOfDayFromInt
    m' <- readMaybe m >>= minuteOfHourFromInt
    s' <- readMaybe s >>= secondOfMinuteFromInt
    pure $ Time h' m' s'

picoToSeconds :: Pico -> Int
picoToSeconds =
  truncate . toRational
