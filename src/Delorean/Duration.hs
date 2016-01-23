{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Delorean.Duration (
    Duration (..)
  , durationToSeconds
  , renderDuration
  , parseDuration
  , durationParser
  ) where

import           Data.Attoparsec.Text
import           Data.Data (Data)
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)

import           P

data Duration =
    Seconds Int
  | Minutes Int
  | Hours Int
  deriving (Read, Show, Typeable, Data)

instance Eq Duration where
  (==) = on (==) durationToSeconds

instance Ord Duration where
  compare a b = on compare durationToSeconds a b

instance Monoid Duration where
  mempty = Seconds 0
  mappend a b = Seconds $ durationToSeconds a + durationToSeconds b

durationToSeconds :: Duration -> Int
durationToSeconds (Seconds n) =
  n
durationToSeconds (Minutes n) =
  n * 60
durationToSeconds (Hours n) =
  n * 60 * 60

renderDuration :: Duration -> Text
renderDuration (Seconds n) =
  (T.pack . show) n <> "s"
renderDuration (Minutes n) =
  (T.pack . show) n <> "m"
renderDuration (Hours n) =
  (T.pack . show) n <> "h"

parseDuration :: Text -> Either String Duration
parseDuration =
  parseOnly durationParser

durationParser :: Parser Duration
durationParser =
  decimal >>= \n -> choice [
      "s" *> pure (Seconds n)
    , "m" *> pure (Minutes n)
    , "h" *> pure (Hours n)
    ]
