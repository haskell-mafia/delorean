{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Delorean.Local.QQ (
    qdate
  , qtime
  , qdatetime
  , qhod
  , qmoh
  , qsom
  ) where

import qualified Data.Text as T

import           Delorean.Local.Date
import           Delorean.Local.Time
import           Delorean.Local.DateTime

import           Language.Haskell.TH.Quote

import           P

import           X.Language.Haskell.TH


qdate :: QuasiQuoter
qdate = qeither parseDate

qtime :: QuasiQuoter
qtime = qeither parseTime

qdatetime :: QuasiQuoter
qdatetime = qeither parseDateTime

qhod :: QuasiQuoter
qhod = qmaybe (\s -> readMaybe (T.unpack s) >>= hourOfDayFromInt)

qmoh :: QuasiQuoter
qmoh = qmaybe (\s -> readMaybe (T.unpack s) >>= minuteOfHourFromInt)

qsom :: QuasiQuoter
qsom = qmaybe (\s -> readMaybe (T.unpack s) >>= secondOfMinuteFromInt)
