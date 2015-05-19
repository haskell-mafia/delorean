{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Delorean.Local.QQ (
    qdate
  , qtime
  , qdatetime
  ) where

import           Delorean.Local.Date
import           Delorean.Local.Time
import           Delorean.Local.DateTime

import           Language.Haskell.TH.Quote

import           X.Language.Haskell.TH

qdate :: QuasiQuoter
qdate = qeither parseDate

qtime :: QuasiQuoter
qtime = qeither parseTime

qdatetime :: QuasiQuoter
qdatetime = qeither parseDateTime
