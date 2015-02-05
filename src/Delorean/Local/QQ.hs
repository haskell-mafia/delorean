{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Delorean.Local.QQ (
    qdate
  , qtime
  , qdatetime
  ) where

import           Data.String (String)
import qualified Data.Text as T

import           Delorean.Local.Date
import           Delorean.Local.Time
import           Delorean.Local.DateTime

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           P

import qualified Prelude as P (error)

qdate :: QuasiQuoter
qdate = QuasiQuoter {
    quoteExp = dateExp
  , quotePat = P.error "not able to qq pats"
  , quoteType = P.error "not able to qq types"
  , quoteDec = P.error "not able to qq a dec"
  }
  where
    dateExp :: String -> ExpQ
    dateExp s =
      case parseDate (T.pack s) of
        Left e ->
          P.error e
        Right d ->
          dataToExpQ (const Nothing) d

qtime :: QuasiQuoter
qtime = QuasiQuoter {
    quoteExp = timeExp
  , quotePat = P.error "not able to qq pats"
  , quoteType = P.error "not able to qq types"
  , quoteDec = P.error "not able to qq a dec"
  }
  where
    timeExp :: String -> ExpQ
    timeExp s =
      case parseTime (T.pack s) of
        Left e ->
          P.error e
        Right t ->
          dataToExpQ (const Nothing) t

qdatetime :: QuasiQuoter
qdatetime = QuasiQuoter {
    quoteExp = datetimeExp
  , quotePat = P.error "not able to qq pats"
  , quoteType = P.error "not able to qq types"
  , quoteDec = P.error "not able to qq a dec"
  }
  where
    datetimeExp :: String -> ExpQ
    datetimeExp s =
      case parseDateTime (T.pack s) of
        Left e ->
          P.error e
        Right dt ->
          dataToExpQ (const Nothing) dt
