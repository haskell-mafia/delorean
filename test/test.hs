import           Control.Monad

import qualified Delorean.DurationTest
import qualified Delorean.Local.DateTest
import qualified Delorean.Local.TimeTest
import qualified Delorean.Local.DateTimeTest
import qualified Delorean.Local.QQTest

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      Delorean.DurationTest.tests
    , Delorean.Local.DateTest.tests
    , Delorean.Local.TimeTest.tests
    , Delorean.Local.DateTimeTest.tests
    , Delorean.Local.QQTest.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
