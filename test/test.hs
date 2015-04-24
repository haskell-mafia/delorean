import           Control.Monad

import qualified Test.Delorean.Duration
import qualified Test.Delorean.Local.Date
import qualified Test.Delorean.Local.Time
import qualified Test.Delorean.Local.DateTime
import qualified Test.Delorean.Local.QQ

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      Test.Delorean.Duration.tests
    , Test.Delorean.Local.Date.tests
    , Test.Delorean.Local.Time.tests
    , Test.Delorean.Local.DateTime.tests
    , Test.Delorean.Local.QQ.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
