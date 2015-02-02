import           Control.Monad

import qualified DeloreanTest

import           System.Exit
import           System.IO


main :: IO ()
main =
  hSetBuffering stdout LineBuffering >> mapM id [
      DeloreanTest.tests
    ] >>= \rs -> when (not . all id $ rs) exitFailure
