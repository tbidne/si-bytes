-- | Entry point for running the unit test suite
module Main (main) where

import Data.Proxy (Proxy (..))
import Test.Tasty qualified as T
import Test.Tasty.Options (OptionDescription (..))
import Unit.Data.Bytes qualified as PBytes
import Unit.Data.Bytes.Class.Conversion qualified as PConv
import Unit.Data.Bytes.Network qualified as PNetwork
import Unit.Props.MaxRuns (MaxRuns)

-- | Runs all unit tests
main :: IO ()
main = do
  let options = T.includingOptions [Option @MaxRuns Proxy]
      ingredients = options : T.defaultIngredients
  T.defaultMainWithIngredients ingredients $
    T.testGroup
      "Unit tests"
      [ PConv.tests,
        PBytes.tests,
        PNetwork.tests
      ]
