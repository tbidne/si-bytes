-- | Entry point for running the unit test suite
module Main (main) where

import Data.Proxy (Proxy (..))
import ByteTypes.Props.Class.Conversion qualified as PConv
import ByteTypes.Props.Data.Bytes qualified as PBytes
import ByteTypes.Props.Data.Network.NetBytes qualified as PNetBytes
import ByteTypes.Props.Data.Network.SomeNetDir qualified as PSomeNetDir
import ByteTypes.Props.MaxRuns (MaxRuns)
import Test.Tasty qualified as T
import Test.Tasty.Options (OptionDescription (..))

-- | Runs all property tests
main :: IO ()
main = do
  let options = T.includingOptions [Option @MaxRuns Proxy]
      ingredients = options : T.defaultIngredients
  T.defaultMainWithIngredients ingredients $
    T.testGroup
      "Property tests"
      [ PConv.props,
        PBytes.props,
        PNetBytes.props,
        PSomeNetDir.props
      ]
