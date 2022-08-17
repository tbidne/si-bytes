-- | Parsing verifiers
--
-- @since 0.1
module Unit.Props.Verify.Parsing
  ( parsingRoundTrip,
  )
where

import Data.Bytes.Class.Parser (Parser, parse)
import Data.Text (Text)
import Data.Text qualified as T
import Hedgehog (Gen, (===))
import Hedgehog qualified as H
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Unit.Props.MaxRuns (MaxRuns (..))
import Unit.Utils qualified as U

-- | Tests that @parsing . format@ is a round trip.
--
-- @since 0.1
parsingRoundTrip ::
  forall a f.
  ( Eq a,
    Parser a,
    Show a,
    Show f
  ) =>
  -- | Generator for bytes type.
  Gen a ->
  -- | Generator for formatter.
  Gen f ->
  -- | Format function.
  (f -> a -> Text) ->
  TestTree
parsingRoundTrip genX genFmt fmt = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat "parse . format is a round trip" "parsingRoundTrip" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll genX
        f <- H.forAll genFmt
        let formatted = fmt f x
        H.annotate $ T.unpack formatted
        case parse @a formatted of
          Left err -> do
            H.annotate (T.unpack err)
            H.failure
          Right x' -> do
            H.annotateShow x
            x === x'