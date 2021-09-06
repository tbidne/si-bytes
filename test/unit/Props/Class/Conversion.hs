-- | Property tests for 'Conversion'.
module Props.Class.Conversion (props) where

import ByteTypes.Class.Conversion qualified as Conv
import ByteTypes.Data.Size (ByteSize (..))
import GHC.Real (Ratio (..))
import Hedgehog (Gen, (===))
import Hedgehog qualified as H
import Props.Data.Size.Generators qualified as Gens
import Props.MaxRuns (MaxRuns (..))
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | 'TestTree' of properties.
props :: TestTree
props = T.testGroup "ByteTypes.Class.Conversion" [convertProps]

convertProps :: TestTree
convertProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Convert" $
    H.withTests limit $
      H.property $ do
        (s1, s2, n) <- H.forAll genConvertInput
        let expected = n * sizesToMult s1 s2
            result = Conv.convert s1 s2 n
        H.footnote $ "expected: " <> show expected
        H.footnote $ "result: " <> show result
        result === expected

genConvertInput :: Gen (ByteSize, ByteSize, Rational)
genConvertInput =
  (,,) <$> Gens.genByteSize <*> Gens.genByteSize <*> Gens.genD

szToRank :: ByteSize -> Int
szToRank B = 0
szToRank K = 1
szToRank M = 2
szToRank G = 3
szToRank T = 4
szToRank P = 5

intToMult :: Int -> Rational
intToMult n
  | n >= 0 = (1_000 ^ n) :% 1
  | otherwise = 1 :% (1_000 ^ abs n)

sizesToMult :: ByteSize -> ByteSize -> Rational
sizesToMult x y = intToMult $ szToRank x - szToRank y
