-- | Property tests for 'Size'.
module Props.Data.Size (props) where

import ByteTypes.Data.Size (ByteSize (..))
import ByteTypes.Data.Size qualified as S
import GHC.Real (Ratio (..))
import Hedgehog (Gen)
import Hedgehog qualified as H
import Props.Data.Size.Generators qualified as Gens
import Props.MaxRuns (MaxRuns (..))
import Props.Utils qualified as PropUtils
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Test.Tasty.Hedgehog qualified as TH

-- | 'TestTree' of properties.
props :: TestTree
props = T.testGroup "Bytes.Data.Size" [convertProps]

convertProps :: TestTree
convertProps = T.askOption $ \(MkMaxRuns limit) ->
  TH.testProperty "Convert" $
    H.withTests limit $
      H.property $ do
        (s1, s2, n) <- H.forAll genConvertInput
        let expected = n * sizesToMult s1 s2
            result = S.convert s1 s2 n
        H.footnote $ "expected: " <> show expected
        H.footnote $ "result: " <> show result
        PropUtils.rationalEq result expected

genConvertInput :: Gen (ByteSize, ByteSize, Rational)
genConvertInput =
  (,,) <$> Gens.genByteSize <*> Gens.genByteSize <*> Gens.genD

szToRank :: ByteSize -> Int
szToRank B = 0
szToRank KB = 1
szToRank MB = 2
szToRank GB = 3
szToRank TB = 4
szToRank PB = 5

intToMult :: Int -> Rational
intToMult n
  | n >= 0 = (1_000 ^ n) :% 1
  | otherwise = 1 :% (1_000 ^ abs n)

sizesToMult :: ByteSize -> ByteSize -> Rational
sizesToMult x y = intToMult $ szToRank x - szToRank y
