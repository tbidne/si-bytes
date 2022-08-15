{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Exports functions for verifying 'Conversion' properties.
module Unit.Props.Verify.Conversion
  ( testConvertToAll,
    ExpectedConvs (..),
    expectedB,
    expectedK,
    expectedM,
    expectedG,
    expectedT,
    expectedP,
    expectedE,
    expectedZ,
    expectedY,
  )
where

import Data.Bytes.Class.Conversion (Conversion (..))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Size (Size (..))
#if MIN_VERSION_base(4, 16, 0)
import Data.Kind (Constraint, Type)
#endif
import Data.Proxy (Proxy (..))
import Hedgehog (Gen, (===))
import Hedgehog qualified as H
import Numeric.Algebra (AMonoid, MGroup (..), MSemigroup (..), Semifield)
import Numeric.Algebra qualified as Algebra
import Numeric.Data.NonZero (NonZero)
import Numeric.Literal.Integer (FromInteger (..))
import Test.Tasty (TestTree)
import Test.Tasty qualified as T
import Unit.Props.MaxRuns (MaxRuns (..))
import Unit.Props.Utils qualified as U

data ExpectedConvs n = MkExpectedConvs
  { bExp :: n -> n,
    kExp :: n -> n,
    mExp :: n -> n,
    gExp :: n -> n,
    tExp :: n -> n,
    pExp :: n -> n,
    eExp :: n -> n,
    zExp :: n -> n,
    yExp :: n -> n
  }

expectedB :: (Eq n, FromInteger n, Semifield n) => ExpectedConvs n
expectedB =
  MkExpectedConvs
    { bExp = id,
      kExp = (.%. nzafromInteger 1_000),
      mExp = (.%. nzafromInteger 1_000_000),
      gExp = (.%. nzafromInteger 1_000_000_000),
      tExp = (.%. nzafromInteger 1_000_000_000_000),
      pExp = (.%. nzafromInteger 1_000_000_000_000_000),
      eExp = (.%. nzafromInteger 1_000_000_000_000_000_000),
      zExp = (.%. nzafromInteger 1_000_000_000_000_000_000_000),
      yExp = (.%. nzafromInteger 1_000_000_000_000_000_000_000_000)
    }

expectedK :: (Eq n, FromInteger n, Semifield n) => ExpectedConvs n
expectedK =
  MkExpectedConvs
    { bExp = (.*. afromInteger 1_000),
      kExp = id,
      mExp = (.%. nzafromInteger 1_000),
      gExp = (.%. nzafromInteger 1_000_000),
      tExp = (.%. nzafromInteger 1_000_000_000),
      pExp = (.%. nzafromInteger 1_000_000_000_000),
      eExp = (.%. nzafromInteger 1_000_000_000_000_000),
      zExp = (.%. nzafromInteger 1_000_000_000_000_000_000),
      yExp = (.%. nzafromInteger 1_000_000_000_000_000_000_000)
    }

expectedM :: (Eq n, FromInteger n, Semifield n) => ExpectedConvs n
expectedM =
  MkExpectedConvs
    { bExp = (.*. afromInteger 1_000_000),
      kExp = (.*. afromInteger 1_000),
      mExp = id,
      gExp = (.%. nzafromInteger 1_000),
      tExp = (.%. nzafromInteger 1_000_000),
      pExp = (.%. nzafromInteger 1_000_000_000),
      eExp = (.%. nzafromInteger 1_000_000_000_000),
      zExp = (.%. nzafromInteger 1_000_000_000_000_000),
      yExp = (.%. nzafromInteger 1_000_000_000_000_000_000)
    }

expectedG :: (Eq n, FromInteger n, Semifield n) => ExpectedConvs n
expectedG =
  MkExpectedConvs
    { bExp = (.*. afromInteger 1_000_000_000),
      kExp = (.*. afromInteger 1_000_000),
      mExp = (.*. afromInteger 1_000),
      gExp = id,
      tExp = (.%. nzafromInteger 1_000),
      pExp = (.%. nzafromInteger 1_000_000),
      eExp = (.%. nzafromInteger 1_000_000_000),
      zExp = (.%. nzafromInteger 1_000_000_000_000),
      yExp = (.%. nzafromInteger 1_000_000_000_000_000)
    }

expectedT :: (Eq n, FromInteger n, Semifield n) => ExpectedConvs n
expectedT =
  MkExpectedConvs
    { bExp = (.*. afromInteger 1_000_000_000_000),
      kExp = (.*. afromInteger 1_000_000_000),
      mExp = (.*. afromInteger 1_000_000),
      gExp = (.*. afromInteger 1_000),
      tExp = id,
      pExp = (.%. nzafromInteger 1_000),
      eExp = (.%. nzafromInteger 1_000_000),
      zExp = (.%. nzafromInteger 1_000_000_000),
      yExp = (.%. nzafromInteger 1_000_000_000_000)
    }

expectedP :: (Eq n, FromInteger n, Semifield n) => ExpectedConvs n
expectedP =
  MkExpectedConvs
    { bExp = (.*. afromInteger 1_000_000_000_000_000),
      kExp = (.*. afromInteger 1_000_000_000_000),
      mExp = (.*. afromInteger 1_000_000_000),
      gExp = (.*. afromInteger 1_000_000),
      tExp = (.*. afromInteger 1_000),
      pExp = id,
      eExp = (.%. nzafromInteger 1_000),
      zExp = (.%. nzafromInteger 1_000_000),
      yExp = (.%. nzafromInteger 1_000_000_000)
    }

expectedE :: (Eq n, FromInteger n, Semifield n) => ExpectedConvs n
expectedE =
  MkExpectedConvs
    { bExp = (.*. afromInteger 1_000_000_000_000_000_000),
      kExp = (.*. afromInteger 1_000_000_000_000_000),
      mExp = (.*. afromInteger 1_000_000_000_000),
      gExp = (.*. afromInteger 1_000_000_000),
      tExp = (.*. afromInteger 1_000_000),
      pExp = (.*. afromInteger 1_000),
      eExp = id,
      zExp = (.%. nzafromInteger 1_000),
      yExp = (.%. nzafromInteger 1_000_000)
    }

expectedZ :: (Eq n, FromInteger n, Semifield n) => ExpectedConvs n
expectedZ =
  MkExpectedConvs
    { bExp = (.*. afromInteger 1_000_000_000_000_000_000_000),
      kExp = (.*. afromInteger 1_000_000_000_000_000_000),
      mExp = (.*. afromInteger 1_000_000_000_000_000),
      gExp = (.*. afromInteger 1_000_000_000_000),
      tExp = (.*. afromInteger 1_000_000_000),
      pExp = (.*. afromInteger 1_000_000),
      eExp = (.*. afromInteger 1_000),
      zExp = id,
      yExp = (.%. nzafromInteger 1_000)
    }

expectedY :: (FromInteger n, MSemigroup n) => ExpectedConvs n
expectedY =
  MkExpectedConvs
    { bExp = (.*. afromInteger 1_000_000_000_000_000_000_000_000),
      kExp = (.*. afromInteger 1_000_000_000_000_000_000_000),
      mExp = (.*. afromInteger 1_000_000_000_000_000_000),
      gExp = (.*. afromInteger 1_000_000_000_000_000),
      tExp = (.*. afromInteger 1_000_000_000_000),
      pExp = (.*. afromInteger 1_000_000_000),
      eExp = (.*. afromInteger 1_000_000),
      zExp = (.*. afromInteger 1_000),
      yExp = id
    }

nzafromInteger :: (AMonoid n, Eq n, FromInteger n) => Integer -> NonZero n
nzafromInteger = Algebra.unsafeAMonoidNonZero . afromInteger

#if MIN_VERSION_base(4, 16, 0)
-- | This class exists so that we can tell testConvertToAll that
-- @Unwrapped (Converted s a) ~ c@ for /all/ @s@. We would like to write
-- it directly i.e. @forall s. Unwrapped (Converted s a) ~ c@, but this runs
-- afoul of the type checker; see:
--
-- * https://gitlab.haskell.org/ghc/ghc/-/issues/17959
-- * https://gitlab.haskell.org/ghc/ghc/-/issues/14046
--
-- As a workaround, we can use this class to separate the quantified
-- constraint from the type family, which is the whole problem.
--
-- This saves us from having to write e.g.
--
-- @
-- Unwrapper (Converted B a),
-- Unwrapped (Converted B a) ~ c
-- ...
-- @
--
-- for every single size.
type ConvEquality :: Size -> Type -> Type -> Constraint
class (Unwrapper (Converted s a), Unwrapped (Converted s a) ~ c) => ConvEquality s a c

instance (Unwrapper (Converted s a), Unwrapped (Converted s a) ~ c) => ConvEquality s a c

-- NB. The cpp exists because this trick seems to only work on ghc 9.2+.
-- On earlier versions we still get errors like "Could not deduce
-- Unwrapper (Converted B a)" for every single size. Might be related to
-- this issue: https://gitlab.haskell.org/ghc/ghc/-/issues/21037
--
-- Sadly this means we have to list all sizes in these cases.
-- The -XQuantifiedConstraints logic is left in so we can use it
-- unconditionally once we drop support for ghc < 9.2.
#endif

-- TODO: Ormolu does not like the cpp. Once it is gone, remove this comment.
{- ORMOLU_DISABLE -}

-- | For a bytes type with fixed size @s@, test that all @s -> t@ conversions
-- are performed correctly.
testConvertToAll ::
  forall a c.
  ( Eq (Unwrapped a),
    Show a,
    Show (Unwrapped a),
    Unwrapper a,
    Unwrapped a ~ c,
#if !MIN_VERSION_base(4, 16, 0)
    Unwrapper (Converted B a),
    Unwrapped (Converted B a) ~ c,
    Unwrapper (Converted K a),
    Unwrapped (Converted K a) ~ c,
    Unwrapper (Converted M a),
    Unwrapped (Converted M a) ~ c,
    Unwrapper (Converted G a),
    Unwrapped (Converted G a) ~ c,
    Unwrapper (Converted T a),
    Unwrapped (Converted T a) ~ c,
    Unwrapper (Converted P a),
    Unwrapped (Converted P a) ~ c,
    Unwrapper (Converted E a),
    Unwrapped (Converted E a) ~ c,
    Unwrapper (Converted Z a),
    Unwrapped (Converted Z a) ~ c,
    Unwrapper (Converted Y a),
    Unwrapped (Converted Y a) ~ c,
#else
    forall s. ConvEquality s a c,
#endif
    Conversion a
  ) =>
  -- | Generator for the type to test
  Gen a ->
  -- | The expectations for each conversion
  ExpectedConvs c ->
  -- | Test description
  T.TestName ->
  [TestTree]
testConvertToAll gen e desc = f <$> [minBound .. maxBound]
  where
    f B = testConversion gen (convert @_ @B Proxy) (bExp e) (desc <> " -> B")
    f K = testConversion gen (convert @_ @K Proxy) (kExp e) (desc <> " -> K")
    f M = testConversion gen (convert @_ @M Proxy) (mExp e) (desc <> " -> M")
    f G = testConversion gen (convert @_ @G Proxy) (gExp e) (desc <> " -> G")
    f T = testConversion gen (convert @_ @T Proxy) (tExp e) (desc <> " -> T")
    f P = testConversion gen (convert @_ @P Proxy) (pExp e) (desc <> " -> P")
    f E = testConversion gen (convert @_ @E Proxy) (eExp e) (desc <> " -> E")
    f Z = testConversion gen (convert @_ @Z Proxy) (zExp e) (desc <> " -> Z")
    f Y = testConversion gen (convert @_ @Y Proxy) (yExp e) (desc <> " -> Y")

-- TODO: Remove when cpp is gone.
{- ORMOLU_ENABLE -}

-- | Tests that a bytes conversion matches an expectation. More precisely,
-- for a given bytes b with @unwrap b === x@, tests that
-- @unwrap (conv x) === expect x@.
testConversion ::
  ( Eq c,
    Show a,
    Show c,
    Unwrapper a,
    Unwrapped a ~ c,
    Unwrapper b,
    Unwrapped b ~ c
  ) =>
  -- | Generator for the type we want to test
  Gen a ->
  -- | Conversion function to apply
  (a -> b) ->
  -- | Expectation function
  (c -> c) ->
  -- | Test description
  String ->
  TestTree
testConversion gen convFn expectedFn desc = T.askOption $ \(MkMaxRuns limit) ->
  U.testPropertyCompat desc "testConversion" $
    H.withTests limit $
      H.property $ do
        x <- H.forAll gen
        let x' = unwrap x
            result = unwrap $ convFn x
        expectedFn x' === result