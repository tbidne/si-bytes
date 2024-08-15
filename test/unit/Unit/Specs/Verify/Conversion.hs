{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides helpers for specs
module Unit.Specs.Verify.Conversion
  ( ExpectedConvs (..),
    convSpecs,
  )
where

import Data.Bytes.Class.Conversion (Conversion (Converted, convert_))
import Data.Bytes.Class.RawNumeric (RawNumeric (Raw, toRaw))
import Data.Bytes.Size (Size (B, E, G, K, M, P, T, Y, Z))
#if MIN_VERSION_base(4, 16, 0)
import Data.Kind (Constraint, Type)
#endif
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@=?))

data ExpectedConvs a = MkExpectedConvs
  { bExp :: !a,
    kExp :: !a,
    mExp :: !a,
    gExp :: !a,
    tExp :: !a,
    pExp :: !a,
    eExp :: !a,
    zExp :: !a,
    yExp :: !a
  }

#if MIN_VERSION_base(4, 16, 0)
type ConvC :: Size -> Type -> Type -> Constraint
class
  ( Eq n,
    Show n,
    RawNumeric (Converted s a),
    Raw (Converted s a) ~ n
  ) =>
  ConvC s a n

instance
  ( Eq n,
    Show n,
    RawNumeric (Converted s a),
    Raw (Converted s a) ~ n
  ) =>
  ConvC s a n
#else
type ConvC a n =
  ( Eq n,
    Show n,
    RawNumeric a,
    Raw a ~ n
  )
#endif

-- TODO: Ormolu does not like the cpp. Once it is gone, remove this comment.
{- ORMOLU_DISABLE -}

-- | Specs for conversion.
--
-- @since 0.1
convSpecs ::
  forall a b n.
  ( Conversion a,
    Conversion b,
#if !MIN_VERSION_base(4, 16, 0)
    ConvC (Converted B a) n,
    ConvC (Converted K a) n,
    ConvC (Converted M a) n,
    ConvC (Converted G a) n,
    ConvC (Converted T a) n,
    ConvC (Converted P a) n,
    ConvC (Converted E a) n,
    ConvC (Converted Z a) n,
    ConvC (Converted Y a) n,
    ConvC (Converted B b) n,
    ConvC (Converted K b) n,
    ConvC (Converted M b) n,
    ConvC (Converted G b) n,
    ConvC (Converted T b) n,
    ConvC (Converted P b) n,
    ConvC (Converted E b) n,
    ConvC (Converted Z b) n,
    ConvC (Converted Y b) n
#else
    forall s. ConvC s a n,
    forall s. ConvC s b n
#endif
  ) =>
  -- | Expectations.
  ExpectedConvs n ->
  -- | Constructor for minimum size.
  (Integer -> a) ->
  -- | Constructor for maximum size.
  (Integer -> b) ->
  TestTree
convSpecs MkExpectedConvs{..} minSizeCons maxSizeCons = testCase "Conversions" $ do
  bExp @=? toRaw (convert_ @_ @B (minSizeCons 1_000_000_000_000_000_000_000_000))
  kExp @=? toRaw (convert_ @_ @K (minSizeCons 1_000_000_000_000_000_000_000_000))
  mExp @=? toRaw (convert_ @_ @M (minSizeCons 1_000_000_000_000_000_000_000_000))
  gExp @=? toRaw (convert_ @_ @G (minSizeCons 1_000_000_000_000_000_000_000_000))
  tExp @=? toRaw (convert_ @_ @T (minSizeCons 1_000_000_000_000_000_000_000_000))
  pExp @=? toRaw (convert_ @_ @P (minSizeCons 1_000_000_000_000_000_000_000_000))
  eExp @=? toRaw (convert_ @_ @E (minSizeCons 1_000_000_000_000_000_000_000_000))
  zExp @=? toRaw (convert_ @_ @Z (minSizeCons 1_000_000_000_000_000_000_000_000))
  yExp @=? toRaw (convert_ @_ @Y (minSizeCons 1_000_000_000_000_000_000_000_000))

  bExp @=? toRaw (convert_ @_ @B (maxSizeCons 1))
  kExp @=? toRaw (convert_ @_ @K (maxSizeCons 1))
  mExp @=? toRaw (convert_ @_ @M (maxSizeCons 1))
  gExp @=? toRaw (convert_ @_ @G (maxSizeCons 1))
  tExp @=? toRaw (convert_ @_ @T (maxSizeCons 1))
  pExp @=? toRaw (convert_ @_ @P (maxSizeCons 1))
  eExp @=? toRaw (convert_ @_ @E (maxSizeCons 1))
  zExp @=? toRaw (convert_ @_ @Z (maxSizeCons 1))
  yExp @=? toRaw (convert_ @_ @Y (maxSizeCons 1))

{- ORMOLU_ENABLE -}
