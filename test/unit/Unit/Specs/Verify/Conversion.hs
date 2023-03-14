{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides helpers for specs
module Unit.Specs.Verify.Conversion
  ( ExpectedConvs (..),
    convSpecs,
  )
where

import Data.Bytes.Class.Conversion (Conversion (Converted, convert))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Size (Size (..))
#if MIN_VERSION_base(4, 16, 0)
import Data.Kind (Constraint, Type)
#endif
import Data.Proxy (Proxy (Proxy))
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
    Unwrapper (Converted s a),
    Unwrapped (Converted s a) ~ n
  ) =>
  ConvC s a n

instance
  ( Eq n,
    Show n,
    Unwrapper (Converted s a),
    Unwrapped (Converted s a) ~ n
  ) =>
  ConvC s a n
#else
type ConvC a n =
  ( Eq n,
    Show n,
    Unwrapper a,
    Unwrapped a ~ n
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
  bExp @=? unwrap (convert (Proxy @B) (minSizeCons 1_000_000_000_000_000_000_000_000))
  kExp @=? unwrap (convert (Proxy @K) (minSizeCons 1_000_000_000_000_000_000_000_000))
  mExp @=? unwrap (convert (Proxy @M) (minSizeCons 1_000_000_000_000_000_000_000_000))
  gExp @=? unwrap (convert (Proxy @G) (minSizeCons 1_000_000_000_000_000_000_000_000))
  tExp @=? unwrap (convert (Proxy @T) (minSizeCons 1_000_000_000_000_000_000_000_000))
  pExp @=? unwrap (convert (Proxy @P) (minSizeCons 1_000_000_000_000_000_000_000_000))
  eExp @=? unwrap (convert (Proxy @E) (minSizeCons 1_000_000_000_000_000_000_000_000))
  zExp @=? unwrap (convert (Proxy @Z) (minSizeCons 1_000_000_000_000_000_000_000_000))
  yExp @=? unwrap (convert (Proxy @Y) (minSizeCons 1_000_000_000_000_000_000_000_000))

  bExp @=? unwrap (convert (Proxy @B) (maxSizeCons 1))
  kExp @=? unwrap (convert (Proxy @K) (maxSizeCons 1))
  mExp @=? unwrap (convert (Proxy @M) (maxSizeCons 1))
  gExp @=? unwrap (convert (Proxy @G) (maxSizeCons 1))
  tExp @=? unwrap (convert (Proxy @T) (maxSizeCons 1))
  pExp @=? unwrap (convert (Proxy @P) (maxSizeCons 1))
  eExp @=? unwrap (convert (Proxy @E) (maxSizeCons 1))
  zExp @=? unwrap (convert (Proxy @Z) (maxSizeCons 1))
  yExp @=? unwrap (convert (Proxy @Y) (maxSizeCons 1))

{- ORMOLU_ENABLE -}
