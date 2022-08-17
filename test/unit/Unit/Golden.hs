{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides helpers for golden tests
module Unit.Golden
  ( normGoldensForUnit,
    convGoldens,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.Bytes.Class.Conversion (Conversion (Converted, convert))
import Data.Bytes.Class.Normalize (Normalize (Norm, normalize))
import Data.Bytes.Size (Size (..))
import Data.Char qualified as Ch
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

-- | Golden tests for normalization.
--
-- @since 0.1
normGoldensForUnit ::
  forall a.
  ( Normalize a,
    Show (Norm a)
  ) =>
  -- | Type name, used in the file name.
  String ->
  -- | Unit, used in file name.
  Char ->
  -- | Constructor.
  (Float -> a) ->
  TestTree
normGoldensForUnit typeName desc cons =
  goldenVsString (desc : []) fp $
    pure $
      toStr results
  where
    fp =
      mconcat
        [ "test/unit/goldens/normalizations/",
          typeName,
          ('-' : Ch.toLower desc : []),
          ".golden"
        ]
    toStr = BS.fromStrict . Char8.pack . unlines . fmap show
    results =
      [ normalize (cons 0.250),
        normalize (cons 750),
        normalize (cons 1_500)
      ]

#if MIN_VERSION_base(4, 16, 0)
type ConvShow :: Size -> Type -> Constraint
class Show (Converted s a) => ConvShow s a

instance Show (Converted s a) => ConvShow s a
#endif

-- TODO: Ormolu does not like the cpp. Once it is gone, remove this comment.
{- ORMOLU_DISABLE -}

-- | Golden tests for normalization.
--
-- @since 0.1
convGoldens ::
  forall a b.
  ( Conversion a,
    Conversion b,
#if !MIN_VERSION_base(4, 16, 0)
    Show (Converted B a),
    Show (Converted K a),
    Show (Converted M a),
    Show (Converted G a),
    Show (Converted T a),
    Show (Converted P a),
    Show (Converted E a),
    Show (Converted Z a),
    Show (Converted Y a),
    Show (Converted B b),
    Show (Converted K b),
    Show (Converted M b),
    Show (Converted G b),
    Show (Converted T b),
    Show (Converted P b),
    Show (Converted E b),
    Show (Converted Z b),
    Show (Converted Y b)
#else
    forall s. ConvShow s a,
    forall s. ConvShow s b
#endif
  ) =>
  -- | Type name, used in the file name.
  String ->
  -- | Constructor for minimum size.
  (Integer -> a) ->
  -- | Constructor for maximum size.
  (Integer -> b) ->
  TestTree
convGoldens typeName minSizeCons maxSizeCons =
  goldenVsString "Conversion Goldens" fp $
    pure $
      toStr results
  where
    fp =
      mconcat
        [ "test/unit/goldens/conversions/",
          typeName,
          ".golden"
        ]
    toStr = BS.fromStrict . Char8.pack . unlines . fmap show
    results =
      [ show $ convert (Proxy @B) (minSizeCons 1_000_000_000_000_000_000_000_000),
        show $ convert (Proxy @K) (minSizeCons 1_000_000_000_000_000_000_000_000),
        show $ convert (Proxy @M) (minSizeCons 1_000_000_000_000_000_000_000_000),
        show $ convert (Proxy @G) (minSizeCons 1_000_000_000_000_000_000_000_000),
        show $ convert (Proxy @T) (minSizeCons 1_000_000_000_000_000_000_000_000),
        show $ convert (Proxy @P) (minSizeCons 1_000_000_000_000_000_000_000_000),
        show $ convert (Proxy @E) (minSizeCons 1_000_000_000_000_000_000_000_000),
        show $ convert (Proxy @Z) (minSizeCons 1_000_000_000_000_000_000_000_000),
        show $ convert (Proxy @Y) (minSizeCons 1_000_000_000_000_000_000_000_000),
        -- reverse sized order lets us easily verify the conversions at a glance
        -- (numbers should get smaller than larger three orders of magnitude
        -- at a time)
        show $ convert (Proxy @Y) (maxSizeCons 1),
        show $ convert (Proxy @Z) (maxSizeCons 1),
        show $ convert (Proxy @E) (maxSizeCons 1),
        show $ convert (Proxy @P) (maxSizeCons 1),
        show $ convert (Proxy @T) (maxSizeCons 1),
        show $ convert (Proxy @G) (maxSizeCons 1),
        show $ convert (Proxy @M) (maxSizeCons 1),
        show $ convert (Proxy @K) (maxSizeCons 1),
        show $ convert (Proxy @B) (maxSizeCons 1)
      ]
