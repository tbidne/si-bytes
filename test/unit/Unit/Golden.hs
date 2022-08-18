{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides helpers for golden tests
module Unit.Golden
  ( normGoldensForUnit,
    convGoldens,
    formatGoldens,

    -- * Formatters
    intSizedFormatters,
    floatSizedFormatters,
    intSizeDirFormatters,
    floatSizeDirFormatters,
  )
where

import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as BSL
import Data.Bytes.Class.Conversion (Conversion (Converted, convert))
import Data.Bytes.Class.Normalize (Normalize (..))
import Data.Bytes.Class.Wrapper (Unwrapper (..))
import Data.Bytes.Formatting
  ( Default (def),
    DirectedFormatter,
    DirectionFormat (DirectionFormatShort),
    FloatingFormatter (MkFloatingFormatter),
    IntegralFormatter (MkIntegralFormatter),
    SizedFormatter,
  )
import Data.Bytes.Formatting qualified as Formatting
import Data.Bytes.Formatting.Base (BaseFormatter, CaseFormat (CaseFormatTitle), Formatter)
import Data.Bytes.Formatting.Size (SizeFormat (SizeFormatLong))
import Data.Bytes.Network.Direction (Directed)
import Data.Bytes.Size (Size (..), Sized)
import Data.Char qualified as Ch
#if MIN_VERSION_base(4, 16, 0)
import Data.Kind (Constraint, Type)
#endif
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Optics.Core (A_Lens, LabelOptic, set')
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import Text.Printf (PrintfArg)

-- | Golden tests for normalization.
--
-- @since 0.1
normGoldensForUnit ::
  forall a.
  ( Normalize a,
    Show (Norm a)
  ) =>
  -- | File name.
  String ->
  -- | Unit, used in file name.
  Char ->
  -- | Constructor.
  (Float -> a) ->
  TestTree
normGoldensForUnit fileName desc cons =
  goldensFromText [desc] "normalizations/" (fileName <> ['-', Ch.toLower desc]) results
  where
    results =
      [ show $ normalize (cons 0.250),
        show $ normalize (cons 750),
        show $ normalize (cons 1_500)
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
  -- | File name.
  String ->
  -- | Constructor for minimum size.
  (Integer -> a) ->
  -- | Constructor for maximum size.
  (Integer -> b) ->
  TestTree
convGoldens fileName minSizeCons maxSizeCons =
  goldensFromText "Goldens" "conversions/" fileName results
  where
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

{- ORMOLU_ENABLE -}

-- | Golden tests for formatting.
--
-- @since 0.1
formatGoldens ::
  -- | Test description.
  String ->
  -- | File name.
  FilePath ->
  -- | The bytes term.
  a ->
  -- | List of formatters to apply.
  [a -> Text] ->
  TestTree
formatGoldens desc fileName x formatters =
  goldensFromText desc "formatting/" fileName results
  where
    results = T.unpack . ($ x) <$> formatters

-- | Creates a golden tests for a term along with its text transformations.
--
-- @since 0.1
goldensFromText ::
  -- | The test name.
  String ->
  -- | The golden subdirectory.
  FilePath ->
  -- | File name.
  FilePath ->
  -- | List of result text to compare against golden expectations.
  [String] ->
  TestTree
goldensFromText testName baseFp typeName results = do
  goldenVsString testName fp $
    pure $
      listToBs results
  where
    fp =
      mconcat
        [ "test/unit/goldens/",
          baseFp,
          typeName,
          ".golden"
        ]

listToBs :: [String] -> BSL.ByteString
listToBs = BSL.fromStrict . Char8.pack . unlines

intSizedFormatters ::
  ( BaseFormatter (Unwrapped a) ~ b,
    b ~ IntegralFormatter,
    Formatter b,
    PrintfArg (Unwrapped a),
    Sized a,
    Unwrapper a
  ) =>
  [a -> Text]
intSizedFormatters =
  [ Formatting.formatSized MkIntegralFormatter Formatting.sizedFormatterUnix,
    Formatting.formatSized MkIntegralFormatter Formatting.sizedFormatterNatural,
    Formatting.formatSized MkIntegralFormatter Formatting.sizedFormatterVerbose,
    Formatting.formatSized MkIntegralFormatter def,
    Formatting.formatSized MkIntegralFormatter (titleCase . sizeLong $ def)
  ]

floatSizedFormatters ::
  ( BaseFormatter (Unwrapped a) ~ b,
    b ~ FloatingFormatter,
    Formatter b,
    PrintfArg (Unwrapped a),
    Sized a,
    Unwrapper a
  ) =>
  [a -> Text]
floatSizedFormatters =
  [ Formatting.formatSized (floatFormatter 2) Formatting.sizedFormatterUnix,
    Formatting.formatSized (floatFormatter 3) Formatting.sizedFormatterNatural,
    Formatting.formatSized (floatFormatter 4) Formatting.sizedFormatterVerbose,
    Formatting.formatSized (floatFormatter 5) def,
    Formatting.formatSized (MkFloatingFormatter Nothing) (titleCase . sizeLong $ def)
  ]

intSizeDirFormatters ::
  ( BaseFormatter (Unwrapped a) ~ b,
    Directed a,
    IntegralFormatter ~ b,
    Formatter b,
    PrintfArg (Unwrapped a),
    Sized a,
    Unwrapper a
  ) =>
  [a -> Text]
intSizeDirFormatters =
  [ Formatting.formatSized MkIntegralFormatter Formatting.sizedFormatterUnix,
    Formatting.formatSizedDirected MkIntegralFormatter Formatting.sizedFormatterUnix Formatting.directedFormatterUnix,
    Formatting.formatSizedDirected MkIntegralFormatter Formatting.sizedFormatterNatural Formatting.directedFormatterUnix,
    Formatting.formatSizedDirected MkIntegralFormatter Formatting.sizedFormatterVerbose Formatting.directedFormatterVerbose,
    Formatting.formatSizedDirected MkIntegralFormatter def def,
    Formatting.formatSizedDirected MkIntegralFormatter (titleCase . sizeLong $ def) (titleCase . dirShort $ def)
  ]

floatSizeDirFormatters ::
  ( BaseFormatter (Unwrapped a) ~ b,
    Directed a,
    FloatingFormatter ~ b,
    Formatter b,
    PrintfArg (Unwrapped a),
    Sized a,
    Unwrapper a
  ) =>
  [a -> Text]
floatSizeDirFormatters =
  [ Formatting.formatSized (floatFormatter 2) Formatting.sizedFormatterUnix,
    Formatting.formatSizedDirected (floatFormatter 3) Formatting.sizedFormatterUnix Formatting.directedFormatterUnix,
    Formatting.formatSizedDirected (floatFormatter 4) Formatting.sizedFormatterNatural Formatting.directedFormatterUnix,
    Formatting.formatSizedDirected (floatFormatter 5) Formatting.sizedFormatterVerbose Formatting.directedFormatterVerbose,
    Formatting.formatSizedDirected (MkFloatingFormatter Nothing) def def,
    Formatting.formatSizedDirected (floatFormatter 2) (titleCase . sizeLong $ def) (titleCase . dirShort $ def)
  ]

floatFormatter :: Word8 -> FloatingFormatter
floatFormatter = MkFloatingFormatter . Just

titleCase :: LabelOptic "caseFormat" A_Lens n n CaseFormat CaseFormat => n -> n
titleCase = set' #caseFormat CaseFormatTitle

sizeLong :: SizedFormatter -> SizedFormatter
sizeLong = set' #sizeFormat SizeFormatLong

dirShort :: DirectedFormatter -> DirectedFormatter
dirShort = set' #directionFormat DirectionFormatShort
