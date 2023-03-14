{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides helpers for specs
module Unit.Specs.Verify.Formatting
  ( formatSpecs,

    -- * Formatters
    intSizedFormatters,
    floatSizedFormatters,
    intSizeDirFormatters,
    floatSizeDirFormatters,
  )
where

import Control.Monad (when, zipWithM_)
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
import Data.Bytes.Size (Sized)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Optics.Core (A_Lens, LabelOptic, set')
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import Text.Printf (PrintfArg)

-- | Specs for formatting.
--
-- @since 0.1
formatSpecs ::
  -- | Test description.
  String ->
  -- | The bytes term.
  a ->
  -- | List of formatters to apply.
  [a -> Text] ->
  -- | Expectations
  [String] ->
  TestTree
formatSpecs desc x formatters expected = testCase desc $ do
  let lenExpects = length expected
  when (lenExpects /= length formatters) $
    assertFailure $
      mconcat
        [ "Received mismatched expectations (",
          show lenExpects,
          ") and formatters (",
          show (length formatters),
          ")"
        ]

  zipWithM_ (@=?) expected results
  where
    results = T.unpack . ($ x) <$> formatters

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

titleCase :: (LabelOptic "caseFormat" A_Lens n n CaseFormat CaseFormat) => n -> n
titleCase = set' #caseFormat CaseFormatTitle

sizeLong :: SizedFormatter -> SizedFormatter
sizeLong = set' #sizeFormat SizeFormatLong

dirShort :: DirectedFormatter -> DirectedFormatter
dirShort = set' #directionFormat DirectionFormatShort
