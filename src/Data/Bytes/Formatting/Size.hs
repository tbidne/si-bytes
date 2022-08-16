{-# LANGUAGE UndecidableInstances #-}

-- | Provides formatters for 'Size'.
--
-- @since 0.1
module Data.Bytes.Formatting.Size
  ( SizeFormat (..),
    _SizeFormatShort,
    _SizeFormatMedium,
    _SizeFormatLong,
    SizedFormatter (..),
    formatSize,
    sizedFormatterUnix,
    sizedFormatterNatural,
    sizedFormatterVerbose,
  )
where

import Data.Bytes.Formatting.Base (CaseFormat (..), Formatter (..), caseFormatToFn)
import Data.Bytes.Size (Size (..), Sized (..))
import Data.Default (Default (..))
import Data.Text (Text)
import Data.Text qualified as T
import Optics.Core
  ( A_Lens,
    LabelOptic (..),
    Prism',
    lens,
    prism,
    (^.),
  )
import Text.Printf (printf)

-- | Determines how to format the size units.
--
-- ==== __Examples__
--
-- >>> def @SizeFormat
-- SizeFormatMedium
--
-- @since 0.1
data SizeFormat
  = -- | @since 0.1
    SizeFormatShort
  | -- | @since 0.1
    SizeFormatMedium
  | -- | @since 0.1
    SizeFormatLong
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Default SizeFormat where
  def = SizeFormatMedium

-- | @since 0.1
_SizeFormatShort :: Prism' SizeFormat ()
_SizeFormatShort = prism (const SizeFormatShort) f
  where
    f SizeFormatShort = Right ()
    f other = Left other

-- | @since 0.1
_SizeFormatMedium :: Prism' SizeFormat ()
_SizeFormatMedium = prism (const SizeFormatMedium) f
  where
    f SizeFormatMedium = Right ()
    f other = Left other

-- | @since 0.1
_SizeFormatLong :: Prism' SizeFormat ()
_SizeFormatLong = prism (const SizeFormatLong) f
  where
    f SizeFormatLong = Right ()
    f other = Left other

-- | Formatting size units.
--
-- ==== __Examples__
--
-- >>> def @SizedFormatter
-- MkSizedFormatter {caseFormat = CaseFormatLower, leadingSpace = True, sizeFormat = SizeFormatMedium}
--
-- @since 0.1
data SizedFormatter = MkSizedFormatter
  { caseFormat :: CaseFormat,
    leadingSpace :: Bool,
    sizeFormat :: SizeFormat
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

instance Default SizedFormatter where
  def = MkSizedFormatter CaseFormatLower True def

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ CaseFormat, b ~ CaseFormat) =>
  LabelOptic "caseFormat" k SizedFormatter SizedFormatter a b
  where
  labelOptic = lens caseFormat (\f cf -> f {caseFormat = cf})

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ Bool, b ~ Bool) =>
  LabelOptic "leadingSpace" k SizedFormatter SizedFormatter a b
  where
  labelOptic = lens leadingSpace (\f ls -> f {leadingSpace = ls})

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ SizeFormat, b ~ SizeFormat) =>
  LabelOptic "sizeFormat" k SizedFormatter SizedFormatter a b
  where
  labelOptic = lens sizeFormat (\f sf -> f {sizeFormat = sf})

-- | @since 0.1
instance Formatter SizedFormatter where
  formatStr fmt =
    if fmt ^. #leadingSpace
      then " %s"
      else "%s"

-- | Unix-style formatter:
--
-- * Upper case.
-- * No space between value and unit.
-- * Single letter unit.
--
-- For example: @7G@.
--
-- @since 0.1
sizedFormatterUnix :: SizedFormatter
sizedFormatterUnix = MkSizedFormatter CaseFormatUpper False SizeFormatShort

-- | "Natural"-style formatter:
--
-- * Lower case.
-- * Space between value and unit.
-- * Two letter units.
--
-- For example: @7 gb@.
--
-- @since 0.1
sizedFormatterNatural :: SizedFormatter
sizedFormatterNatural = MkSizedFormatter CaseFormatLower True SizeFormatMedium

-- | Verbose formatter:
--
-- * Lower case.
-- * Space between value and unit.
-- * Full spelling.
--
-- For example: @7 gigabytes@.
--
-- @since 0.1
sizedFormatterVerbose :: SizedFormatter
sizedFormatterVerbose = MkSizedFormatter CaseFormatLower True SizeFormatLong

-- | Formats size units.
--
-- @since 0.1
formatSize :: Sized a => SizedFormatter -> a -> Text
formatSize fmt x = T.pack $ printf (T.unpack $ formatStr fmt) size'
  where
    size = formatSize' (fmt ^. #sizeFormat) (sizeOf x)
    size' = caseFormatToFn (fmt ^. #caseFormat) size

-- | Formats the size.
--
-- @since 0.1
formatSize' :: SizeFormat -> Size -> Text
formatSize' SizeFormatShort = formatSizeShort
formatSize' SizeFormatMedium = formatSizeMedium
formatSize' SizeFormatLong = formatSizeLong

formatSizeShort :: Size -> Text
formatSizeShort B = "B"
formatSizeShort K = "K"
formatSizeShort M = "M"
formatSizeShort G = "G"
formatSizeShort T = "T"
formatSizeShort P = "P"
formatSizeShort E = "E"
formatSizeShort Z = "Z"
formatSizeShort Y = "Y"

formatSizeMedium :: Size -> Text
formatSizeMedium B = "B"
formatSizeMedium K = "KB"
formatSizeMedium M = "MB"
formatSizeMedium G = "GB"
formatSizeMedium T = "TB"
formatSizeMedium P = "PB"
formatSizeMedium E = "EB"
formatSizeMedium Z = "ZB"
formatSizeMedium Y = "YB"

formatSizeLong :: Size -> Text
formatSizeLong B = "Bytes"
formatSizeLong K = "Kilobytes"
formatSizeLong M = "Megabytes"
formatSizeLong G = "Gigabytes"
formatSizeLong T = "Terabytes"
formatSizeLong P = "Petabytes"
formatSizeLong E = "Exabytes"
formatSizeLong Z = "Zettabytes"
formatSizeLong Y = "Yottabytes"
