{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Provides types for abstracting over formatters.
--
-- @since 0.1
module Data.Bytes.Formatting.Base
  ( -- * Abstracting over formatters
    Formatter (..),
    IntegralFormatter (..),
    FloatingFormatter (..),
    BaseFormatter,
    formatBase,

    -- * Common formatting types
    CaseFormat (..),
    _CaseFormatLower,
    _CaseFormatTitle,
    _CaseFormatUpper,
    caseFormatToFn,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)
import Optics.Core (Prism', prism)
import Text.Printf (PrintfArg, printf)

-- | Formatter for integral types.
--
-- @since 0.1
data IntegralFormatter = MkIntegralFormatter

-- | Formatter for floating types. Takes an optional param for rounding
-- digits.
--
-- @since 0.1
newtype FloatingFormatter = MkFloatingFormatter (Maybe Word8)

-- | Maps the formatter to its format string.
--
-- @since 0.1
class Formatter a where
  formatStr :: a -> Text

-- | @since 0.1
instance Formatter IntegralFormatter where
  formatStr _ = "%d"

-- | @since 0.1
instance Formatter FloatingFormatter where
  formatStr (MkFloatingFormatter Nothing) = "%f"
  formatStr (MkFloatingFormatter (Just r)) = "%." <> T.pack (show r) <> "f"

-- | Formats a value to a string. 'BaseFormatter' is used to enforce
-- type-safety.
--
-- @since 0.1
formatBase :: (BaseFormatter a ~ f, Formatter f, PrintfArg a) => f -> a -> Text
formatBase basefmt = T.pack . printf (T.unpack $ formatStr basefmt)

-- | Relates a "base" value with its given formatter. This is used to enforce
-- type-safe formatting e.g. floating types can only be used with
-- 'FloatingFormatter'.
--
-- @since 0.1
type BaseFormatter :: Type -> Type
type family BaseFormatter a

type instance BaseFormatter Int = IntegralFormatter

type instance BaseFormatter Int8 = IntegralFormatter

type instance BaseFormatter Int16 = IntegralFormatter

type instance BaseFormatter Int32 = IntegralFormatter

type instance BaseFormatter Int64 = IntegralFormatter

type instance BaseFormatter Integer = IntegralFormatter

type instance BaseFormatter Word = IntegralFormatter

type instance BaseFormatter Word8 = IntegralFormatter

type instance BaseFormatter Word16 = IntegralFormatter

type instance BaseFormatter Word32 = IntegralFormatter

type instance BaseFormatter Word64 = IntegralFormatter

type instance BaseFormatter Natural = IntegralFormatter

type instance BaseFormatter Float = FloatingFormatter

type instance BaseFormatter Double = FloatingFormatter

-- | Case formatting.
--
-- @since 0.1
data CaseFormat
  = -- | @since 0.1
    CaseFormatLower
  | -- | @since 0.1
    CaseFormatTitle
  | -- | @since 0.1
    CaseFormatUpper
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
_CaseFormatLower :: Prism' CaseFormat ()
_CaseFormatLower = prism (const CaseFormatLower) f
  where
    f CaseFormatLower = Right ()
    f other = Left other

-- | @since 0.1
_CaseFormatTitle :: Prism' CaseFormat ()
_CaseFormatTitle = prism (const CaseFormatTitle) f
  where
    f CaseFormatTitle = Right ()
    f other = Left other

-- | @since 0.1
_CaseFormatUpper :: Prism' CaseFormat ()
_CaseFormatUpper = prism (const CaseFormatUpper) f
  where
    f CaseFormatUpper = Right ()
    f other = Left other

-- | @since 0.1
caseFormatToFn :: CaseFormat -> Text -> Text
caseFormatToFn CaseFormatLower = T.toLower
caseFormatToFn CaseFormatTitle = T.toTitle
caseFormatToFn CaseFormatUpper = T.toUpper
