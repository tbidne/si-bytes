-- | Provides high level formatters for different byte types. This module
-- contains the functions and types for typical usage. Additional
-- functionality (e.g. optics, type classes/families for defining custom
-- formatters) can be found in:
--
-- * "Data.Bytes.Formatting.Base"
-- * "Data.Bytes.Formatting.Size"
-- * "Data.Bytes.Formatting.Direction"
--
-- @since 0.1
module Data.Bytes.Formatting
  ( -- * High-level functions
    formatSized,
    formatSizedDirected,

    -- * Types

    -- ** Base
    CaseFormat (..),
    IntegralFormatter (..),
    FloatingFormatter (..),

    -- ** Size
    SizeFormat (..),
    SizedFormatter (MkSizedFormatter),
    sizedFormatterUnix,
    sizedFormatterNatural,
    sizedFormatterVerbose,

    -- ** Direction
    DirectionFormat (..),
    DirectedFormatter (MkDirectedFormatter),
    directedFormatterUnix,
    directedFormatterVerbose,

    -- ** Reexports
    Default (def),
  )
where

import Data.Bytes.Class.RawNumeric (RawNumeric (Raw, toRaw))
import Data.Bytes.Formatting.Base
  ( BaseFormatter,
    CaseFormat (CaseFormatLower, CaseFormatTitle, CaseFormatUpper),
    FloatingFormatter (MkFloatingFormatter),
    Formatter,
    IntegralFormatter (MkIntegralFormatter),
    formatBase,
  )
import Data.Bytes.Formatting.Direction
  ( DirectedFormatter (MkDirectedFormatter),
    DirectionFormat (DirectionFormatLong, DirectionFormatShort),
    directedFormatterUnix,
    directedFormatterVerbose,
    formatDirection,
  )
import Data.Bytes.Formatting.Size
  ( SizeFormat (SizeFormatLong, SizeFormatMedium, SizeFormatShort),
    SizedFormatter (MkSizedFormatter),
    formatSize,
    sizedFormatterNatural,
    sizedFormatterUnix,
    sizedFormatterVerbose,
  )
import Data.Bytes.Network.Direction (Directed)
import Data.Bytes.Size (Sized)
import Data.Default (Default (def))
import Data.Text (Text)
import Text.Printf (PrintfArg)

-- | Formats a value with 'Data.Bytes.Size.Size' units. Can be used with all byte types
-- in this package.
--
-- ==== __Examples__
--
-- >>> import Data.Default (Default (..))
-- >>> import Data.Bytes (Bytes (..), Size (..), hideSize)
-- >>> let bf1 = MkIntegralFormatter
-- >>> let b1 = MkBytes @M @Int 7
-- >>> formatSized bf1 def b1
-- "7 mb"
--
-- >>> formatSized bf1 sizedFormatterUnix b1
-- "7M"
--
-- >>> let bf2 = MkFloatingFormatter (Just 2)
-- >>> let b2 = hideSize $ MkBytes @G @Float 20.248
-- >>> formatSized bf2 sizedFormatterVerbose b2
-- "20.25 gigabytes"
--
-- @since 0.1
formatSized ::
  ( Formatter (BaseFormatter (Raw a)),
    PrintfArg (Raw a),
    RawNumeric a,
    Sized a
  ) =>
  -- | Formatter to use on the underlying value i.e. 'IntegralFormatter'
  -- or 'FloatingFormatter'.
  BaseFormatter (Raw a) ->
  -- | Formatter to use on the size units.
  SizedFormatter ->
  -- | Value to be formatted.
  a ->
  -- | Result.
  Text
formatSized basefmt sizefmt x =
  formatBase basefmt (toRaw x) <> formatSize sizefmt x

-- | Formats a value with 'Data.Bytes.Size.Size' and
-- 'Data.Bytes.Direction.Direction' units. Can only be used when /both/ units
-- are present.
--
-- ==== __Examples__
--
-- >>> import Data.Default (Default (..))
-- >>> import Data.Bytes.Network (Direction (..), NetBytes (..), Size (..))
-- >>> import Data.Bytes.Size (Sized (..))
-- >>> let bf1 = MkIntegralFormatter
-- >>> let b1 = MkNetBytesP @Up @M @Int 7
-- >>> formatSizedDirected bf1 def def b1
-- "7 mb up"
--
-- >>> formatSizedDirected bf1 sizedFormatterUnix directedFormatterUnix b1
-- "7M U"
--
-- >>> let bf2 = MkFloatingFormatter (Just 2)
-- >>> let b2 = hideSize $ MkNetBytesP @Down @G @Float 20.248
-- >>> formatSizedDirected bf2 sizedFormatterVerbose directedFormatterVerbose b2
-- "20.25 gigabytes down"
--
-- @since 0.1
formatSizedDirected ::
  ( Directed a,
    Formatter (BaseFormatter (Raw a)),
    PrintfArg (Raw a),
    RawNumeric a,
    Sized a
  ) =>
  -- | Formatter to use on the underlying value i.e. 'IntegralFormatter'
  -- or 'FloatingFormatter'.
  BaseFormatter (Raw a) ->
  -- | Formatter to use on the size units.
  SizedFormatter ->
  -- | Formatter to use on the direction units.
  DirectedFormatter ->
  -- | Value to be formatted.
  a ->
  -- | Result.
  Text
formatSizedDirected basefmt sizefmt dirfmt x =
  mconcat
    [ formatBase basefmt (toRaw x),
      formatSize sizefmt x,
      formatDirection dirfmt x
    ]
