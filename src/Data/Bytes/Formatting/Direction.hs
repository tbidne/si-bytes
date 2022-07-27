{-# LANGUAGE UndecidableInstances #-}

-- | Provides formatters for 'Direction'.
--
-- @since 0.1
module Data.Bytes.Formatting.Direction
  ( DirectionFormat (..),
    _DirectionFormatShort,
    _DirectionFormatLong,
    DirectedFormatter (..),
    formatDirection,
    directedFormatterUnix,
    directedFormatterVerbose,
  )
where

import Data.Bytes.Formatting.Base (CaseFormat (..), Formatter (..), caseFormatToFn)
import Data.Bytes.Network.Direction (Directed (..), Direction (..))
import Data.Default (Default (..))
import Data.Text (Text)
import Optics.Core
  ( A_Lens,
    LabelOptic (..),
    Prism',
    lens,
    prism,
    (^.),
  )
import Text.Printf (printf)

-- | Determines how to format the direction units.
--
-- ==== __Examples__
--
-- >>> def @DirectionFormat
-- DirectionFormatLong
--
-- @since 0.1
data DirectionFormat
  = -- | @since 0.1
    DirectionFormatShort
  | -- | @since 0.1
    DirectionFormatLong
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Default DirectionFormat where
  def = DirectionFormatLong

-- | @since 0.1
_DirectionFormatShort :: Prism' DirectionFormat ()
_DirectionFormatShort = prism (const DirectionFormatShort) f
  where
    f DirectionFormatShort = Right ()
    f other = Left other

-- | @since 0.1
_DirectionFormatLong :: Prism' DirectionFormat ()
_DirectionFormatLong = prism (const DirectionFormatLong) f
  where
    f DirectionFormatLong = Right ()
    f other = Left other

-- | Formats bytes over floating types.
--
-- ==== __Examples__
--
-- >>> def @DirectedFormatter
-- MkDirectedFormatter {caseFormat = CaseFormatLower, directionFormat = DirectionFormatLong}
--
-- @since 0.1
data DirectedFormatter = MkDirectedFormatter
  { caseFormat :: CaseFormat,
    directionFormat :: DirectionFormat
  }
  deriving stock
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Show
    )

-- | @since 0.1
instance Default DirectedFormatter where
  def = MkDirectedFormatter CaseFormatLower def

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ CaseFormat, b ~ CaseFormat) =>
  LabelOptic "caseFormat" k DirectedFormatter DirectedFormatter a b
  where
  labelOptic = lens caseFormat (\f x -> f {caseFormat = x})

-- | @since 0.1
instance
  (k ~ A_Lens, a ~ DirectionFormat, b ~ DirectionFormat) =>
  LabelOptic "directionFormat" k DirectedFormatter DirectedFormatter a b
  where
  labelOptic = lens directionFormat (\f x -> f {directionFormat = x})

-- | @since 0.1
instance Formatter DirectedFormatter where
  formatStr _ = " %s"

-- | "unix"-style formatter:
--
-- * Upper case.
-- * Short spelling.
--
-- For example: @7 U@.
--
-- @since 0.1
directedFormatterUnix :: DirectedFormatter
directedFormatterUnix = MkDirectedFormatter CaseFormatUpper DirectionFormatShort

-- | Verbose formatter:
--
-- * Lower case.
-- * Full spelling.
--
-- For example: @7 down@.
--
-- @since 0.1
directedFormatterVerbose :: DirectedFormatter
directedFormatterVerbose = MkDirectedFormatter CaseFormatLower DirectionFormatLong

-- | Formats a directed value with the given formatter.
--
-- @since 0.1
formatDirection ::
  ( Directed a
  ) =>
  DirectedFormatter ->
  a ->
  String
formatDirection fmt x = printf (formatStr fmt) dir'
  where
    dir = formatDirection' (fmt ^. #directionFormat) (directionOf x)
    dir' = caseFormatToFn (fmt ^. #caseFormat) dir

formatDirection' :: DirectionFormat -> Direction -> Text
formatDirection' DirectionFormatShort Up = "U"
formatDirection' DirectionFormatShort Down = "D"
formatDirection' DirectionFormatLong Up = "Up"
formatDirection' DirectionFormatLong Down = "Down"
