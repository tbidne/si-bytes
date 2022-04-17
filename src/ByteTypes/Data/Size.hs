{-# LANGUAGE UndecidableInstances #-}

-- | Provides the 'Size' type and typeclasses for converting
-- between units.
--
-- @since 0.1
module ByteTypes.Data.Size
  ( -- * Size Tags
    Size (..),
    SSize (..),
    SingSize (..),
    withSingSize,

    -- * Type Families for Relating Tags
    NextSize,
    PrevSize,
  )
where

import Data.Kind (Type)
import Data.Type.Equality (TestEquality (..), (:~:) (..))
import GHC.TypeLits (ErrorMessage (..), TypeError)

-- | Byte units.
--
-- @since 0.1
data Size
  = -- | Bytes
    --
    -- @since 0.1
    B
  | -- | Kilobytes
    --
    -- @since 0.1
    K
  | -- | Megabytes
    --
    -- @since 0.1
    M
  | -- | Gigabytes
    --
    -- @since 0.1
    G
  | -- | Terabytes
    --
    -- @since 0.1
    T
  | -- | Petabytes
    --
    -- @since 0.1
    P
  | -- | Exabytes
    --
    -- @since 0.1
    E
  | -- | Zettabytes
    --
    -- @since 0.1
    Z
  | -- | Yottabytes
    --
    -- @since 0.1
    Y
  deriving
    ( -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )

-- | Singleton for 'Size'.
--
-- @since 0.1
type SSize :: Size -> Type
data SSize s where
  -- | @since 0.1
  SB :: SSize 'B
  -- | @since 0.1
  SK :: SSize 'K
  -- | @since 0.1
  SM :: SSize 'M
  -- | @since 0.1
  SG :: SSize 'G
  -- | @since 0.1
  ST :: SSize 'T
  -- | @since 0.1
  SP :: SSize 'P
  -- | @since 0.1
  SE :: SSize 'E
  -- | @since 0.1
  SZ :: SSize 'Z
  -- | @since 0.1
  SY :: SSize 'Y

-- | @since 0.1
instance TestEquality SSize where
  testEquality x y = case (x, y) of
    (SB, SB) -> Just Refl
    (SK, SK) -> Just Refl
    (SM, SM) -> Just Refl
    (SG, SG) -> Just Refl
    (ST, ST) -> Just Refl
    (SP, SP) -> Just Refl
    (SE, SE) -> Just Refl
    (SZ, SZ) -> Just Refl
    (SY, SY) -> Just Refl
    _ -> Nothing

-- | @since 0.1
deriving stock instance Show (SSize s)

-- | Typeclass for recovering the 'Size' at runtime.
--
-- @since 0.1
class SingSize s where
  -- | @since 0.1
  singSize :: SSize s

-- | @since 0.1
instance SingSize 'B where singSize = SB

-- | @since 0.1
instance SingSize 'K where singSize = SK

-- | @since 0.1
instance SingSize 'M where singSize = SM

-- | @since 0.1
instance SingSize 'G where singSize = SG

-- | @since 0.1
instance SingSize 'T where singSize = ST

-- | @since 0.1
instance SingSize 'P where singSize = SP

-- | @since 0.1
instance SingSize 'E where singSize = SE

-- | @since 0.1
instance SingSize 'Z where singSize = SZ

-- | @since 0.1
instance SingSize 'Y where singSize = SY

-- | Singleton \"with\"-style convenience function. Allows us to run a
-- computation @SingSize d => r@ without explicitly pattern-matching
-- every time.
--
-- @since 0.1
withSingSize :: SSize s -> (SingSize s => r) -> r
withSingSize s x = case s of
  SB -> x
  SK -> x
  SM -> x
  SG -> x
  ST -> x
  SP -> x
  SE -> x
  SZ -> x
  SY -> x

-- | Type family that relates units to the next larger one.
--
-- ==== __Examples__
--
-- >>> :kind! NextSize 'M
-- NextSize 'M :: Size
-- = 'G
--
-- >>> :kind! NextSize 'Y
-- NextSize 'Y :: Size
-- = (TypeError ...)
--
-- @since 0.1
type NextSize :: Size -> Size
type family NextSize s = t where
  NextSize 'B = 'K
  NextSize 'K = 'M
  NextSize 'M = 'G
  NextSize 'G = 'T
  NextSize 'T = 'P
  NextSize 'P = 'E
  NextSize 'E = 'Z
  NextSize 'Z = 'Y
  NextSize 'Y = TypeError ('Text "The byte unit Y does not have a 'next size'.")

-- | Type family that relates units to the previous smaller one.
--
-- ==== __Examples__
--
-- >>> :kind! PrevSize 'M
-- PrevSize 'M :: Size
-- = 'K
--
-- >>> :kind! PrevSize 'B
-- PrevSize 'B :: Size
-- = (TypeError ...)
--
-- @since 0.1
type PrevSize :: Size -> Size
type family PrevSize s = t where
  PrevSize 'B = TypeError ('Text "The byte unit B does not have a 'previous size'.")
  PrevSize 'K = 'B
  PrevSize 'M = 'K
  PrevSize 'G = 'M
  PrevSize 'T = 'G
  PrevSize 'P = 'T
  PrevSize 'E = 'P
  PrevSize 'Z = 'E
  PrevSize 'Y = 'Z
