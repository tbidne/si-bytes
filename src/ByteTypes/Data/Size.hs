-- | Provides the 'ByteSize' type and typeclasses for converting
-- between units.
module ByteTypes.Data.Size
  ( -- * ByteSize Tags
    ByteSize (..),

    -- * Typeclasses for converting units
    Normalize (..),
    DecByteSize (..),
    IncByteSize (..),
    Conversion (..),

    -- * Type Families for Relating Tags
    NextUnit,
    PrevUnit,

    -- * Functions
    convert,
  )
where

-- | Byte units.
data ByteSize
  = B
  | KB
  | MB
  | GB
  | TB
  | PB
  deriving (Show)

-- | Closed type family that relates units to the next larger one.
type NextUnit :: ByteSize -> ByteSize
type family NextUnit a where
  NextUnit 'B = 'KB
  NextUnit 'KB = 'MB
  NextUnit 'MB = 'GB
  NextUnit 'GB = 'TB
  NextUnit 'TB = 'PB
  NextUnit 'PB = 'PB

-- | Closed type family that relates units to the previous smaller one.
type PrevUnit :: ByteSize -> ByteSize
type family PrevUnit a where
  PrevUnit 'B = 'B
  PrevUnit 'KB = 'B
  PrevUnit 'MB = 'KB
  PrevUnit 'GB = 'MB
  PrevUnit 'TB = 'GB
  PrevUnit 'PB = 'TB

-- | Used for normalizing bytes @b@ such that,
--
-- \[
--    1 \le \text{normalize}(b) < 1000 \iff 1\text{ B} \le b < 1000\text{ PB}
-- \]
class Normalize a where
  type Result a
  normalize :: a -> Result a

-- | Typeclass for decreasing bytes to the previous units.
class DecByteSize a where
  -- | Prev should involve 'PrevUnit'.
  type Prev a

  prev :: a -> Prev a

-- | Typeclass for increasing bytes to the next units.
class IncByteSize a where
  -- | Next should involve 'NextUnit'.
  type Next a

  next :: a -> Next a

-- | Provides a common interface for converting between byte sizes.
class Conversion a where
  type BType a
  type KType a
  type MType a
  type GType a
  type TType a
  type PType a
  toB :: a -> BType a
  toKB :: a -> KType a
  toMB :: a -> MType a
  toGB :: a -> GType a
  toTB :: a -> TType a
  toPB :: a -> PType a

-- | Low level function for converting a numeric literal between
-- byte sizes. @convert b1 b2@ converts /from/ @b1@ /to/ @b2@,
-- e.g. @convert GB KB = \n -> n * 1_000_000@. The higher level
-- byte types and functions should be preferred
-- (e.g. 'ByteTypes.Data.Bytes', 'Normalize'), but this is here
-- when it is needed.
convert :: Fractional n => ByteSize -> ByteSize -> n -> n
convert B B n = n
convert B KB n = n / 1_000
convert B MB n = n / 1_000_000
convert B GB n = n / 1_000_000_000
convert B TB n = n / 1_000_000_000_000
convert B PB n = n / 1_000_000_000_000_000
convert KB B n = n * 1_000
convert KB KB n = n
convert KB MB n = n / 1_000
convert KB GB n = n / 1_000_000
convert KB TB n = n / 1_000_000_000
convert KB PB n = n / 1_000_000_000_000
convert MB B n = n * 1_000_000
convert MB KB n = n * 1_000
convert MB MB n = n
convert MB GB n = n / 1_000
convert MB TB n = n / 1_000_000
convert MB PB n = n / 1_000_000_000
convert GB B n = n * 1_000_000_000
convert GB KB n = n * 1_000_000
convert GB MB n = n * 1_000
convert GB GB n = n
convert GB TB n = n / 1_000
convert GB PB n = n / 1_000_000
convert TB B n = n * 1_000_000_000_000
convert TB KB n = n * 1_000_000_000
convert TB MB n = n * 1_000_000
convert TB GB n = n * 1_000
convert TB TB n = n
convert TB PB n = n / 1_000
convert PB B n = n * 1_000_000_000_000_000
convert PB KB n = n * 1_000_000_000_000
convert PB MB n = n * 1_000_000_000
convert PB GB n = n * 1_000_000
convert PB TB n = n * 1_000
convert PB PB n = n
