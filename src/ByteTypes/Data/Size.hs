-- | Provides the 'ByteSize' type and typeclasses for converting
-- between units.
module ByteTypes.Data.Size
  ( -- * ByteSize Tags
    ByteSize (..),
    SByteSize (..),
    SingByteSize (..),

    -- * Type Families for Relating Tags
    NextUnit,
    PrevUnit,

    -- * Typeclasses for converting units
    Normalize (..),
    DecByteSize (..),
    IncByteSize (..),
    Conversion (..),

    -- * Functions
    convert,
  )
where

import ByteTypes.Class.Math.Algebra.Field (Field (..))
import Data.Kind (Type)

-- | Byte units.
data ByteSize
  = B
  | KB
  | MB
  | GB
  | TB
  | PB
  deriving (Eq, Ord, Show)

-- | Singleton for 'ByteSize'.
type SByteSize :: ByteSize -> Type
data SByteSize s where
  SB :: SByteSize 'B
  SKB :: SByteSize 'KB
  SMB :: SByteSize 'MB
  SGB :: SByteSize 'GB
  STB :: SByteSize 'TB
  SPB :: SByteSize 'PB

deriving instance Show (SByteSize s)

-- | Typeclass for recovering the 'ByteSize' at runtime.
class SingByteSize s where
  singByteSize :: SByteSize s

instance SingByteSize 'B where singByteSize = SB

instance SingByteSize 'KB where singByteSize = SKB

instance SingByteSize 'MB where singByteSize = SMB

instance SingByteSize 'GB where singByteSize = SGB

instance SingByteSize 'TB where singByteSize = STB

instance SingByteSize 'PB where singByteSize = SPB

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

-- | Used for normalizing bytes @b@ such that
--
-- \[
--    1 \le \text{normalize}(b) < 1000 \iff 1\text{ B} \le b < 1000\text{ PB}.
-- \]
--
-- If we view @normalize@ as single function taking in a type and a value,
-- then the only law is idempotence:
--
-- \[
--   \textrm{normalize} \circ \textrm{normalize} = \textrm{normalize}.
-- \]
--
-- Restricting @normalize@ to a single type (i.e. one instance) also yields
-- injectivity:
--
-- \[
--   \textrm{normalize}(x) = \textrm{normalize}(y) \implies x = y.
-- \]
--
-- N.B. Notice that \(\textrm{normalize} : \mathcal{C} \rightarrow \mathcal{C}\) does __not__ define a homomorpism.
-- That is, the law
--
-- \[
--   \textrm{normalize}(f(x, y)) = f(\textrm{normalize}(x), \textrm{normalize}(y))
-- \]
--
-- cannot possibly hold. For one, it is possible the result of @f@ will not be
-- normalized regardless of its arguments. Even if we had such @f@, being
-- homomorphic would require following the /field/ distributive law,
--
-- \[ k (x + y) = k x + k y, \]
--
-- which most 'Num' instances violate. For instance, even 'Integer' violates
-- this when accounting for division, as the action induced by division is
-- not free. In other words, it violates
--
-- \[
--   \forall n \in \mathbb{Z},\;\; d_1, d_2, \in \mathbb{Z}^\times,\quad \frac{n}{d_1} = \frac{n}{d_2} \implies d_1 = d_2.
-- \]
--
-- The moral of this story is that normalization should typically be the
-- /last/ operation performed, most likely used for display purposes.
-- There is usually no need to normalize multiple times.
class Normalize a where
  type Norm a
  normalize :: a -> Norm a

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
  type Converted (b :: ByteSize) a
  toB :: a -> Converted 'B a
  toKB :: a -> Converted 'KB a
  toMB :: a -> Converted 'MB a
  toGB :: a -> Converted 'GB a
  toTB :: a -> Converted 'TB a
  toPB :: a -> Converted 'PB a

-- | Low level function for converting a numeric literal between
-- byte sizes. @convert b1 b2@ converts /from/ @b1@ /to/ @b2@,
-- e.g. @convert GB KB = \\n -> n * 1_000_000@. The higher level
-- byte types and functions should be preferred
-- (e.g. 'ByteTypes.Data.Bytes', 'Normalize'), but this is here
-- when it is needed.
convert :: (Num n, Field n) => ByteSize -> ByteSize -> n -> n
convert B B n = n
convert B KB n = n .%. 1_000
convert B MB n = n .%. 1_000_000
convert B GB n = n .%. 1_000_000_000
convert B TB n = n .%. 1_000_000_000_000
convert B PB n = n .%. 1_000_000_000_000_000
convert KB B n = n * 1_000
convert KB KB n = n
convert KB MB n = n .%. 1_000
convert KB GB n = n .%. 1_000_000
convert KB TB n = n .%. 1_000_000_000
convert KB PB n = n .%. 1_000_000_000_000
convert MB B n = n * 1_000_000
convert MB KB n = n * 1_000
convert MB MB n = n
convert MB GB n = n .%. 1_000
convert MB TB n = n .%. 1_000_000
convert MB PB n = n .%. 1_000_000_000
convert GB B n = n * 1_000_000_000
convert GB KB n = n * 1_000_000
convert GB MB n = n * 1_000
convert GB GB n = n
convert GB TB n = n .%. 1_000
convert GB PB n = n .%. 1_000_000
convert TB B n = n * 1_000_000_000_000
convert TB KB n = n * 1_000_000_000
convert TB MB n = n * 1_000_000
convert TB GB n = n * 1_000
convert TB TB n = n
convert TB PB n = n .%. 1_000
convert PB B n = n * 1_000_000_000_000_000
convert PB KB n = n * 1_000_000_000_000
convert PB MB n = n * 1_000_000_000
convert PB GB n = n * 1_000_000
convert PB TB n = n * 1_000
convert PB PB n = n
