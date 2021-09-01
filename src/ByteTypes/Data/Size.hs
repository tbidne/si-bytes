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
  )
where

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
