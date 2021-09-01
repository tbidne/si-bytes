-- | Provides the 'Ring' typeclass.
module ByteTypes.Class.Math.Algebra.Ring
  ( Ring (..),
  )
where

import ByteTypes.Class.Math.Algebra.Group (Group)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Defines an algebraic ring.
class Group r => Ring r where
  (.*.) :: r -> r -> r
  rid :: r

infixl 7 .*.

instance Ring Double where
  (.*.) = (*)
  rid = 1

instance Ring Float where
  (.*.) = (*)
  rid = 1

instance Ring Int where
  (.*.) = (*)
  rid = 1

instance Ring Int8 where
  (.*.) = (*)
  rid = 1

instance Ring Int16 where
  (.*.) = (*)
  rid = 1

instance Ring Int32 where
  (.*.) = (*)
  rid = 1

instance Ring Int64 where
  (.*.) = (*)
  rid = 1

instance Ring Integer where
  (.*.) = (*)
  rid = 1

instance Ring Natural where
  (.*.) = (*)
  rid = 1

instance Ring Word where
  (.*.) = (*)
  rid = 1

instance Ring Word8 where
  (.*.) = (*)
  rid = 1

instance Ring Word16 where
  (.*.) = (*)
  rid = 1

instance Ring Word32 where
  (.*.) = (*)
  rid = 1

instance Ring Word64 where
  (.*.) = (*)
  rid = 1

instance Integral a => Ring (Ratio a) where
  (.*.) = (*)
  rid = 1
