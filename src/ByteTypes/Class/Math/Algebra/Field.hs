-- | Provides the 'Field' typeclass.
module ByteTypes.Class.Math.Algebra.Field
  ( Field (..),
  )
where

import ByteTypes.Class.Math.Algebra.Ring (Ring (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Defines an algebraic field.
class Ring f => Field f where
  finv :: f -> f
  finv x = rid .%. x

  (.%.) :: f -> f -> f
  x .%. y = x .*. finv y

  {-# MINIMAL (finv | (.%.)) #-}

infixl 7 .%.

instance Field Double where (.%.) = (/)

instance Field Float where (.%.) = (/)

instance Field Int where (.%.) = div

instance Field Int8 where (.%.) = div

instance Field Int16 where (.%.) = div

instance Field Int32 where (.%.) = div

instance Field Int64 where (.%.) = div

instance Field Integer where (.%.) = div

instance Field Natural where (.%.) = div

instance Field Word where (.%.) = div

instance Field Word8 where (.%.) = div

instance Field Word16 where (.%.) = div

instance Field Word32 where (.%.) = div

instance Field Word64 where (.%.) = div

instance Integral a => Field (Ratio a) where (.%.) = (/)
