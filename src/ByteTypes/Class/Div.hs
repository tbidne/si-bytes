{-# LANGUAGE UndecidableInstances #-}

-- | Provides the Div typeclass for abstracting division..
module ByteTypes.Class.Div
  ( Div (..),
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Ratio (Ratio)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Natural (Natural)

-- | Abstracts over division, allowing us to use whichever operation makes
-- sense for different types.
class Div a where
  (%) :: a -> a -> a

infixl 7 %

instance Div Double where (%) = (/)

instance Div Float where (%) = (/)

instance Div Int where (%) = div

instance Div Int8 where (%) = div

instance Div Int16 where (%) = div

instance Div Int32 where (%) = div

instance Div Int64 where (%) = div

instance Div Integer where (%) = div

instance Div Natural where (%) = div

instance Div Word where (%) = div

instance Div Word8 where (%) = div

instance Div Word16 where (%) = div

instance Div Word32 where (%) = div

instance Div Word64 where (%) = div

instance Integral a => Div (Ratio a) where (%) = (/)
