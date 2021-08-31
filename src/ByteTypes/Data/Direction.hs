-- | Provides the 'ByteDirection' type and singletons.
module ByteTypes.Data.Direction
  ( -- * ByteDirection Tags
    ByteDirection (..),
    SByteDirection (..),
    SingByteDirection (..),
  )
where

import Data.Kind (Type)

-- | Tags for differentiating downloaded vs. uploaded bytes.
data ByteDirection
  = Down
  | Up
  deriving (Show)

-- | Singleton for 'ByteDirection'.
type SByteDirection :: ByteDirection -> Type
data SByteDirection d where
  SDown :: SByteDirection 'Down
  SUp :: SByteDirection 'Up

deriving instance Show (SByteDirection d)

-- | Typeclass for recovering the 'ByteDirection' at runtime.
class SingByteDirection d where
  singByteDirection :: SByteDirection d

instance SingByteDirection 'Down where singByteDirection = SDown

instance SingByteDirection 'Up where singByteDirection = SUp
