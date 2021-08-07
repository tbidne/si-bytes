-- | Provides the 'BytesNum' typeclass.
module ByteTypes.Class.Num
  ( BytesNum (..),
  )
where

-- | Replacement for the standard 'Num' class, to be used with
-- our byte types.
class BytesNum a where
  type Scalar a
  (|+|) :: a -> a -> a
  (|-|) :: a -> a -> a

  (*|) :: Scalar a -> a -> a
  (*|) = flip (|*)

  (|*) :: a -> Scalar a -> a
  (|*) = flip (*|)

  {-# MINIMAL (|+|), (|-|), ((*|) | (|*)) #-}

infixl 6 |+|

infixl 6 |-|

infixl 7 |*

infixl 7 *|