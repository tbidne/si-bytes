-- | Provides the 'Module' typeclass.
module ByteTypes.Class.Math.Algebra.Module
  ( Module (..),
  )
where

import ByteTypes.Class.Math.Algebra.Group (Group)
import ByteTypes.Class.Math.Algebra.Ring (Ring)

-- | Defines an algebraic module over a ring.
class (Group m, Ring r) => Module m r where
  (.*) :: m -> r -> m
  (.*) = flip (*.)

  (*.) :: r -> m -> m
  (*.) = flip (.*)

  {-# MINIMAL ((.*) | (*.)) #-}

infixl 7 .*

infixl 7 *.
