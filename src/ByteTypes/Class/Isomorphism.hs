-- | Provides the 'Isomorphism' typeclass.
module ByteTypes.Class.Isomorphism
  ( Isomorphism (..),
    liftFrom,
    liftFrom2,
    liftFrom3,
    liftTo,
    liftTo2,
    liftTo3,
  )
where

-- | This class is intended for lifting functions between equivalent types.
--
-- Instances should define an isomorphism, i.e.,
--
-- @
-- to . from == id == from . to
-- @
class Isomorphism a b where
  to :: a -> b
  from :: b -> a

-- | Lifts a function.
liftFrom :: Isomorphism a b => (b -> b) -> a -> a
liftFrom f = from . f . to

-- | Lifts a binary function.
liftFrom2 :: Isomorphism a b => (b -> b -> b) -> a -> a -> a
liftFrom2 f x = from . f (to x) . to

-- | Lifts a ternary function.
liftFrom3 :: Isomorphism a b => (b -> b -> b -> b) -> a -> a -> a -> a
liftFrom3 f x y = from . f (to x) (to y) . to

-- | Lifts a function.
liftTo :: Isomorphism a b => (a -> a) -> b -> b
liftTo f = to . f . from

-- | Lifts a binary function.
liftTo2 :: Isomorphism a b => (a -> a -> a) -> b -> b -> b
liftTo2 f x = to . f (from x) . from

-- | Lifts a ternary function.
liftTo3 :: Isomorphism a b => (a -> a -> a -> a) -> b -> b -> b -> b
liftTo3 f x y = to . f (from x) (from y) . from
