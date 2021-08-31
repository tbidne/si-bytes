-- | Provides the 'Isomorphism' typeclass.
module ByteTypes.Class.Isomorphism
  ( Isomorphism (..),
    liftL,
    liftL2,
    liftL3,
    liftR,
    liftR2,
    liftR3,
  )
where

-- | This class is intended for lifting functions between equivalent types.
--
-- Instances should define an isomorphism, i.e.,
--
-- @
-- toR . toL == id == toL . toR
-- @
class Isomorphism a b where
  toR :: a -> b
  toL :: b -> a

-- | Lifts a function.
liftL :: Isomorphism a b => (b -> b) -> a -> a
liftL f = toL . f . toR

-- | Lifts a binary function.
liftL2 :: Isomorphism a b => (b -> b -> b) -> a -> a -> a
liftL2 f x = toL . f (toR x) . toR

-- | Lifts a ternary function.
liftL3 :: Isomorphism a b => (b -> b -> b -> b) -> a -> a -> a -> a
liftL3 f x y = toL . f (toR x) (toR y) . toR

-- | Lifts a function.
liftR :: Isomorphism a b => (a -> a) -> b -> b
liftR f = toR . f . toL

-- | Lifts a binary function.
liftR2 :: Isomorphism a b => (a -> a -> a) -> b -> b -> b
liftR2 f x = toR . f (toL x) . toL

-- | Lifts a ternary function.
liftR3 :: Isomorphism a b => (a -> a -> a -> a) -> b -> b -> b -> b
liftR3 f x y = toR . f (toL x) (toL y) . toL
