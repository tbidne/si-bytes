-- | Utilities for property tests.
module ByteTypes.Utils
  ( -- * Logical operators
    (==>),
    (<=>),
  )
where

-- | Logical implication.
(==>) :: Bool -> Bool -> Bool
True ==> False = False
_ ==> _ = True

infixr 1 ==>

-- | Logical equivalence.
(<=>) :: Bool -> Bool -> Bool
True <=> True = True
False <=> False = True
_ <=> _ = False

infixr 1 <=>
