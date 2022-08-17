{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities for property tests.
module Unit.Utils
  ( -- * Logical operators
    (==>),
    (<=>),

    -- * Tasty
    testPropertyCompat,
  )
where

import Hedgehog (Property, PropertyName)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog qualified as TastyH

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

testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat = TastyH.testPropertyNamed
#else
testPropertyCompat tn _ = TastyH.testProperty tn
#endif
