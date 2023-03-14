{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Provides helpers for specs
module Unit.Specs.Verify.Normalize
  ( ExpectedNorms (..),
    normSpecs,
  )
where

import Data.Bytes.Class.Normalize (Normalize (..))
#if MIN_VERSION_base(4, 16, 0)
#endif
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@=?))

data ExpectedNorms = MkExpectedNorms
  { expectedNormDown :: String,
    expectedNormNeutral :: String,
    expectedNormUp :: String
  }

-- | Specs for normalization.
--
-- @since 0.1
normSpecs ::
  forall a.
  ( Normalize a,
    Show (Norm a)
  ) =>
  -- | Unit, used in description.
  Char ->
  -- | Constructor.
  (Float -> a) ->
  ExpectedNorms ->
  TestTree
normSpecs unit cons MkExpectedNorms {..} = testCase [unit] $ do
  expectedNormDown @=? show (normalize (cons 0.250))
  expectedNormNeutral @=? show (normalize (cons 750))
  expectedNormUp @=? show (normalize (cons 1_500))
