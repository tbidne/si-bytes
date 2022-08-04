{-# LANGUAGE RecordWildCards #-}

-- | Exports functions for verifying 'Conversion' properties.
module Props.Verify.Conversion
  ( ResultConvs (..),
    convertB,
    convertK,
    convertM,
    convertG,
    convertT,
    convertP,
    convertE,
    convertZ,
    convertY,
  )
where

import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H
import Numeric.Algebra (Field, MGroup (..), MSemigroup (..))
import Numeric.Algebra qualified as Algebra
import Numeric.Data.NonZero (NonZero)
import Numeric.Literal.Integer (FromInteger (..))

data ExpectedConvs n = MkExpectedConvs
  { bExp :: n -> n,
    kExp :: n -> n,
    mExp :: n -> n,
    gExp :: n -> n,
    tExp :: n -> n,
    pExp :: n -> n,
    eExp :: n -> n,
    zExp :: n -> n,
    yExp :: n -> n
  }

-- | Record that holds an \"original value\", along with the result of
-- various conversions.
data ResultConvs n = MkResultConvs
  { original :: n,
    bRes :: n,
    kRes :: n,
    mRes :: n,
    gRes :: n,
    tRes :: n,
    pRes :: n,
    eRes :: n,
    zRes :: n,
    yRes :: n
  }

-- | Tests that the \"to bytes\" conversion matches expectations.
convertB :: (Eq n, Field n, FromInteger n, Show n) => ResultConvs n -> PropertyT IO ()
convertB results = do
  let bExp = (.%. nzafromInteger 1)
      kExp = (.%. nzafromInteger 1_000)
      mExp = (.%. nzafromInteger 1_000_000)
      gExp = (.%. nzafromInteger 1_000_000_000)
      tExp = (.%. nzafromInteger 1_000_000_000_000)
      pExp = (.%. nzafromInteger 1_000_000_000_000_000)
      eExp = (.%. nzafromInteger 1_000_000_000_000_000_000)
      zExp = (.%. nzafromInteger 1_000_000_000_000_000_000_000)
      yExp = (.%. nzafromInteger 1_000_000_000_000_000_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to kilobytes\" conversion matches expectations.
convertK :: (Eq n, Field n, FromInteger n, Show n) => ResultConvs n -> PropertyT IO ()
convertK results = do
  let bExp = (.*. afromInteger 1_000)
      kExp = id
      mExp = (.%. nzafromInteger 1_000)
      gExp = (.%. nzafromInteger 1_000_000)
      tExp = (.%. nzafromInteger 1_000_000_000)
      pExp = (.%. nzafromInteger 1_000_000_000_000)
      eExp = (.%. nzafromInteger 1_000_000_000_000_000)
      zExp = (.%. nzafromInteger 1_000_000_000_000_000_000)
      yExp = (.%. nzafromInteger 1_000_000_000_000_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to megabytes\" conversion matches expectations.
convertM :: (Eq n, Field n, FromInteger n, Show n) => ResultConvs n -> PropertyT IO ()
convertM results = do
  let bExp = (.*. afromInteger 1_000_000)
      kExp = (.*. afromInteger 1_000)
      mExp = id
      gExp = (.%. nzafromInteger 1_000)
      tExp = (.%. nzafromInteger 1_000_000)
      pExp = (.%. nzafromInteger 1_000_000_000)
      eExp = (.%. nzafromInteger 1_000_000_000_000)
      zExp = (.%. nzafromInteger 1_000_000_000_000_000)
      yExp = (.%. nzafromInteger 1_000_000_000_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to gigabytes\" conversion matches expectations.
convertG :: (Eq n, Field n, FromInteger n, Show n) => ResultConvs n -> PropertyT IO ()
convertG results = do
  let bExp = (.*. afromInteger 1_000_000_000)
      kExp = (.*. afromInteger 1_000_000)
      mExp = (.*. afromInteger 1_000)
      gExp = id
      tExp = (.%. nzafromInteger 1_000)
      pExp = (.%. nzafromInteger 1_000_000)
      eExp = (.%. nzafromInteger 1_000_000_000)
      zExp = (.%. nzafromInteger 1_000_000_000_000)
      yExp = (.%. nzafromInteger 1_000_000_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to terabytes\" conversion matches expectations.
convertT :: (Eq n, Field n, FromInteger n, Show n) => ResultConvs n -> PropertyT IO ()
convertT results = do
  let bExp = (.*. afromInteger 1_000_000_000_000)
      kExp = (.*. afromInteger 1_000_000_000)
      mExp = (.*. afromInteger 1_000_000)
      gExp = (.*. afromInteger 1_000)
      tExp = id
      pExp = (.%. nzafromInteger 1_000)
      eExp = (.%. nzafromInteger 1_000_000)
      zExp = (.%. nzafromInteger 1_000_000_000)
      yExp = (.%. nzafromInteger 1_000_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to petabytes\" conversion matches expectations.
convertP :: (Eq n, Field n, FromInteger n, Show n) => ResultConvs n -> PropertyT IO ()
convertP results = do
  let bExp = (.*. afromInteger 1_000_000_000_000_000)
      kExp = (.*. afromInteger 1_000_000_000_000)
      mExp = (.*. afromInteger 1_000_000_000)
      gExp = (.*. afromInteger 1_000_000)
      tExp = (.*. afromInteger 1_000)
      pExp = id
      eExp = (.%. nzafromInteger 1_000)
      zExp = (.%. nzafromInteger 1_000_000)
      yExp = (.%. nzafromInteger 1_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to exabytes\" conversion matches expectations.
convertE :: (Eq n, Field n, FromInteger n, Show n) => ResultConvs n -> PropertyT IO ()
convertE results = do
  let bExp = (.*. afromInteger 1_000_000_000_000_000_000)
      kExp = (.*. afromInteger 1_000_000_000_000_000)
      mExp = (.*. afromInteger 1_000_000_000_000)
      gExp = (.*. afromInteger 1_000_000_000)
      tExp = (.*. afromInteger 1_000_000)
      pExp = (.*. afromInteger 1_000)
      eExp = id
      zExp = (.%. nzafromInteger 1_000)
      yExp = (.%. nzafromInteger 1_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to zettabytes\" conversion matches expectations.
convertZ :: (Eq n, Field n, FromInteger n, Show n) => ResultConvs n -> PropertyT IO ()
convertZ results = do
  let bExp = (.*. afromInteger 1_000_000_000_000_000_000_000)
      kExp = (.*. afromInteger 1_000_000_000_000_000_000)
      mExp = (.*. afromInteger 1_000_000_000_000_000)
      gExp = (.*. afromInteger 1_000_000_000_000)
      tExp = (.*. afromInteger 1_000_000_000)
      pExp = (.*. afromInteger 1_000_000)
      eExp = (.*. afromInteger 1_000)
      zExp = id
      yExp = (.%. nzafromInteger 1_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to yottabytes\" conversion matches expectations.
convertY :: (Eq n, Field n, FromInteger n, Show n) => ResultConvs n -> PropertyT IO ()
convertY results = do
  let bExp = (.*. afromInteger 1_000_000_000_000_000_000_000_000)
      kExp = (.*. afromInteger 1_000_000_000_000_000_000_000)
      mExp = (.*. afromInteger 1_000_000_000_000_000_000)
      gExp = (.*. afromInteger 1_000_000_000_000_000)
      tExp = (.*. afromInteger 1_000_000_000_000)
      pExp = (.*. afromInteger 1_000_000_000)
      eExp = (.*. afromInteger 1_000_000)
      zExp = (.*. afromInteger 1_000)
      yExp = id

  convert MkExpectedConvs {..} results

convert :: (Eq n, Show n) => ExpectedConvs n -> ResultConvs n -> PropertyT IO ()
convert MkExpectedConvs {..} MkResultConvs {..} = do
  convertAndTest bExp original bRes "B"
  convertAndTest kExp original kRes "K"
  convertAndTest mExp original mRes "M"
  convertAndTest gExp original gRes "G"
  convertAndTest tExp original tRes "T"
  convertAndTest pExp original pRes "P"
  convertAndTest eExp original eRes "E"
  convertAndTest zExp original zRes "Z"
  convertAndTest yExp original yRes "Y"

convertAndTest ::
  (Eq t, Show t) =>
  (s -> t) ->
  s ->
  t ->
  String ->
  PropertyT IO ()
convertAndTest convFn original result label = do
  let expected = convFn original
  H.footnote label
  expected === result

nzafromInteger :: (Eq n, Field n, FromInteger n) => Integer -> NonZero n
nzafromInteger = Algebra.unsafeAMonoidNonZero . afromInteger
