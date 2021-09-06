{-# LANGUAGE RecordWildCards #-}

-- | Exports functions for verifying 'Conversion' properties.
module Props.Verify.Conversion
  ( ResultConvs (..),
    convertB,
    convertKB,
    convertMB,
    convertGB,
    convertTB,
    convertPB,
  )
where

import ByteTypes.Class.Math.Algebra.Field (Field (..))
import ByteTypes.Class.Math.Algebra.Ring (Ring (..))
import ByteTypes.Class.Math.Literal (NumLiteral (..))
import Hedgehog (PropertyT, (===))
import Hedgehog qualified as H

data ExpectedConvs n = MkExpectedConvs
  { bExp :: n -> n,
    kExp :: n -> n,
    mExp :: n -> n,
    gExp :: n -> n,
    tExp :: n -> n,
    pExp :: n -> n
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
    pRes :: n
  }

-- | Tests that the \"to bytes\" conversion matches expectations.
convertB :: (Eq n, Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertB results = do
  let bExp = (.%. fromLit 1)
      kExp = (.%. fromLit 1_000)
      mExp = (.%. fromLit 1_000_000)
      gExp = (.%. fromLit 1_000_000_000)
      tExp = (.%. fromLit 1_000_000_000_000)
      pExp = (.%. fromLit 1_000_000_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to kilobytes\" conversion matches expectations.
convertKB :: (Eq n, Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertKB results = do
  let bExp = (.*. fromLit 1_000)
      kExp = id
      mExp = (.%. fromLit 1_000)
      gExp = (.%. fromLit 1_000_000)
      tExp = (.%. fromLit 1_000_000_000)
      pExp = (.%. fromLit 1_000_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to megabytes\" conversion matches expectations.
convertMB :: (Eq n, Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertMB results = do
  let bExp = (.*. fromLit 1_000_000)
      kExp = (.*. fromLit 1_000)
      mExp = id
      gExp = (.%. fromLit 1_000)
      tExp = (.%. fromLit 1_000_000)
      pExp = (.%. fromLit 1_000_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to gigabytes\" conversion matches expectations.
convertGB :: (Eq n, Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertGB results = do
  let bExp = (.*. fromLit 1_000_000_000)
      kExp = (.*. fromLit 1_000_000)
      mExp = (.*. fromLit 1_000)
      gExp = id
      tExp = (.%. fromLit 1_000)
      pExp = (.%. fromLit 1_000_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to terabytes\" conversion matches expectations.
convertTB :: (Eq n, Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertTB results = do
  let bExp = (.*. fromLit 1_000_000_000_000)
      kExp = (.*. fromLit 1_000_000_000)
      mExp = (.*. fromLit 1_000_000)
      gExp = (.*. fromLit 1_000)
      tExp = id
      pExp = (.%. fromLit 1_000)

  convert MkExpectedConvs {..} results

-- | Tests that the \"to petabytes\" conversion matches expectations.
convertPB :: (Eq n, Field n, NumLiteral n, Show n) => ResultConvs n -> PropertyT IO ()
convertPB results = do
  let bExp = (.*. fromLit 1_000_000_000_000_000)
      kExp = (.*. fromLit 1_000_000_000_000)
      mExp = (.*. fromLit 1_000_000_000)
      gExp = (.*. fromLit 1_000_000)
      tExp = (.*. fromLit 1_000)
      pExp = id

  convert MkExpectedConvs {..} results

convert :: (Eq n, Show n) => ExpectedConvs n -> ResultConvs n -> PropertyT IO ()
convert MkExpectedConvs {..} MkResultConvs {..} = do
  convertAndTest bExp original bRes "B"
  convertAndTest kExp original kRes "KB"
  convertAndTest mExp original mRes "MB"
  convertAndTest gExp original gRes "GB"
  convertAndTest tExp original tRes "TB"
  convertAndTest pExp original pRes "PB"

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
