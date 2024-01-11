{-# LANGUAGE CPP #-}

-- | Exports generators for parsing
module Unit.Props.Generators.Parsing
  ( genIntBytesText,
    genFloatBytesText,
    genIntSizedBytesText,
    genFloatSizedBytesText,
    genIntDirectedBytesText,
    genFloatDirectedBytesText,
    genIntSizedDirectedBytesText,
    genFloatSizedDirectedBytesText,
  )
where

import Data.Bytes.Size (Size)
import Data.Char qualified as Ch
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TLBuilder
import Hedgehog (Gen)
import Hedgehog.Gen qualified as HG
import Hedgehog.Range qualified as HR

-- | Generates integral 'Text' with no size or direction.
--
-- @since 0.1
genIntBytesText :: Gen Text
genIntBytesText = genIntDigits

-- | Generates floating 'Text' with no size or direction.
--
-- @since 0.1
genFloatBytesText :: Gen Text
genFloatBytesText = genFloatDigits

-- | Generates integral 'Text' with size.
--
-- @since 0.1
genIntSizedBytesText :: Gen Text
genIntSizedBytesText = genSizedBytesText' genIntDigits

-- | Generates floating 'Text' with size.
--
-- @since 0.1
genFloatSizedBytesText :: Gen Text
genFloatSizedBytesText = genSizedBytesText' genFloatDigits

genSizedBytesText' :: Gen Text -> Gen Text
genSizedBytesText' genDigits =
  (\a b c d -> a <> b <> c <> d)
    <$> genDigits
    <*> genWhitespace
    <*> genSize
    <*> genWhitespace

-- | Generates integral 'Text' with direction.
--
-- @since 0.1
genIntDirectedBytesText :: Gen Text
genIntDirectedBytesText = genDirectedBytesText' genIntDigits

-- | Generates floating 'Text' with direction.
--
-- @since 0.1
genFloatDirectedBytesText :: Gen Text
genFloatDirectedBytesText = genDirectedBytesText' genFloatDigits

genDirectedBytesText' :: Gen Text -> Gen Text
genDirectedBytesText' genDigits =
  (\a b c d -> a <> b <> c <> d)
    <$> genDigits
    <*> genWhitespace
    <*> genDirection
    <*> genWhitespace

-- | Generates integral 'Text' corresponding with size and direction.
--
-- @since 0.1
genIntSizedDirectedBytesText :: Gen Text
genIntSizedDirectedBytesText = genSizedDirectedBytesText' genIntDigits

-- | Generates floating 'Text' corresponding with size and direction.
--
-- @since 0.1
genFloatSizedDirectedBytesText :: Gen Text
genFloatSizedDirectedBytesText = genSizedDirectedBytesText' genFloatDigits

genSizedDirectedBytesText' :: Gen Text -> Gen Text
genSizedDirectedBytesText' genDigits =
  (\a b c d e f -> a <> b <> c <> d <> e <> f)
    <$> genDigits
    <*> genWhitespace
    <*> genSize
    <*> genWhitespace1
    <*> genDirection
    <*> genWhitespace

genDirection :: Gen Text
genDirection = HG.element allDirections >>= textCase

genSize :: Gen Text
genSize = HG.element allSizes >>= textCase

genWhitespace :: Gen Text
genWhitespace = HG.text (HR.linear 0 5) (pure ' ')

genWhitespace1 :: Gen Text
genWhitespace1 = HG.text (HR.linear 1 5) (pure ' ')

genIntDigits :: Gen Text
genIntDigits = HG.text (HR.exponential 1 25) HG.digit

genFloatDigits :: Gen Text
genFloatDigits = do
  c <- HG.text (HR.exponential 1 25) HG.digit
  rest <- HG.text (HR.exponential 0 10) HG.digit
  pure $ case rest of
    "" -> c
    decimals -> c <> T.cons '.' decimals

textCase :: Text -> Gen Text
textCase =
  fmap (TL.toStrict . TLBuilder.toLazyText)
    . tfoldr f (pure "")
  where
    f :: Char -> Gen Builder -> Gen Builder
    f c gacc =
      charCase c >>= \c' ->
        (TLBuilder.singleton c' <>) <$> gacc

charCase :: Char -> Gen Char
charCase c = caseTransform <*> pure c

caseTransform :: Gen (Char -> Char)
caseTransform =
  HG.element
    [ Ch.toUpper,
      Ch.toLower,
      id
    ]

allDirections :: [Text]
allDirections = ["up", "down", "u", "d"]

allSizes :: [Text]
allSizes =
  (T.pack . show <$> [minBound @Size .. maxBound])
    ++ [ "bytes",
         "kilobytes",
         "megabytes",
         "gigabytes",
         "terabytes",
         "petabytes",
         "exabytes",
         "zettabytes",
         "yottabytes"
       ]

-- TODO: switch to unconditional foldr' once the oldest stack snapshot we
-- support has it (text 2.0.1)
tfoldr :: (Char -> a -> a) -> a -> Text -> a
#if MIN_VERSION_text(2,0,1)
tfoldr = T.foldr'
#else
tfoldr = T.foldr
#endif
