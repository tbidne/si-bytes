module Main (main) where

import System.Environment qualified as Env
import Test.DocTest qualified as DocTest
import Prelude

main :: IO ()
main = do
  shouldRun <- Env.lookupEnv "RUN_DOCTEST"
  case shouldRun of
    Just "true" -> DocTest.doctest args
    _ -> putStrLn "*** Doctests Disabled ***"
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/ByteTypes/Bytes.hs",
    "src/ByteTypes/Class/Conversion.hs",
    "src/ByteTypes/Data/Bytes/Internal.hs",
    "src/ByteTypes/Data/Network/NetBytes/Internal.hs",
    "src/ByteTypes/Data/Network/SomeNetDir/Internal.hs",
    "src/ByteTypes/Network.hs"
  ]

-- This is needed because DocTest does not read the cabal
-- file's default-extensions
exts :: [String]
exts =
  [ "-XDataKinds",
    "-XDefaultSignatures",
    "-XDeriveFunctor",
    "-XDeriveGeneric",
    "-XDerivingStrategies",
    "-XDerivingVia",
    "-XFlexibleContexts",
    "-XFlexibleInstances",
    "-XFunctionalDependencies",
    "-XGADTs",
    "-XImportQualifiedPost",
    "-XInstanceSigs",
    "-XKindSignatures",
    "-XLambdaCase",
    "-XMultiParamTypeClasses",
    "-XMultiWayIf",
    "-XNamedFieldPuns",
    "-XNumericUnderscores",
    "-XOverloadedStrings",
    "-XPatternSynonyms",
    "-XPolyKinds",
    "-XRankNTypes",
    "-XScopedTypeVariables",
    "-XStandaloneDeriving",
    "-XStandaloneKindSignatures",
    "-XTypeApplications",
    "-XTypeFamilyDependencies",
    "-XTypeOperators"
  ]
