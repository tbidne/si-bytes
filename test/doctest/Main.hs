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
    "src/Data/Bytes.hs",
    "src/Data/Bytes/Class/Conversion.hs",
    "src/Data/Bytes/Class/Normalize.hs",
    "src/Data/Bytes/Internal.hs",
    "src/Data/Bytes/Network.hs",
    "src/Data/Bytes/Network/NetBytes/Internal.hs",
    "src/Data/Bytes/Network/SomeNetDir/Internal.hs",
    "src/Data/Bytes/Size.hs"
  ]

-- This is needed because DocTest does not read the cabal
-- file's default-extensions
exts :: [String]
exts =
  [ "-XDataKinds",
    "-XDefaultSignatures",
    "-XDeriveDataTypeable",
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
