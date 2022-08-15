module Main (main) where

import System.Environment.Guard (ExpectEnv (..), guardOrElse')
import Test.DocTest qualified as DocTest
import Prelude

main :: IO ()
main =
  guardOrElse'
    "RUN_DOCTEST"
    ExpectEnvSet
    (DocTest.doctest args)
    (putStrLn "*** Doctests Disabled ***")
  where
    args = files <> exts

files :: [String]
files =
  [ "-isrc",
    "src/Data/Bytes.hs",
    "src/Data/Bytes/Class/Conversion.hs",
    "src/Data/Bytes/Class/Normalize.hs",
    "src/Data/Bytes/Class/Parser.hs",
    "src/Data/Bytes/Class/Wrapper.hs",
    "src/Data/Bytes/Formatting.hs",
    "src/Data/Bytes/Formatting/Direction.hs",
    "src/Data/Bytes/Formatting/Size.hs",
    "src/Data/Bytes/Internal.hs",
    "src/Data/Bytes/Network.hs",
    "src/Data/Bytes/Network/Direction.hs",
    "src/Data/Bytes/Network/Internal.hs",
    "src/Data/Bytes/Size.hs"
  ]

-- This is needed because DocTest does not read the cabal
-- file's default-extensions
exts :: [String]
exts =
  [ "-XApplicativeDo",
    "-XDataKinds",
    "-XDefaultSignatures",
    "-XDeriveAnyClass",
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
    "-XOverloadedLabels",
    "-XOverloadedStrings",
    "-XPatternSynonyms",
    "-XPolyKinds",
    "-XRankNTypes",
    "-XScopedTypeVariables",
    "-XStandaloneDeriving",
    "-XStandaloneKindSignatures",
    "-XTupleSections",
    "-XTypeApplications",
    "-XTypeFamilyDependencies",
    "-XTypeOperators"
  ]
