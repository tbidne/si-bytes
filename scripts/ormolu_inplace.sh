#!/bin/sh

find ./src -name '*.hs' | xargs ormolu \
  --ghc-opt -XImportQualifiedPost \
  --ghc-opt -XTypeApplications \
  --mode=inplace

find ./test -name '*.hs' | xargs ormolu \
  --ghc-opt -XImportQualifiedPost \
  --ghc-opt -XTypeApplications \
  --mode=inplace