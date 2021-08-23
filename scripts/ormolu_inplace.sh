#!/bin/sh

find ./src -name '*.hs' | xargs ormolu --ghc-opt -XImportQualifiedPost --mode=inplace
find ./test -name '*.hs' | xargs ormolu --ghc-opt -XImportQualifiedPost --mode=inplace