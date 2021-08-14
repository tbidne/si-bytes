{ compiler ? "ghc8104"
, pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/c464dc811babfe316ed4ab7bbc12351122e69dd7.tar.gz") { }
}:

let
  haskellDeps = ps: with ps; [
    cabal-install
    cabal-fmt
    cabal-plan
    haskell-language-server
    hlint
    implicit-hie
  ];

  haskellOtherDeps = [ pkgs.haskellPackages.ormolu ];

  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages haskellDeps;

  otherDeps = [
    pkgs.nixpkgs-fmt
  ];
in
pkgs.mkShell {

  buildInputs =
    [ ghc ]
    ++ haskellOtherDeps
    ++ otherDeps;
}
