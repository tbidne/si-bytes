{
  description = "byte-types flake";
  inputs.nixpkgs.url = "github:nixos/nixpkgs?rev=1ffba9f2f683063c2b14c9f4d12c55ad5f4ed887";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.algebra-simple-src.url = "github:tbidne/algebra-simple";
  outputs =
    { flake-utils
    , nixpkgs
    , self
    , algebra-simple-src
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      pkgs = import nixpkgs { inherit system; };
      compilerVersion = "ghc922";
      compiler = pkgs.haskell.packages."${compilerVersion}";
      mkPkg = returnShellEnv:
        compiler.developPackage {
          inherit returnShellEnv;
          name = "byte-types";
          root = ./.;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with compiler; [
              cabal-install
              haskell-language-server
              hlint
              ghcid
              pkgs.nixpkgs-fmt
            ]);
          overrides = final: prev: with compiler; {
            algebra-simple =
              final.callCabal2nix "algebra-simple" algebra-simple-src { };
          };
        };
    in
    {
      defaultPackage = mkPkg false;

      devShell = mkPkg true;
    });
}
