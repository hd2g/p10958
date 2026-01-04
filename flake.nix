{
  description = "TODO: Edit description message";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskell.packages.ghc910;
      in
        rec {
          devShells = {
            default = pkgs.mkShellNoCC {
              buildInputs = with pkgs; [
                hpkgs.ghc
                hpkgs.cabal-install
                hpkgs.hoogle
              ];
            };
          };
        }
    );
}
