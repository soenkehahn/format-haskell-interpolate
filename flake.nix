{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import "${nixpkgs}" {
          inherit system;
        };
        strings = pkgs.lib.strings;
        lists = pkgs.lib.lists;
        ourHaskell = pkgs.haskell.packages.ghc945;
      in
      {
        lib = pkgs.lib;
        packages.default =
          let
            src = ./.;
          in
          (ourHaskell.callCabal2nix "format-haskell-interpolate" src { });
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              ghcid
              ormolu
              cabal-install
              (ourHaskell.ghc.withPackages (p: self.packages.${system}.default.buildInputs))
              self.packages.${system}.default.buildInputs
              (haskell-language-server.override {
                dynamic = true;
                supportedGhcVersions = [ "945" ];
              })
              ourHaskell.cabal2nix
              nixpkgs-fmt
            ];
          };
        };
      }
    );
}
