{
  description = "hpci";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
    flake-utils.url = "github:numtide/flake-utils";
    static-haskell-nix = {
      url = "github:nh2/static-haskell-nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, static-haskell-nix }:
    let
      compiler = "ghc948";
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              hpci = hfinal.callCabal2nix "hpci" ./. { };
            };
        };
        hpci = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.hpci;
      };
      staticOverlay = final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${compiler} = prev.haskell.packages.${compiler}.override {
              overrides = final: prev: {
                hpci = final.callCabal2nix "hpci" ./. { };
              };
            };
          };
        };
      };
      perSystem = system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ overlay staticOverlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShells = rec {
            default = hpci-shell;
            hpci-shell = hspkgs.shellFor {
              withHoogle = true;
              packages = p: [ p.hpci ];
              buildInputs = [
                hspkgs.cabal-install
                hspkgs.haskell-language-server
                hspkgs.hlint
                hspkgs.ormolu
                pkgs.bashInteractive
                pkgs.pkg-config
                pkgs.libssh2
              ];
            };
          };
          packages = rec {
            default = hpci;
            hpci = pkgs.hpci;
            static = static-haskell-nix.survey.haskellPackages.${compiler}.hpci;
          };
        };
    in
    { inherit overlay; } // flake-utils.lib.eachDefaultSystem perSystem;
}
