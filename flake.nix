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
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              hpci = hfinal.callCabal2nix "hpci" ./. { };
            };
        };
        hpci = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.hpci;
      };
      perSystem = system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;

          staticPkgs = import nixpkgs { inherit system; overlays = [ overlay static-haskell-nix.overlay ]; };
          staticHspkgs = staticPkgs.haskell.packages.ghc948;
          appendConfigureFlags = drv: flags: drv.overrideAttrs (old: {
            configureFlags = (old.configureFlags or []) ++ flags;
          });
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
            static = appendConfigureFlags staticHspkgs.hpci [
              "--ghc-option=-optl=-static"
              "--ghc-option=-optl=-pthread"
              "--extra-lib-dirs=${staticPkgs.zlib.static}/lib"
              "--extra-lib-dirs=${staticPkgs.libssh2}/lib"
              "--ghc-option=-fPIC"
              "--ghc-option=-optc=-fPIC"
              "--ghc-option=-optl=-static-libgcc"
              "--ghc-option=-optl=-static-libstdc++"
              "--ghc-option=-optl=-lm"
              "--ghc-option=-optl=-lstdc++"
            ];
            buildInputs = [
              staticPkgs.pkg-config
              staticPkgs.libssh2
              staticPkgs.zlib
            ];
          };
        };
    in
    { inherit overlay; } // flake-utils.lib.eachDefaultSystem perSystem;
}
