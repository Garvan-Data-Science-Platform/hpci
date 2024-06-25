{
  description = "HPCI";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
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
            pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
            hspkgs = pkgs.haskellPackages;
            staticPkgs = nixpkgs.legacyPackages.${system}.pkgsMusl;
            haskellPackages = staticPkgs.haskell.packages.ghc948;
            packageName = "hpci";
            jailbreakUnbreak = pkg: staticPkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
            inherit (staticPkgs.haskell.lib) appendConfigureFlags justStaticExecutables;
            mypackage = haskellPackages.callCabal2nix packageName self rec {
            };
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
          packages.${packageName} = staticPkgs.haskell.lib.overrideCabal mypackage (old: {
            enableSharedExecutables = false;
            enableSharedLibraries = false;
            configureFlags =     [
                  "--ghc-option=-optl=-static"
                  "--extra-lib-dirs=${staticPkgs.gmp6.override { withStatic = true; }}/lib"
                  "--extra-lib-dirs=${staticPkgs.libsshw.override { withStatic = true; }}/lib"
                  "--extra-lib-dirs=${staticPkgs.zlib.static}/lib"
                  "--extra-lib-dirs=${staticPkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
                  "--extra-lib-dirs=${staticPkgs.ncurses.override { enableStatic = true; }}/lib"
            ];
          });

          defaultPackage = self.packages.${system}.${packageName};
          devShell = pkgs.mkShell {
            buildInputs = with haskellPackages; [
              haskell-language-server
              cabal-install
              cabal2nix
              hlint
              ormolu
            ] ++ [ 
              pkgs.zlib
              pkgs.libssh2
              pkgs.pkg-config
            ];
            inputsFrom = builtins.attrValues self.packages.${system};
          };
        }
      );
}
