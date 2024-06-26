{
  description = "hpci";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}.pkgsMusl;
            haskellPackages = pkgs.haskell.packages.ghc948;
            packageName = "hpci";
            jailbreakUnbreak = pkg: pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
            inherit (pkgs.haskell.lib) appendConfigureFlags justStaticExecutables;
            mypackage = haskellPackages.callCabal2nix packageName self rec {
            };
        in
        {
          packages.${packageName} = pkgs.haskell.lib.overrideCabal mypackage (old: {
            enableSharedExecutables = false;
            enableSharedLibraries = false;
            configureFlags =     [
                  "--ghc-option=-optl=-static"
                  "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
                  "--extra-lib-dirs=${pkgs.libssh2}/lib"
                  "--extra-lib-dirs=${pkgs.pkg-config}/lib"
                  "--extra-lib-dirs=${pkgs.zlib.static}/lib"
                  "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
            ];
          });

          defaultPackage = self.packages.${system}.${packageName};
          devShell = pkgs.mkShell {
            buildInputs = with haskellPackages; [
              haskell-language-server
              cabal-install
              cabal2nix
            ] ++ [ pkgs.zlib ];
            inputsFrom = builtins.attrValues self.packages.${system};
          };
        }
      );
}
