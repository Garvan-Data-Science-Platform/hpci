{
  description = "hpci";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  };

  outputs = { self, nixpkgs }:
    let
      pkgsForSystem = system: if system == "x86_64-linux" then
        nixpkgs.legacyPackages.${system}.pkgsMusl
      else
        nixpkgs.legacyPackages.${system};
    in
    {
      packages =
        let
          packageForSystem = system:
            let
              pkgs = pkgsForSystem system;
              static_ssl = (pkgs.openssl.override { static = true; });
              haskellPackages = pkgs.haskell.packages.ghc948;
              packageName = "hpci";
              jailbreakUnbreak = pkg: pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
              inherit (pkgs.haskell.lib) appendConfigureFlags justStaticExecutables;
              mypackage = haskellPackages.callCabal2nix packageName self rec {
              };
            in
            pkgs.haskell.lib.overrideCabal mypackage (old: {
              enableSharedExecutables = false;
              enableSharedLibraries = false;
              configureFlags = [
                "--ghc-option=-optl=-static"
                "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
                "--ghc-option=-optl=-lz"
                "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
                "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
                "--extra-lib-dirs=${pkgs.libssh2.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
                "--extra-lib-dirs=${pkgs.pkg-config}/lib"
                "--extra-lib-dirs=${static_ssl}/lib"
                "--extra-lib-dirs=${pkgs.zlib.static}/lib"
              ];
              buildDepends = [
                pkgs.libffi
                pkgs.libssh2
                pkgs.pkg-config
                static_ssl
                pkgs.zlib.static
              ];
            });
        in
        {
          x86_64-linux = { hpci = packageForSystem "x86_64-linux"; };
          aarch64-darwin = { hpci = packageForSystem "aarch64-darwin"; };
        };

      devShells =
        let
          devShellForSystem = system:
            let pkgs = pkgsForSystem system;
                haskellPackages = pkgs.haskell.packages.ghc948;
            in pkgs.mkShell {
              buildInputs = with haskellPackages; [
                haskell-language-server
                cabal-install
                cabal2nix
              ] ++ [ pkgs.zlib ];
              inputsFrom = builtins.attrValues self.packages.${system};
            };
        in
        {
          x86_64-linux = devShellForSystem "x86_64-linux";
          aarch64-darwin = devShellForSystem "aarch64-darwin";
        };

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.hpci;
      defaultPackage.aarch64-darwin = self.packages.aarch64-darwin.hpci;
    };
}
