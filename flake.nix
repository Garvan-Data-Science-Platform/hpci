{
  description = "hpci";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";
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
              ssl_static = pkgs.openssl.override { static = true; };
              ssh2_static = pkgs.libssh2.overrideAttrs (old: { dontDisableStatic = true; });
              numa_static = pkgs.numactl.overrideAttrs (old: { dontDisableStatic = true; });
              zstd_static = pkgs.zstd.override { static = true; };
              xz_static = pkgs.xz.override { enableStatic = true; };
              bz2_static = pkgs.bzip2.override { enableStatic = true; };
              haskellPackages = pkgs.haskell.packages.ghc98;
              packageName = "hpci";
              jailbreakUnbreak = pkg: pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
              inherit (pkgs.haskell.lib) appendConfigureFlags justStaticExecutables;
              mypackage = haskellPackages.callPackage ./hpci.nix {
              };
            in
            pkgs.haskell.lib.overrideCabal mypackage (old: {
              enableSharedExecutables = false;
              enableSharedLibraries = false;
              doCheck = false;
              configureFlags = [
                "--ghc-option=-optl=-static"

                "--ghc-option=-optl=-L${ssh2_static}/lib"
                "--ghc-option=-optl=-lssh2"
                "--ghc-option=-optl=-L${ssl_static}/lib"
                "--ghc-option=-optl=-lssl"
                "--ghc-option=-optl=-lcrypto"

                "--ghc-option=-optl=-L${pkgs.elfutils.dev}/lib"
                "--ghc-option=-optl=-ldw"
                "--ghc-option=-optl=-lelf"

                "--ghc-option=-optl=-L${numa_static}/lib"
                "--ghc-option=-optl=-lnuma"
                "--ghc-option=-optl=-L${pkgs.zstd.dev}/lib"
                "--ghc-option=-optl=-lzstd"
                "--ghc-option=-optl=-L${pkgs.xz.dev}/lib"
                "--ghc-option=-optl=-llzma"
                "--ghc-option=-optl=-L${pkgs.bzip2.out}/lib"
                "--ghc-option=-optl=-lbz2"
                "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
                "--ghc-option=-optl=-lz"

                "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
                "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
                "--extra-lib-dirs=${ssh2_static}/lib"
                "--extra-lib-dirs=${pkgs.pkg-config}/lib"
                "--extra-lib-dirs=${ssl_static}/lib"
                "--extra-lib-dirs=${pkgs.zlib.static}/lib"
                "--extra-lib-dirs=${numa_static}/lib"
                "--extra-lib-dirs=${pkgs.elfutils.dev}/lib"
                "--extra-lib-dirs=${zstd_static}/lib"
                "--extra-lib-dirs=${xz_static}/lib"
                "--extra-lib-dirs=${bz2_static}/lib"
              ];
              buildDepends = [
                pkgs.libffi
                pkgs.pkg-config
                ssh2_static
                ssl_static
                pkgs.zlib.static
                numa_static
                pkgs.elfutils
                zstd_static
                xz_static
                bz2_static
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
            let pkgs = nixpkgs.legacyPackages.${system};
                haskellPackages = pkgs.haskell.packages.ghc98;
            in pkgs.mkShell {
              buildInputs = with haskellPackages; [
                cabal-install
                cabal2nix
                ghc
              ] ++ [
                pkgs.zlib
                pkgs.entr
                pkgs.haskell-language-server
                pkgs.cachix
                pkgs.wget
                pkgs.openssl
                pkgs.libssh2
                pkgs.google-cloud-sdk
                pkgs.pkg-config
                pkgs.docker
                pkgs.docker-compose
              ];
            };
        in
        {
          x86_64-linux =   { default = devShellForSystem "x86_64-linux";};
          aarch64-darwin = { default = devShellForSystem "aarch64-darwin";};
        };

      defaultPackage.x86_64-linux = self.packages.x86_64-linux.hpci;
      defaultPackage.aarch64-darwin = self.packages.aarch64-darwin.hpci;
    };
}
