{
  description = "hpci";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
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
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
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
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}