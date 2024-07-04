# hpci
[![hpci on hackage](https://img.shields.io/hackage/v/hpci)](http://hackage.haskell.org/package/hpci)
[![hpci on Stackage Nightly](https://stackage.org/package/hpci/badge/nightly)](https://stackage.org/nightly/package/hpci)

Generated with [template-haskell](https://github.com/jonascarpay/template-haskell)

## Development environment

### If you use `nix`:

This project uses [nix](https://nixos.org/) and [nix flakes](https://nixos.wiki/wiki/flakes) to provide a consistent software environment for development and compilation.
To get started with `nix` I recommend using the [Determinante Systems nix installer](https://github.com/DeterminateSystems/nix-installer), and the [Zero to Nix](https://zero-to-nix.com/) guide to learning Nix and flakes.
There is also a `.envrc` file (requires installing [direnv](https://direnv.net/) and [nix-direnv](https://github.com/nix-community/nix-direnv)) to automatically start a nix flake devShell when you `cd` into the directory containing the code from this repo.
The flake uses `cabal2nix` to generate nix build instructions from the Cabal file, therefore new haskell dependencies can be added via cabal, with no need to adjust the `flake.nix`.

The `flake.nix` file is configured to work for both x86_64-linux and aarch-darwin architecture + operating systems, however the set up is slightly different between them:
  - the x86_64-linux development environment uses a version of GHC that has been compiled against `musl` rather than `glibc`.
    - This allows generation of a statically linked Haskell executable (see [this blog post for more details](https://cs-syd.eu/posts/2024-04-20-static-linking-haskell-nix)).
    - I am not currently using linux for my day-to-day development on this project, rather I am using this exclusively for generating portable executables.
  
  - the aarch64-darwin development environment does not use musl-linked packages.

If you do not set up `direnv` (see links above), activate a nix development environment using `nix develop .#devShells.x86_64-linux` or `nix develop .#devShells.aarch64-darwin` depending on your architecture + operating system.

To build a statically linked executable on x86_64-linux use `nix build .#packages.x86_64-linux.hpci`.

To build a dynamically linked executable on aarch64-darwin use `cabal build` inside the flake devShell.

### If you do not use `nix`:

You do not need nix to run the code, all you need is the Glascow Haskell Compiler (GHC) and Cabal package manager.
These can been downloaded using [GHCup](https://www.haskell.org/ghcup/).

To compile and run the program, use `cabal run exes --` followed by the following required arguments:
  - username   - *username on remote system*
  - hostname   - *IP address or domain name or remote host*
  - port       - *port declaration - typically 22*
  - cmd        - *command to execute on remote system - typically `qsub` on PBS hpci*
  - publickey  - *local filepath for ssh public key*
  - privatekey - *local filepath for ssh private key*
  - script     - *local filepath of `.pbs` script to accomany the `qsub` command*
  - logFile    - *remote filepath of logfile produced by `.pbs` script to copy back to local system*
