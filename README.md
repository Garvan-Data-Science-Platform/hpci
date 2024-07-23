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

## Testing in CI

Integration tests are run against a version of OpenPBS dockerised along with an openssh server.
The code for building the docker image (and associated scripts) are in the `ci` directory.
It is tricky to build this image on aarch64-darwin as building the docker image involves compiling OpenPBS from source.

The `.github/workflows/build-pbs.yml` workflow file builds the image in ci (only if there has been a change to code in the `ci` directory, or the `build-pbs.yml` workflow file) and pushes the image to a GCP artifact registry.
The `.github/workflows/build-test.yml` builds a fully statically linked version of `hpci`, pulls the test docker image, and runs the new `hpci` binary to connect to the OpenPBS docker container and run a basic job submission.

## Testing locally

Create a ssh key called `test_key` at the root of the `hpci` repository.

The `makefile` has convenience commands for local testing.
A summary of commands can be accessed using `make help`.

For typical development and testing on an aarch64-darwin machine run:
- ` gcloud auth login --project [GCP_PROJECT_NAME]`
- ` gcloud auth configure-docker [GCP_REGION]-docker.pkg.dev/[GCP_PROJECT_NAME]/docker`
- Make sure you have a container runtime like docker desktop or [colima](https://github.com/abiosoft/colima)
- `make PROJECT=[GCP_PROJECT_NAME] pull` to pull docker image
- `make PROJECT=[GCP_PROJECT_NAME] run` to run OpenPBS docker container.
- use `docker logs -f pbs` to watch the logs and wait until the sshd server has been restarted.
- In a seperate terminal run `ls app/*.hs | entr make test` to recompile and run tests eachtime `hpci` haskell files are saved (requires installing [entr](https://github.com/eradman/entr))
- If you are on a x86_84-linux machine, then you can run `make test-bin` to test with the compiled binary
- `make stop` to stop (and automatically remove) docker container when finished
