{
  description = "A flake to provide a Haskell environment using haskell.nix";

  inputs.haskellNix.url = "github:F-Joachim/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskellNix,
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            # This overlay adds the project to pkgs
            project = final.haskell-nix.stackProject' {
              src = {
                outPath = ./.;
                name = "purely-effectful";
              };

              #compiler-nix-name = "ghc9103";

              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint`, `haskell-language-server` and `stack`.
              shell.tools = {
                cabal = { };
                hlint = { };
                haskell-language-server = { };
                stack = {
                  version = "3.9.1";
                };
              };

              modules = [
                {
                  packages.directory.flags.os-string = true;
                  packages.unix.flags.os-string = true;
                  packages.process.flags.os-string = true;
                }
              ];

              # Non-Haskell shell tools go here
              shell.buildInputs = with final.pkgs; [
                nixpkgs-fmt
                zlib
              ];
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.project.flake {
          # crossPlatforms = p: [p.ghcjs];
        };
      in
      flake
      // {
        # Built by `nix build .`
        packages.default = flake.packages."purely-effectful:exe:purely-effectful-exe";
      }
    );
  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = "true";
  };
}
