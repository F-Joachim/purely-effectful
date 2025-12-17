{
  description = "A flake to provide a Haskell environment using haskell.nix";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: _prev: {
          # This overlay adds the project to pkgs
          project =
            final.haskell-nix.stackProject' {
             src = {
                outPath = ./.;
                name = "purely-effectful";
              };
              
              #compiler-nix-name = "ghc9103";

              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint`, `haskell-language-server` and `stack`.
              shell.tools = {
                cabal = {};
                hlint = {};
                haskell-language-server = {};
                stack = {version = "3.7.1";};
              };

              # Non-Haskell shell tools go here
              shell.buildInputs = with final.pkgs; [
                nixpkgs-fmt
              ];
              # Adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.project.flake {
        # crossPlatforms = p: [p.ghcjs];
      };
    in flake // {
      # Built by `nix build .`
      packages.default = flake.packages."purely-effectful:exe:purely-effectful-exe";
    });
}
