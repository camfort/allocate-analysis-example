{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, config, ... }: {

        haskellProjects.ghc96 = import ./haskell-flake-ghc96.nix pkgs;

        haskellProjects.default = {
          #basePackages = config.haskellProjects.ghc96.outputs.finalPackages;

          #source-overrides = {
          #  singletons = "3.0.2"; # required for GHC 9.4,9.6 (Nix default 3.0.1)
          #  fgl = "5.8.1.1"; # Nix default is broken, missing Functor instance
          #};

          source-overrides = {
            fortran-src = "0.14.0";
          };

          devShell = {
            tools = hp: {
              ghcid = null; # broken on GHC 9.6? old fsnotify
              hlint = null; # broken on GHC 9.6? old
              haskell-language-server = null; # TAKES AGES TO BUILD FFS
            };
          };

        };

        packages.default = self'.packages.fortran-src-balanced-allocs;
      };
    };
}
