{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
       inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
      ];
      perSystem = { self', lib, config, pkgs, ... }: {
        haskellProjects.default = {
          packages = {
            # You can add more than one local package here.
            aoc.root = ./.; # Assumes ./my-package.cabal
          };

            buildTools = hp: {
            treefmt = config.treefmt.build.wrapper;
          } // config.treefmt.build.programs;
          # overrides = self: super: {}
          hlintCheck.enable = true;
          hlsCheck.enable = true;
        };
             # source tree was auto formatted.
        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;

          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };
     # source tree was auto formatted.
        # treefmt.config = {
        #   inherit (config.flake-root) projectRootFile;
        #   package = pkgs.treefmt;

        #   programs.ormolu.enable = true;
        #   programs.nixpkgs-fmt.enable = true;
        #   programs.cabal-fmt.enable = true;

        #   # We use fourmolu
        #   programs.ormolu.package = pkgs.haskellPackages.fourmolu;
        #   settings.formatter.ormolu = {
        #     options = [
        #       "--ghc-opt"
        #       "-XImportQualifiedPost"
        #     ];
        #   };
        # };        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.aoc;
      };
    };
}
