{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem = { self', lib, config, pkgs, ... }: {
    haskellProjects.default = {
      # To avoid unnecessary rebuilds, we filter projectRoot:
      # https://community.flake.parts/haskell-flake/local#rebuild
      projectRoot = builtins.toString (lib.fileset.toSource {
        inherit root;
        fileset = lib.fileset.unions [
          (root + /src)
          (root + /test)
          (root + /bidir-type-checker.cabal)
          # (root + /LICENSE)
          (root + /CHANGELOG.md)
          (root + /README.md)
        ];
      });

      # The base package set (this value is the default)
      # basePackages = pkgs.haskellPackages;

      # Packages to add on top of `basePackages`
      packages = {
        # Add source or Hackage overrides here
        # (Local packages are added automatically)
        /*
        aeson.source = "1.5.0.0" # Hackage version
        shower.source = inputs.shower; # Flake input
        */
      };

      # Add your package overrides here
      settings = {
        lambda-untyped = {
          stan = true;
          haddock = true;
        };
        /*
        aeson = {
          check = false;
        };
        */
      };

      # What should haskell-flake add to flake outputs?
      autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
    };

    packages.default = self'.packages.lambda-untyped;
    apps.default = self'.apps.lambda-untyped;
  };
}
