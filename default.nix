{ sources ? import ./nix/sources.nix
, pkgs ? (import sources."nixpkgs" { overlays = []; config = {}; })
, lib ? pkgs.lib
, callPackage ? pkgs.callPackage
, haskell ? pkgs.haskell
, ghcVersion ? "ghc8104"
, symlinkJoin ? pkgs.symlinkJoin
}:

let
    haskellPackageSources = {
        chris-martin-org = ./chris-martin-org;
    };

    haskellPackages = haskell.packages.${ghcVersion}.override (old: {
        overrides = lib.fold lib.composeExtensions (old.overrides or (_: _: { }))
          [
              (self: _: {
                  rss = dontCheck (self.callPackage ./hackage/rss/3000.2.0.7.nix {});
              })
              (haskell.lib.packageSourceOverrides haskellPackageSources)
          ];
    });

    inherit (haskell.lib) dontCheck;

in

symlinkJoin {
    name = "root-chris-martin-org";
    paths = [ haskellPackages.chris-martin-org ];
} // {
    inherit haskellPackages haskellPackageSources;
}
