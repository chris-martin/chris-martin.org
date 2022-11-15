{ sources ? import ../nix/sources.nix
, pkgs ? import sources."nixpkgs" { overlays = []; config = {}; }
, haskellPackages ? pkgs.haskellPackages
}:
haskellPackages.developPackage {
  root = ./.;
}
