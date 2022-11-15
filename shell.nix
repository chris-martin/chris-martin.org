let
sources = import ./nix/sources.nix;
pkgs = import sources."nixpkgs" { overlays = []; config = {}; };
inherit ((pkgs.callPackage ./. {}).haskellPackages) chris-martin-org;

in pkgs.mkShell {
    name = "chris-martin-org-shell";
    inputsFrom = [ chris-martin-org ];
    packages = [ pkgs.sassc ];
}
