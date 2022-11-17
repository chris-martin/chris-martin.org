let
sources = import ./nix/sources.nix;
pkgs = import sources."nixpkgs" { overlays = []; config = {}; };
inherit ((pkgs.callPackage ./. {}).haskellPackages) chris-martin-org;

in pkgs.mkShell {
    name = "chris-martin-org-shell";
    inputsFrom = [ chris-martin-org.env ];
    packages = [ pkgs.sassc pkgs.cabal-install pkgs.zlib ];
    LC_ALL = "en_US.UTF-8";
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
}
