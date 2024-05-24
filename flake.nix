{
  inputs = {
    "nixos-23.11".url = "github:NixOS/nixpkgs/nixos-23.11";
    "nixos-unstable".url = "github:NixOS/nixpkgs/nixos-unstable";
    "flake-utils".url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-23.11" = import inputs."nixos-23.11" { inherit system; };
          "nixos-unstable" = import inputs."nixos-unstable" { inherit system; };
        };
        pkgs = nixpkgs."nixos-23.11";
        inherit (pkgs) runCommand crane;
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;
        inherit (pkgs.haskell.lib) justStaticExecutables;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

        haskellPackages = pkgs.haskell.packages.ghc810.override {
          overrides = new: old: {
            chris-martin-org = new.callPackage ./chris-martin-org {};
          };
        };

        htmlPages = ./out;

        container = pkgs.dockerTools.buildLayeredImage {
          name = "chris-martin-org";
          contents = [ pkgs.busybox htmlPages ];
          config = {
            Cmd = [ "${pkgs.busybox}/bin/busybox" "httpd" "-f" "-h" "${htmlPages}" ];
            ExposedPorts."80/tcp" = { };
          };
        };

        deploy = pkgs.writeShellApplication {
          name = "deploy-chris-martin";
          runtimeInputs = [ crane ];
          text = ''
            cleanup() {
              rm -rf "$tmp"
            }
            trap cleanup EXIT
            tmp="$(mktemp -d)"
            nix build .#container --out-link "$tmp/container.tar.gz"
            gunzip --force "$tmp/container.tar.gz" > "$tmp/container.tar"
            crane push "$tmp/container.tar" registry.digitalocean.com/iowa/chris-martin:latest
          '';
        };

      in {
        packages = { inherit container htmlPages crane; };
        devShells.default = pkgs.mkShell {
          inputsFrom = [ haskellPackages.chris-martin-org.env ];
          packages = [
            pkgs.haskell.compiler.ghc810
            pkgs.sassc
            pkgs.cabal-install
            pkgs.zlib
            pkgs.rsync
            pkgs.openssh
          ];
        };
        devShells.deploy = pkgs.mkShell { packages = [ deploy ]; };
      }
    );
}
