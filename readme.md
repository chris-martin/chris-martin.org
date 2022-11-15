Run commands from a Nix shell.

Build:

    cabal run site

This places the built site contents into the `out` directory.

Then deploy:

    bash ./deploy.bash
