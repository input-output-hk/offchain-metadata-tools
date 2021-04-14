# This is the derivation used by "stack --nix".
# It provides the system dependencies required for a stack build.
{ pkgs ? import ./default.nix {}
}:

let
  cabalShell = import ./cabal-shell.nix {
    inherit pkgs;
  };
in
  cabalShell.overrideAttrs (old: {
    name = "offchain-metadata-tools-stack-env";

    buildInputs = old.buildInputs ++ [ pkgs.stack ];

    # Build environment setup copied from
    # <nixpkgs/pkgs/development/haskell-modules/generic-stack-builder.nix>
    GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    STACK_IN_NIX_SHELL = "true";
  })
