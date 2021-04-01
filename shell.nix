# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? {}
, sourcesOverride ? {}
, withHoogle ? true
, pkgs ? import ./nix {
    inherit config sourcesOverride;
  }
}:

with pkgs; with commonLib;
let

  sources = import ./nix/sources.nix {};

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = offchainMetadataToolsHaskellPackages.shellFor {
    name = "offchain-metadata-tools-shell";

    # If shellFor local packages selection is wrong,
    # then list all local packages then include source-repository-package that cabal complains about:
    packages = ps: lib.attrValues (haskell-nix.haskellLib.selectProjectPackages ps);

    # packags = ps: pkgs.lib.attrValues (selectProjectPackages ps);
    additional = ps: [
      ps.cardano-prelude
      ps.cardano-crypto-class
    ];


    tools = { cabal = "3.2.0.0"; };

    # These programs will be available inside the nix-shell.
    buildInputs = (with pkgs; [
      # cabal-install
      ghcid
      git
      hlint
      niv
      nix
      pkgconfig
      stylish-haskell
    ]) ++ (with haskellPackages; [ weeder ]);

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    inherit withHoogle;

    GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";
  };

in

 shell
