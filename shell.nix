# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ metadataPackages ? import ./default.nix { inherit system crossSystem config sourcesOverride; }
, system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, pkgs ? metadataPackages.pkgs
, profiling ? false  # enable profiling in haskell dependencies
, sourcesOverride ? {}  # see sourcesOverride in nix/default.nix
, withHoogle ? false
}:

let
  inherit (pkgs) lib;
  inherit (pkgs.haskell-nix.haskellLib) selectProjectPackages;

  mkShell = name: project: project.shellFor rec {
    inherit name;
    packages = ps: lib.attrValues (selectProjectPackages ps);
    nativeBuildInputs = (with pkgs; [
      cabalWrapped
      haskellPackages.ghcid
      git
      hlint
      niv
      nix
      pkgconfig
      stylish-haskell
    ]);

    tools = { cabal = "3.4.0.0"; };

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    inherit withHoogle;

    GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";

    meta.platforms = lib.platforms.unix;
  };

in
  if profiling
    then mkShell "metadata-shell-profiled" metadataPackages.profiledProject
    else mkShell "metadata-shell" metadataPackages.project
