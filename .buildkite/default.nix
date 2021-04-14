{ pkgs ? import ../nix/default.nix {} }:

with pkgs.lib;
with pkgs;

let
  buildTools = [
    gnused gnugrep coreutils git nix gnumake gnutar gzip lz4
    stack haskellPackages.weeder
  ];
  libs = ps: with ps; [turtle safe transformers extra async];
  ghc' = haskellPackages.ghcWithPackages libs;

  stackRebuild = runCommand "stack-rebuild" {
    buildInputs = [ ghc' makeWrapper ];
  } ''
    mkdir -p $out/bin
    ghc -Wall -threaded -o $out/bin/rebuild ${./rebuild.hs}
    wrapProgram $out/bin/rebuild \
      --set PATH "${lib.makeBinPath buildTools}"
  '';

in
  stackRebuild
