{ pkgs
, haskellPackages ? (import ../../../default.nix { }).project.hsPkgs
, supportedSystems ? [ "x86_64-linux" ]
}:

with pkgs.lib;

let
  forAllSystems = genAttrs supportedSystems;
  importTest = fn: args: system: let
    imported = import fn;
    test = import (pkgs.path + "/nixos/tests/make-test-python.nix") imported;
  in test ({
    inherit pkgs system;
    inherit (pkgs) config;
  } // args);
  callTest = fn: args: forAllSystems (system: let test = importTest fn args system; in hydraJob test // { inherit test; });
in rec {
  metadataStorePostgres = callTest ./metadata-store-postgres.nix { inherit haskellPackages; };
  metadataSync = callTest ./metadata-sync.nix { inherit haskellPackages; };
  # Test will require local faucet setup
  # asset                 = callTest ./docs/asset.nix {};
  # The "without nix" tutorial (and hence its test) is hardwired to
  # PostgreSQL 11 configuration internals and needs a rewrite:
  # noNixSetup            = callTest ./docs/no-nix-setup.nix { inherit haskellPackages; };
  nixSetup              = callTest ./docs/nix-setup.nix {};
}
