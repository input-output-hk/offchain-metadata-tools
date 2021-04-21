{ pkgs
, haskellPackages ? (import ../../../default.nix {}).project
, supportedSystems ? [ "x86_64-linux" ]
}:

with pkgs;
with pkgs.commonLib;

 let
  forAllSystems = genAttrs supportedSystems;
  importTest = fn: args: system: let
    imported = import fn;
    test = import (pkgs.path + "/nixos/tests/make-test-python.nix") imported;
  in test ({
    inherit pkgs system config;
  } // args);
  callTest = fn: args: forAllSystems (system: let test = importTest fn args system; in hydraJob test // { inherit test; });
in rec {
  metadataStorePostgres = callTest ./metadata-store-postgres.nix { inherit haskellPackages; };
  metadataSync = callTest ./metadata-sync.nix { inherit haskellPackages; };
  memUsage = callTest ./mem-usage.nix {};
  # Test will require local faucet setup
  # asset                 = callTest ./docs/asset.nix { inherit (pkgs.commonLib) sources; };
  noNixSetup            = callTest ./docs/no-nix-setup.nix { inherit haskellPackages; };
  nixSetup              = callTest ./docs/nix-setup.nix {};
}
