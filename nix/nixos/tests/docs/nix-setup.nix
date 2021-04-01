{ pkgs, ... }:
with pkgs;
let
  docScripts = pkgs.callPackage ../../../../docs/default.nix {};
in
{
  name = "nix-setup-test";

  nodes = {
    server = { config, ... }: {
      nixpkgs.pkgs = pkgs;

      nix.nixPath =
        [
          "nixpkgs=${pkgs.path}"
          "nixos-config=/etc/nixos/configuration.nix"
          "/nix/var/nix/profiles/per-user/root/channels"
        ];

      imports = [];

      environment.systemPackages = [
        docScripts
        jq
      ];

      users = {
        mutableUsers = false;

        users = {
          # For ease of debugging the VM as the `root` user
          root.password = "";
        };
      };

    } // (import "${docScripts}/metadata-config.nix" { inherit config pkgs; sources = { metadata-server = ../../../..; }; } );
  };

  testScript =
    ''
    import json
    import sys

    start_all()

    server.wait_for_unit("metadata-server.service")
    server.succeed(
        "${docScripts}/bin/nix-setup-test.sh"
    )
    '';
}
