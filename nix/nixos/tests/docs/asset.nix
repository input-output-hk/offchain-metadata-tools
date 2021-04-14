{ pkgs, sources, ... }:
with pkgs;
let
  cardano-cli = (import sources.cardano-node {}).cardano-cli;
  docScripts = import ../default.nix { inherit pkgs; };
in
{
  name = "documentation-test";

  nodes = {
    server = { config, ... }: {
      nixpkgs.pkgs = pkgs;
      imports = [
        ../.
        (sources.cardano-node + "/nix/nixos")
      ];

      users = {
        mutableUsers = false;

        users = {
          # For ease of debugging the VM as the `root` user
          root.password = "";
        };
      };

      environment.systemPackages = [
        cardano-cli
      ];

    };
  };

  testScript =
    ''
    import json
    import sys

    start_all()

    server.succeed(
        "${docScripts}/out.sh"
    )
    '';
}
