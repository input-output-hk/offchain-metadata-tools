{ pkgs, haskellPackages, ... }:
with pkgs;
let
  # Single source of truth for all constants
  table  = "metadata";
  user   = "metadata-server";
  folder = "/tmp/metadata-store-file-integration-test";
in
{
  name = "metadata-store-file-integration-test";

  nodes = {
    server = { config, ... }: {
      nixpkgs.pkgs = pkgs;
      imports = [
        ../.
      ];

      # Open the default port for `postgrest` in the firewall
      networking.firewall.allowedTCPPorts = [];

      users = {
        mutableUsers = false;

        users = {
          # For ease of debugging the VM as the `root` user
          root.password = "";

          # Create a system user that matches the database user so that we
          # can use peer authentication.
          "${user}".isSystemUser = true;
        };
      };

      services.metadata-server = {
        enable = true;

        user = user;

        fileStore.folder = folder;
      };
    };
  };

  testScript =
    ''
    import json
    import sys

    start_all()

    server.succeed("mkdir -p ${folder}")
    server.succeed(
        "${haskellPackages.metadata-store-file.components.tests.integration-tests}/bin/integration-tests \
        --folder ${folder}"
    )
    '';
}
