{ pkgs, ... }:
with pkgs;
let
  # Single source of truth for all tutorial constants
  database      = "postgres";
  schema        = "api";
  table         = "metadata";
  user          = "metadata-server";
  postgresUser  = "metadata_server";
  password      = "mysecretpassword";
  webRole       = "web_anon";
  postgresPort  = 5432;
  metadataServerPort = 8080;
  metadataWorkingDir = "/run/metadata-server";

  totalMem = 1024;
  allowedMem = 100;

  vegetaCommands = pkgs.callPackage ./mem-usage/vegeta-attack.nix { port = metadataServerPort; };
  vegetaGo = pkgs.writeShellScriptBin "vegeta-go" ''
    cat ${vegetaCommands} | vegeta attack -duration 10s -connections 1 -rate 100/s -output vegeta.log
  '';
in
{
  name = "metadata-server-mem-usage-test";

  nodes = {
    server = { config, ... }: {
      nixpkgs.pkgs = pkgs;
      imports = [
        ../.
      ];

      virtualisation.memorySize = totalMem;

      environment.systemPackages = [ pkgs.vegeta pkgs.htop vegetaGo pkgs.haskellPackages.hp2pretty ];

      services.postgresql = {
        enable = true;
        port = postgresPort;
        package = pkgs.postgresql;
        ensureDatabases = [ "${database}" ];
        ensureUsers = [
          {
            name = "${postgresUser}";
            ensurePermissions = {
              "DATABASE ${database}" = "ALL PRIVILEGES";
            };
          }
        ];
        identMap = ''
          metadata-server-users root ${postgresUser}
          metadata-server-users ${user} ${postgresUser}
          metadata-server-users postgres postgres
        '';
        authentication = ''
          local all all ident map=metadata-server-users
        '';

        settings =
          {
            log_connections = true;
            log_statement = "all";
            logging_collector = true;
            log_disconnections = true;
            log_destination = lib.mkForce "syslog";
          };
      };

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
        port = metadataServerPort;

       postgres = {
          port     = postgresPort;
          table    = table;
          user     = postgresUser;
          database = database;
        };

        extraFlags = ["+RTS" "-h" "-RTS"];

        metadataServerPkgs = (import ../../../. {}).profiledProject;
      };
      systemd.services.metadata-server.serviceConfig.WorkingDirectory = metadataWorkingDir;

      systemd.tmpfiles.rules = [
        "L /root/vegeta.atk - - - - ${vegetaCommands}"
      ];
    };
  };

  testScript =
    ''
    # fmt: off
    import os
    import pathlib

    start_all()

    server.wait_for_open_port(${toString postgresPort})
    server.wait_for_open_port(${toString metadataServerPort})
    server.succeed("cat ${vegetaCommands} | vegeta attack -duration 10s -connections 1 -rate 100/s")
    server.succeed("cd ${metadataWorkingDir} && hp2pretty metadata-server.hp")
    server.copy_from_vm("${metadataWorkingDir}/metadata-server.svg")

    def write_hydra_build_products(data):
      out_dir = pathlib.Path(os.environ.get("out", os.getcwd()))
      hydra_build_products = out_dir / "nix-support" / "hydra-build-products"
      hydra_build_products.parent.mkdir(exist_ok=True, parents=True)
      with hydra_build_products.open(mode="a") as f:
        f.write(data.format(out_dir) + "\n")

    write_hydra_build_products("file svg {}/metadata-server.svg")
    '';
}
