
{ pkgs, haskellPackages, ... }:
with pkgs;
let
  # Single source of truth for all tutorial constants
  database      = "postgres";
  table         = "metadata";
  user          = "metadata-server";
  postgresUser  = "metadata_server";
  postgresPort  = 5432;
in
{
  name = "metadata-sync-integration-test";

  nodes = {
    server = { config, ... }: {
      nixpkgs.pkgs = pkgs;
      imports = [
        ../.
      ];

      # Open the default port for `postgrest` in the firewall
      networking.firewall.allowedTCPPorts = [];

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

        postgres = {
          port     = postgresPort;
          table    = table;
          user     = postgresUser;
          database = database;
        };
      };
    };
  };

  testScript =
    ''
    start_all()

    server.wait_for_open_port(${toString postgresPort})

    server.succeed(
        "${haskellPackages.metadata-sync.components.tests.integration-tests}/bin/integration-tests \
        --db-user ${postgresUser} \
        --db-host /run/postgresql \
        --db-name ${database} \
        --db-table ${table}"
    )
    '';
}
