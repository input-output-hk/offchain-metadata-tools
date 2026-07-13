
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

      # `ensurePermissions` no longer exists and PostgreSQL 15+ needs
      # explicit schema grants. These run in postgresql-setup.service
      # (not postgresql.service) so they fire *after* ensureDatabases has
      # created the database; that service puts `psql` on PATH and sets
      # PGPORT, so a bare `psql` connects as the superuser.
      systemd.services.postgresql-setup.postStart = pkgs.lib.mkAfter ''
        psql -tAc 'GRANT ALL PRIVILEGES ON DATABASE ${database} TO "${postgresUser}";'
        psql -d ${database} -tAc 'GRANT ALL ON SCHEMA public TO "${postgresUser}";'
      '';

      users = {
        mutableUsers = false;

        users = {
          # For ease of debugging the VM as the `root` user
          root.password = "";

          # Create a system user that matches the database user so that we
          # can use peer authentication.
          "${user}" = {
            isSystemUser = true;
            group = user;
          };
        };

        groups."${user}" = {};
      };

      services.metadata-server = {
        enable = true;

        metadataServerPkgs = haskellPackages;

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
