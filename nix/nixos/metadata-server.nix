{ config, lib, pkgs, ... }:
let
  # "metadata-server" and "metadataServer" with "[-sS]erver" will be used where possible
  # to avoid confusion with other similarly named services like `token-metadata-creator`
  cfg = config.services.metadata-server;
in {

  options = {
    services.metadata-server = {
      enable = lib.mkEnableOption "enable the metadata server";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      metadataServerPkgs = lib.mkOption {
        type = lib.types.attrs;
        default = (import ../../. {}).project.hsPkgs;
        defaultText = "metadata-server pkgs";
        description = ''
          The metadata-server packages and library that should be used.
        '';
        internal = true;
      };
      package = lib.mkOption {
        type = lib.types.package;
        default = cfg.metadataServerPkgs.metadata-server.components.exes.metadata-server;
      };
      user = lib.mkOption {
        type = lib.types.str;
        # Linux OS user can have a hyphen (`-`) along with systemd service names as standard
        default = "metadata-server";
        description = "the user to run as";
      };
      port = lib.mkOption {
        type = lib.types.int;
        default = 8080;
        description = "the port the metadata server runs on";
      };
      environmentFile = lib.mkOption {
        type = with lib.types; nullOr str;
        default = null;
        description = ''
          Optional path to a systemd EnvironmentFile. Not needed for the
          default local socket setup, which authenticates as the peer user.

          For a remote database, set PGPASSWORD here rather than passing
          --db-pass, which would put the password on the command line where
          any local user can read it out of ps. libpq picks PGPASSWORD up
          from the environment, and PGPASSFILE works the same way.

          The file is read by systemd at runtime and its contents never enter
          the Nix store. Point it at a runtime secret path, e.g. one decrypted
          by sops-nix or agenix.
        '';
      };
      postgres = {
        socketdir = lib.mkOption {
          type = lib.types.str;
          default = "/run/postgresql";
          description = "the path to the postgresql socket";
        };
        port = lib.mkOption {
          type = lib.types.int;
          default = 5432;
          description = "the postgresql port";
        };
        database = lib.mkOption {
          type = lib.types.str;
          # Postgresql cannot have a hyphen (`-`)
          default = "metadata_server";
          description = "the postgresql database to use";
        };
        table = lib.mkOption {
          type = lib.types.str;
          default = "metadata";
          description = "the postgresql database table to use";
        };
        user = lib.mkOption {
          type = lib.types.str;
          # Postgresql cannot have a hyphen (`-`)
          default = "metadata_user";
          description = "the postgresql user to use";
        };
        numConnections = lib.mkOption {
          type = lib.types.int;
          default = 1;
          description = "the number of connections to open to the postgresql database";
        };
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services.metadata-server.script = let
      exec = "metadata-server";
      cmd = builtins.filter (x: x != "") [
          "${cfg.package}/bin/${exec}"
          "--db ${config.services.metadata-server.postgres.database}"
          "--db-user ${config.services.metadata-server.postgres.user}"
          "--db-host ${config.services.metadata-server.postgres.socketdir}"
          "--db-table ${config.services.metadata-server.postgres.table}"
          "--db-conns ${toString config.services.metadata-server.postgres.numConnections}"
          "--port ${toString config.services.metadata-server.port}"
      ];
    in pkgs.writeShellScript "metadata-server" ''
      set -euo pipefail
      echo "Starting ${exec}: ${lib.concatStringsSep "\"\n   echo \"" cmd}"
      echo "..or, once again, in a single line:"
      echo "${toString cmd}"
      exec ${toString cmd}
    '';
    environment.systemPackages = [ cfg.package config.services.postgresql.package ];
    systemd.services.metadata-server = {
      path = [ cfg.package pkgs.netcat pkgs.postgresql ];
      preStart = ''
        for x in {1..60}; do
          nc -z localhost ${toString config.services.metadata-server.postgres.port} && break
          echo loop $x: waiting for postgresql 2 sec...
          sleep 2
        done
        sleep 1
      '';
      serviceConfig = {
        EnvironmentFile = lib.mkIf (cfg.environmentFile != null) cfg.environmentFile;
        ExecStart = config.services.metadata-server.script;
        DynamicUser = true;
        User = config.services.metadata-server.user;
        RuntimeDirectory = "metadata-server";
        StateDirectory = "metadata-server";
      };

      wantedBy = [ "multi-user.target" ];
      # postgresql-setup.service (when services.postgresql is used) creates
      # the database/user and grants schema privileges; start after it so we
      # don't try to create tables before those grants land. After= on a unit
      # that doesn't exist (external DB) is a harmless no-op.
      after = [ "postgresql-setup.service" ];
      requires = [ "postgresql.service" ];
    };
  };
}
