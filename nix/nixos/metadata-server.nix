{ config, lib, pkgs, ... }:
let
  # "metadata-server" and "metadataServer" with "[-sS]erver" will be used where possible
  # to avoid confusion with other similarly named services like `cardano-metadata-submitter`
  cfg = config.services.metadata-server;
  inherit (cfg.metadataServerPkgs) metadataServerHaskellPackages metadataServerTestingHaskellPackages iohkNix;
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
        default = import ../. {};
        defaultText = "metadata-server pkgs";
        description = ''
          The metadata-server packages and library that should be used.
        '';
        internal = true;
      };
      testing-mode = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "enable testing APIs";
      };
      package = lib.mkOption {
        type = lib.types.package;
        default = if cfg.testing-mode
          then metadataServerTestingHaskellPackages.metadata-server.components.exes.metadata-server
          else metadataServerHaskellPackages.metadata-server.components.exes.metadata-server;
      };
      user = lib.mkOption {
        type = lib.types.str;
        default = "metadata-server";
        description = "the user to run as";
      };
      port = lib.mkOption {
        type = lib.types.int;
        default = 8080;
        description = "the port the metadata server runs on";
      };
      postgres = {
        generatePGPASS = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "generate pgpass";
        };
        pgpass = lib.mkOption {
          type = lib.types.path;
          default = builtins.toFile "pgpass" "${cfg.postgres.socketdir}:${toString cfg.postgres.port}:${cfg.postgres.database}:${cfg.postgres.user}:*";
        };
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
          default = cfg.postgres.user;
          description = "the postgresql database to use";
        };
        table = lib.mkOption {
          type = lib.types.str;
          default = "metadata";
          description = "the postgresql database table to use";
        };
        user = lib.mkOption {
          type = lib.types.str;
          default = cfg.user;
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
    in pkgs.writeShellScript "metadata-server" ''
      set -euo pipefail
      RUNTIME_DIRECTORY=''${RUNTIME_DIRECTORY:-$(pwd)}
      ${lib.optionalString cfg.postgres.generatePGPASS ''
      cp ${cfg.postgres.pgpass} /$RUNTIME_DIRECTORY/pgpass
      chmod 0600 $RUNTIME_DIRECTORY/pgpass
      export PGPASSFILE=/$RUNTIME_DIRECTORY/pgpass
      ''}
      exec ${cfg.package}/bin/metadata-server \
        --db-name  ${config.services.metadata-server.postgres.database}
        --db-user  ${config.services.metadata-server.postgres.user}
        --db-host  ${config.services.metadata-server.postgres.socketdir}
        --db-table ${config.services.metadata-server.postgres.table}
        --db-conns ${toString config.services.metadata-server.postgres.numConnections}
        --port     ${toString config.services.metadata-server.port}
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
        ExecStart = config.services.metadata-server.script;
        DynamicUser = true;
        RuntimeDirectory = "metadata-server";
        StateDirectory = "metadata-server";
      };

      wantedBy = [ "multi-user.target" ];
      after = [ "postgres.service" ];
      requires = [ "postgresql.service" ];
    };
  };
}
