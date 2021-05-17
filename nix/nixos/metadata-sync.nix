{ config, lib, pkgs, ... }:
let
  cfg = config.services.metadata-sync;
in {

  options = {
    services.metadata-sync = {
      enable = lib.mkEnableOption "enable the metadata sync script";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      offchainMetadataToolsPkgs = lib.mkOption {
        type = lib.types.attrs;
        default = (import ../../. {}).project;
        defaultText = "offchain-metadata-tools pkgs";
        description = ''
          The offchain-metadata-tools package set.
        '';
        internal = true;
      };
      package = lib.mkOption {
        type = lib.types.package;
        default = cfg.offchainMetadataToolsPkgs.metadata-sync.components.exes.metadata-sync;
      };
      user = lib.mkOption {
        type = lib.types.str;
        # Linux OS user can have a hyphen (`-`) along with systemd service names as standard
        default = "metadata-sync";
        description = "the user to run as";
      };
      git = {
        repositoryUrl = lib.mkOption {
          type = lib.types.str;
          description = "URL of the git repository of the metadata registry.";
        };

        metadataFolder = lib.mkOption {
          type = lib.types.str;
          default = "/";
          description = "Folder within the git repository that contains the metadata entries.";
        };
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
    services.metadata-sync.script = let
      exec = "metadata-sync";
      cmd = builtins.filter (x: x != "") [
          "${cfg.package}/bin/${exec}"
          "--db ${config.services.metadata-sync.postgres.database}"
          "--db-user ${config.services.metadata-sync.postgres.user}"
          "--db-host ${config.services.metadata-sync.postgres.socketdir}"
          "--db-table ${config.services.metadata-sync.postgres.table}"
          "--db-conns ${toString config.services.metadata-sync.postgres.numConnections}"
          "--git-url ${config.services.metadata-sync.git.repositoryUrl}"
          "--git-metadata-folder ${config.services.metadata-sync.git.metadataFolder}"
      ];
    in pkgs.writeShellScript "metadata-sync" ''
      set -euo pipefail
      echo "Starting ${exec}: ${lib.concatStringsSep "\"\n   echo \"" cmd}"
      echo "..or, once again, in a single line:"
      echo "${toString cmd}"
      exec ${toString cmd}
    '';
    environment.systemPackages = [ cfg.package config.services.postgresql.package ];
    systemd.services.metadata-sync = {
      path = [ cfg.package pkgs.netcat pkgs.postgresql pkgs.git ];
      preStart = ''
        for x in {1..60}; do
          nc -z localhost ${toString config.services.metadata-sync.postgres.port} && break
          echo loop $x: waiting for postgresql 2 sec...
          sleep 2
        done
        sleep 1
      '';
      serviceConfig = {
        ExecStart = config.services.metadata-sync.script;
        DynamicUser = true;
        RuntimeDirectory = "metadata-sync";
        StateDirectory = "metadata-sync";
      };

      wantedBy = [ "multi-user.target" ];
      after = [ "postgres.service" ];
      requires = [ "postgresql.service" ];
    };

    systemd.timers.run-metadata-sync = {
      timerConfig = {
        Unit = "metadata-sync.service";
        OnCalendar = "hourly";
      };
      wantedBy = [ "timers.target" ];
    };
  };
}
