{ config, lib, pkgs, ... }:
let
  cfg = config.services.metadata-webhook;
in {

  options = {
    services.metadata-webhook = {
      enable = lib.mkEnableOption "enable the metadata webhook";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      metadataServerPkgs = lib.mkOption {
        type = lib.types.attrs;
        default = (import ../../. {}).project;
        defaultText = "metadata-server pkgs";
        description = ''
          The metadata-server packages and library that should be used.
        '';
        internal = true;
      };
      package = lib.mkOption {
        type = lib.types.package;
        default = cfg.metadataServerPkgs.metadata-webhook.components.exes.metadata-webhook;
      };
      user = lib.mkOption {
        type = lib.types.str;
        # Linux OS user can have a hyphen (`-`) along with systemd service names as standard
        default = "metadata-webhook";
        description = "the user to run as";
      };
      port = lib.mkOption {
        type = lib.types.int;
        default = 8081;
        description = "the port the metadata webhook runs on";
      };
      environmentFile = lib.mkOption {
        type = with lib.types; nullOr str;
        default = null;
        description = ''
          A string path to a systemd environmentFile used to set secrets of:
            METADATA_GITHUB_TOKEN
            METADATA_WEBHOOK_SECRET

          Note that if environmentFile is set, cfg.webHookSecret and cfg.gitHubToken cannot be set.
        '';
      };
      webHookSecret = lib.mkOption {
        type = with lib.types; nullOr str;
        default = null;
        description = ''
          The GitHub webhook secret.
          This option is used to set the environment variable METADATA_WEBHOOK_SECRET in the systemd service unit.

          Note that if cfg.webHookSecret is set, cfg.environmentFile cannot be set.
        '';
      };
      gitHubToken = lib.mkOption {
        type = with lib.types; nullOr str;
        default = null;
        description = ''
          The GitHub token to use to query the GitHub API.
          This option is used to set the environment variable METADATA_GITHUB_TOKEN in the systemd service unit.

          Note that if cfg.gitHubToken is set, cfg.environmentFile cannot be set.
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
    services.metadata-webhook.script = let
      exec = "metadata-webhook";
      cmd = builtins.filter (x: x != "") [
          "${cfg.package}/bin/${exec}"
          "--db ${cfg.postgres.database}"
          "--db-user ${cfg.postgres.user}"
          "--db-host ${cfg.postgres.socketdir}"
          "--db-table ${cfg.postgres.table}"
          "--db-conns ${toString cfg.postgres.numConnections}"
          "--port ${toString cfg.port}"
      ];
    in pkgs.writeShellScript "metadata-webhook" ''
      set -euo pipefail
      echo "Starting ${exec}: ${lib.concatStringsSep "\"\n   echo \"" cmd}"
      echo "..or, once again, in a single line:"
      echo "${toString cmd}"
      exec ${toString cmd}
    '';
    environment.systemPackages = [ cfg.package config.services.postgresql.package ];
    systemd.services.metadata-webhook = {
      path = [ cfg.package pkgs.netcat pkgs.postgresql ];
      preStart = ''
        for x in {1..60}; do
          nc -z localhost ${toString cfg.postgres.port} && break
          echo loop $x: waiting for postgresql 2 sec...
          sleep 2
        done
        sleep 1
      '';
      environment = lib.mkIf (cfg.webHookSecret != null || cfg.gitHubToken != null) {
        METADATA_GITHUB_TOKEN = toString cfg.gitHubToken;
        METADATA_WEBHOOK_SECRET = toString cfg.webHookSecret;
      };
      serviceConfig = {
        EnvironmentFile = lib.mkIf (cfg.environmentFile != null) cfg.environmentFile;
        ExecStart = config.services.metadata-webhook.script;
        DynamicUser = true;
        User = config.services.metadata-webhook.user;
        RuntimeDirectory = "metadata-webhook";
        StateDirectory = "metadata-webhook";
        StandardOutput = "journal";
      };

      wantedBy = [ "multi-user.target" ];
      after = [ "postgres.service" ];
      requires = [ "postgresql.service" ];
    };

    assertions = [
      {
        assertion = !(cfg.environmentFile != null && cfg.webHookSecret != null || cfg.gitHubToken != null);
        message = "The metadata-webhook nixos service config option environmentFile cannot be declared when webHookSecret and/or gitHubToken options are also declared.";
      }
      {
        assertion = !(cfg.environmentFile == null && cfg.webHookSecret == null && cfg.gitHubToken == null);
        message = "Either the metadata-webhook nixos service config option environmentFile or webHookSecret and/or gitHubToken options must be declared.";
      }
    ];
  };
}
