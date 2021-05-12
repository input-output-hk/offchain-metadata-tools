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
        default = (import ../../. {}).project;
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
      extraFlags = lib.mkOption {
        type = with lib.types; listOf str;
        default = [];
        description = ''
          Extra flags to pass to the metadata-server executable.
        '';
        example = ["+RTS" "-M500M" "-RTS"];
      };
      fileStore = {
        folder = lib.mkOption {
          type = lib.types.str;
          description = "The path to the folder where the metadata entries are stored.";
        };
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services.metadata-server.script = let
      exec = "metadata-server";
      cmd = builtins.filter (x: x != "") ([
          "${cfg.package}/bin/${exec}"
          "--folder ${config.services.metadata-server.fileStore.folder}"
      ] ++ cfg.extraFlags);
    in pkgs.writeShellScript "metadata-server" ''
      set -euo pipefail
      echo "Starting ${exec}: ${lib.concatStringsSep "\"\n   echo \"" cmd}"
      echo "..or, once again, in a single line:"
      echo "${toString cmd}"
      exec ${toString cmd}
    '';
    systemd.services.metadata-server = {
      path = [ cfg.package ];
      serviceConfig = {
        ExecStart = config.services.metadata-server.script;
        DynamicUser = true;
        RuntimeDirectory = "metadata-server";
        StateDirectory = "metadata-server";
      };

      wantedBy = [ "multi-user.target" ];
    };
  };
}
