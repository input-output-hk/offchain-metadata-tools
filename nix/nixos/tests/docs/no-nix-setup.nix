{ pkgs, haskellPackages, ... }:
with pkgs;
let
  docScripts = pkgs.callPackage ../../../../docs/default.nix {};
in
{
  name = "no-nix-setup-test";

  nodes = {
    server = { config, ... }: {
      nixpkgs.pkgs = pkgs;
      imports = [];

      users = {
        mutableUsers = false;

        users = {
          # For ease of debugging the VM as the `root` user
          root.password = "";

          "metadata-server".isSystemUser = true;
        };
      };

      nixpkgs.config.allowUnfree = true;

      environment.systemPackages = [
        docScripts
        haskellPackages.metadata-server.components.exes.metadata-server
        haskellPackages.metadata-webhook.components.exes.metadata-webhook
        vim
        jq
        # ngrok-2
      ];

      services.postgresql = {
        enable = true;
        package = pkgs.postgresql;
      };

      # Modify postgresql service so we can use our own config
      systemd.services.postgresql = {
        preStart =
          let
            cfg = config.services.postgresql;
          in
            lib.mkForce ''
            if ! test -e ${cfg.dataDir}/PG_VERSION; then
              # Cleanup the data directory.
              rm -f ${cfg.dataDir}/*.conf

              # Initialise the database.
              initdb -U ${cfg.superUser} ${lib.concatStringsSep " " cfg.initdbArgs}

              # See postStart!
              touch "${cfg.dataDir}/.first_startup"
            fi

            ${lib.optionalString (cfg.recoveryConfig != null) ''
              ln -sfn "${pkgs.writeText "recovery.conf" cfg.recoveryConfig}" \
                "${cfg.dataDir}/recovery.conf"
            ''}
          '';
        postStart =
          let
            cfg = config.services.postgresql;
          in
            lib.mkForce ''
            PSQL="psql --port=${toString cfg.port}"

            while ! $PSQL -d postgres -c "" 2> /dev/null; do
                if ! kill -0 "$MAINPID"; then exit 1; fi
                sleep 0.1
            done

            if test -e "${cfg.dataDir}/.first_startup"; then
              ${lib.optionalString (cfg.initialScript != null) ''
                $PSQL -f "${cfg.initialScript}" -d postgres
              ''}
              rm -f "${cfg.dataDir}/.first_startup"
            fi
            '';
      };
    };
  };

  testScript =
    ''
    start_all()

    server.wait_for_unit("postgresql.service")
    server.succeed(
        "${docScripts}/bin/no-nix-setup.sh"
    )
    '';
}
