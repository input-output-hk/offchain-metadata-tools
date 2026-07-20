{ pkgs, ... }:
with pkgs;
let
  docScripts = pkgs.callPackage ../../../../docs/default.nix {};
in
{
  name = "nix-setup-test";

  nodes = {
    server = { config, ... }: {
      # Compose the tutorial config as a module (deep merge) rather than `//`,
      # which would shallow-replace environment.systemPackages (dropping jq)
      # when metadata-config.nix also sets an environment.* key.
      imports = [
        (import "${docScripts}/metadata-config.nix" { inherit config pkgs; sources = { metadata-server = ../../../..; }; })
      ];

      nixpkgs.pkgs = pkgs;

      nix.nixPath =
        [
          "nixpkgs=${pkgs.path}"
          "nixos-config=/etc/nixos/configuration.nix"
          "/nix/var/nix/profiles/per-user/root/channels"
        ];

      environment.systemPackages = [
        docScripts
        jq
      ];

      users.mutableUsers = false;
    };
  };

  testScript =
    ''
    start_all()

    server.wait_for_unit("metadata-server.service")
    server.succeed(
        "${docScripts}/bin/nix-setup-test.sh"
    )
    '';
}
