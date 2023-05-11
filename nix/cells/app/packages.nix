{
  cell,
  inputs,
}: let
  inherit (
    import "${inputs.self}/release.nix" {
      metadata-server = inputs.self;
      supportedSystems = [inputs.nixpkgs.system];
    }
  ) native;
in
builtins.mapAttrs (_: p: p.${inputs.nixpkgs.system}) {
  inherit (native)
    metadata-server
    metadata-sync
    metadata-validator-github
    metadata-webhook
    token-metadata-creator
    ;
}
