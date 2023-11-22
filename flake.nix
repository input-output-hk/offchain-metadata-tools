{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/release-23.05";

  outputs = inputs:
    with builtins;
    with inputs.nixpkgs.lib; let
      systems = ["x86_64-linux" "x86_64-darwin"];

      hydraJobs = import "${inputs.self}/release.nix" {
        metadata-server = inputs.self;
        supportedSystems = systems;
      };

      nativePkgs = {
        inherit
          (hydraJobs.native)
          metadata-server
          metadata-sync
          metadata-validator-github
          metadata-webhook
          token-metadata-creator
          ;
      };
    in {
      inherit hydraJobs;

      packages = foldl' (
        acc: pkg:
          recursiveUpdate acc (
            listToAttrs (
              map (
                system:
                  optionalAttrs (hasAttr system pkg.value) (
                    nameValuePair system (setAttrByPath [pkg.name] pkg.value.${system})
                  )
              )
              systems
            )
          )
      ) {} (mapAttrsToList nameValuePair nativePkgs);
    };

  nixConfig = {
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = true;
  };
}
