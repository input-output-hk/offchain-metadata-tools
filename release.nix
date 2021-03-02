############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ metadata-server ? { outPath = ./.; rev = "abcdef"; }

# Function arguments to pass to the project
, projectArgs ? {
    config = { allowUnfree = false; inHydra = true; };
    inherit sourcesOverride;
  }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]

# The systems used for cross-compiling
, supportedCrossSystems ? [ "x86_64-linux" ]

# A Hydra option
, scrubJobs ? true

# Dependencies overrides
, sourcesOverride ? {}

# Import pkgs, including IOHK common nix lib
, pkgs ? import ./nix { inherit sourcesOverride; }
}:

with (import pkgs.commonLib.release-lib) {
  inherit pkgs;

  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import metadata-server;
  gitrev = metadata-server.rev;
};

with pkgs.lib;

let
  nonDefaultBuildSystems = tail supportedSystems;
  # Paths or prefixes of paths of derivations to build only on the default system (ie. linux on hydra):
  onlyBuildOnDefaultSystem = [
    ["nixosTests"]
  ];
  testsSupportedSystems = [ "x86_64-linux" ];
  collectTests = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  collectJobs' = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  collectJobs = ds: concatLists (
    mapAttrsToList (packageName: package:
      map (drv: drv // { inherit packageName; }) (collectJobs' package)
    ) ds);

  filteredBuilds = mapAttrsRecursiveCond (a: !(isList a)) (path: value:
    if (any (p: take (length p) path == p) onlyBuildOnDefaultSystem) then filter (s: !(elem s nonDefaultBuildSystems)) value else value)
    (packagePlatforms project);

  inherit (systems.examples) musl64;

  jobs = {
    native = mapTestOn filteredBuilds;
  } // (mkRequiredJob (
      collectTests jobs.native.checks.tests ++
      collectTests jobs.native.benchmarks ++
      collectJobs jobs.native.nixosTests ++
      [
        jobs.native.metadata-server.x86_64-linux
        jobs.native.metadata-webhook.x86_64-linux
        jobs.native.metadata-validator-github.x86_64-linux
        jobs.native.haskellPackages.metadata-lib.components.library.x86_64-linux
        jobs.native.haskellPackages.metadata-lib.components.tests.unit-tests.x86_64-linux
        jobs.native.haskellPackages.metadata-store-postgres.components.library.x86_64-linux
      ]
    ))
  # Build the shell derivation in Hydra so that all its dependencies
  # are cached.
  // mapTestOn (packagePlatforms { inherit (project) shell; });
in jobs
