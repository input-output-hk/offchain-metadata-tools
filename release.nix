############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ metadata-server ? { outPath = ./.; rev = pkgs.commonLib.commitIdFromGitRepoOrZero ./.git; }

# Function arguments to pass to the project
, projectArgs ? {
    config = { allowUnfree = false; inHydra = true; };
    gitrev = metadata-server.rev;
    inherit pr borsBuild sourcesOverride;
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

# GitHub PR number (as a string), provided as a Hydra input
, pr ? null

# Can be "staging" or "trying" to indicate that this is a bors jobset
, borsBuild ? null

# Platform filter string for jobset.
, platform ? "all"
}:

assert pkgs.lib.asserts.assertOneOf "platform" platform
  ["all" "linux" "macos" "windows"];

let
  buildNative  = builtins.elem builtins.currentSystem supportedSystems;
  buildLinux   = builtins.elem "x86_64-linux" supportedSystems && buildForPlatform "linux";
  buildMacOS   = builtins.elem "x86_64-darwin" supportedSystems && buildForPlatform "macos";
  buildMusl    = builtins.elem "x86_64-linux" supportedCrossSystems && buildLinux;
  buildWindows = builtins.elem builtins.currentSystem supportedCrossSystems && buildForPlatform "windows";
  buildForPlatform = name: builtins.elem platform ["all" name];
in

with (import pkgs.commonLib.release-lib) {
  inherit pkgs;

  inherit supportedCrossSystems scrubJobs projectArgs;
  supportedSystems =
    pkgs.lib.optional buildLinux "x86_64-linux" ++
    pkgs.lib.optional buildMacOS "x86_64-darwin";

  packageSet = import metadata-server;
  gitrev = metadata-server.rev;
};

with pkgs.lib;

let
  ### OLD
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

        jobs.native.shell.x86_64-linux
        jobs.native.shell-prof.x86_64-linux
      ]
    ))
  # Build the shell derivation in Hydra so that all its dependencies
  # are cached.
  // mapTestOn (packagePlatforms { inherit (project) shell; });
in jobs
