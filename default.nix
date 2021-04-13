{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# override scripts with custom configuration
, customConfig ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix cardano-shell '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
, gitrev ? pkgs.iohkNix.commitIdFromGitRepoOrZero ./.git
}:
with pkgs; with commonLib;
let

  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages offchainMetadataToolsHaskellPackages);
  haskellPackagesMusl64 = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages pkgs.pkgsCross.musl64.offchainMetadataToolsHaskellPackages);
  metadataValidatorGitHubTarball = pkgs.runCommandNoCC "metadata-validator-github-tarball" { buildInputs = [ pkgs.gnutar gzip ]; } ''
    cp ${haskellPackagesMusl64.metadata-validator-github.components.exes.metadata-validator-github}/bin/metadata-validator-github ./
    mkdir -p $out/nix-support
    tar -czvf $out/metadata-validator-github.tar.gz metadata-validator-github
    echo "file binary-dist $out/metadata-validator-github.tar.gz" > $out/nix-support/hydra-build-products
  '';
  tokenMetadataCreatorTarball = pkgs.runCommandNoCC "token-metadata-creator" { buildInputs = [ pkgs.gnutar gzip ]; } ''
    cp ${haskellPackagesMusl64.token-metadata-creator.components.exes.token-metadata-creator}/bin/token-metadata-creator ./
    mkdir -p $out/nix-support
    tar -czvf $out/token-metadata-creator.tar.gz token-metadata-creator
    echo "file binary-dist $out/token-metadata-creator.tar.gz" > $out/nix-support/hydra-build-products
  '';

  nixosTests = recRecurseIntoAttrs (import ./nix/nixos/tests {
    inherit pkgs;
  });

  updateDocsScript = pkgs.callPackage ./scripts/update-docs.nix { inherit (pkgs.haskellPackages) ghcWithPackages; };

  # Scripts for keeping Hackage and Stackage up to date, and CI tasks.
  # The dontRecurseIntoAttrs prevents these from building on hydra
  # as not all of them can work in restricted eval mode (as they
  # are not pure).
  maintainer-scripts = pkgs.dontRecurseIntoAttrs {
    update-docs = pkgs.buildPackages.callPackage ./scripts/update-docs.nix { inherit (pkgs.haskellPackages) ghcWithPackages; };

    # Because this is going to be used to test caching on hydra, it must not
    # use the darcs package from the haskell.nix we are testing.  For that reason
    # it uses `pkgs.buildPackages.callPackage` not `haskell.callPackage`
    # (We could pull in darcs from a known good haskell.nix for hydra to
    # use)
    check-hydra = pkgs.buildPackages.callPackage ./scripts/check-hydra.nix {};
  };

  self = {
    inherit updateDocsScript;
    inherit offchainMetadataToolsHaskellPackages;
    inherit metadataValidatorGitHubTarball tokenMetadataCreatorTarball;
    inherit haskellPackages hydraEvalErrors nixosTests;

    inherit (pkgs.iohkNix) checkCabalProject;

    inherit (haskellPackages.metadata-server.identifier) version;
    inherit (haskellPackages.metadata-server.components.exes) metadata-server;
    inherit (haskellPackages.metadata-webhook.components.exes) metadata-webhook;
    inherit (haskellPackages.metadata-validator-github.components.exes) metadata-validator-github;
    inherit (haskellPackages.token-metadata-creator.components.exes) token-metadata-creator;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
  };
in self
