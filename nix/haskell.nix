############################################################################
# Builds Haskell packages with Haskell.nix
#
# This file returns the arguments for `haskell-nix.cabalProject'`.
############################################################################
{ CHaP }:
{ pkgs, lib, config, ... }:
let
  # This exe component postInstall script adds shell completion
  # scripts. These completion scripts will be picked up automatically
  # if the resulting derivation is installed, e.g. by `nix profile install`.
  optparseCompletionPostInstall = exeName: lib.optionalString pkgs.stdenv.hostPlatform.isUnix ''
    bashCompDir="$out/share/bash-completion/completions"
    zshCompDir="$out/share/zsh/vendor-completions"
    fishCompDir="$out/share/fish/vendor_completions.d"
    mkdir -p "$bashCompDir" "$zshCompDir" "$fishCompDir"
    "$out/bin/${exeName}" --bash-completion-script "$out/bin/${exeName}" >"$bashCompDir/${exeName}"
    "$out/bin/${exeName}" --zsh-completion-script "$out/bin/${exeName}" >"$zshCompDir/_${exeName}"
    "$out/bin/${exeName}" --fish-completion-script "$out/bin/${exeName}" >"$fishCompDir/${exeName}.fish"
  '';
in
{
  name = "offchain-metadata-tools";
  src = ../.;
  compiler-nix-name = lib.mkDefault "ghc967";

  # Resolve the cardano-haskell-packages repository (declared in
  # cabal.project) to the CHaP flake input.
  inputMap = { "https://chap.intersectmbo.org/" = CHaP; };

  shell = {
    name = "metadata-shell";
    withHoogle = true;
    tools = {
      cabal = "latest";
      haskell-language-server = "latest";
      hlint = "latest";
    };
    nativeBuildInputs = with pkgs.pkgsBuildBuild; [
      ghcid
      git
      pkg-config
      stylish-haskell
      postgresql # for the metadata-store-postgres/metadata-sync integration tests
    ];
  };

  modules = [
    # NOTE: servant-github-webhook is unmaintained (last release 2019) and
    # its Hackage version does not compile against aeson 2. cabal.project
    # pulls it from the aeson-2-updated input-output-hk fork via a
    # source-repository-package, which haskell.nix builds from the same
    # source -- so no patch module is needed here.

    # Install shell completion scripts alongside the executables.
    {
      packages.metadata-server.components.exes.metadata-server.postInstall =
        optparseCompletionPostInstall "metadata-server";
      packages.metadata-webhook.components.exes.metadata-webhook.postInstall =
        optparseCompletionPostInstall "metadata-webhook";
      packages.metadata-sync.components.exes.metadata-sync.postInstall =
        optparseCompletionPostInstall "metadata-sync";
      packages.metadata-validator-github.components.exes.metadata-validator-github.postInstall =
        optparseCompletionPostInstall "metadata-validator-github";
      packages.token-metadata-creator.components.exes.token-metadata-creator.postInstall =
        optparseCompletionPostInstall "token-metadata-creator";
    }

    # The integration tests need a running PostgreSQL instance; they are
    # run by the NixOS tests (nix/nixos/tests) instead.
    {
      packages.metadata-store-postgres.components.tests.integration-tests.doCheck = false;
      packages.metadata-sync.components.tests.integration-tests.doCheck = false;
    }

    # Musl (fully static) build tweaks.
    ({ lib, pkgs, ... }: lib.mkIf pkgs.stdenv.hostPlatform.isMusl {
      # Haddock is not working for cross builds and is not needed anyway.
      doHaddock = false;
    })
  ];
}
