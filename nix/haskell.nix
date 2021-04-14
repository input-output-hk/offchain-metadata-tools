############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, stdenv
, pkgs
, haskell-nix
, buildPackages
, config ? {}
# Enable profiling
, profiling ? config.haskellNix.profiling or false
# Project top-level source tree
, src
# GitHub PR number (when building a PR jobset on Hydra)
, pr ? null
# Bors job type (when building a bors jobset on Hydra)
, borsBuild ? null
# Version info, to be passed when not building from a git work tree
, gitrev ? null
, libsodium ? pkgs.libsodium
}:
let
  haskell = pkgs.haskell-nix;

  # Chop out a subdirectory of the source, so that the package is only
  # rebuilt when something in the subdirectory changes.
  filterSubDir = subDir: {
    src = haskell.haskellLib.cleanSourceWith { inherit src subDir; };
    package.isProject = true;  # fixme: Haskell.nix
  };

  pkg-set = haskell-nix.cabalProject ({
    inherit src;
    compiler-nix-name = "ghc8104";
    modules = [
      # Add source filtering to local packages
      {
        packages.metadata-lib = filterSubDir "metadata-lib";
        packages.metadata-server = filterSubDir "metadata-server";
        packages.metadata-webhook = filterSubDir "metadata-webhook";
        packages.metadata-store-postgres = filterSubDir "metadata-store-postgres";
        packages.metadata-validator-github = filterSubDir "metadata-validator-github";
      }
      # Enable release flag (optimization and -Werror) on all local packages
      {
        packages.metadata-lib.flags.release = true;
        packages.metadata-server.flags.release = true;
        packages.metadata-webhook.flags.release = true;
        packages.metadata-store-postgres.flags.release = true;
        packages.metadata-validator-github.flags.release = true;
      }
      {
        packages.metadata-server.components.exes.metadata-server.postInstall = optparseCompletionPostInstall;
        packages.metadata-webhook.components.exes.metadata-webhook.postInstall = optparseCompletionPostInstall;
        packages.metadata-validator-github.components.exes.metadata-validator-github.postInstall = optparseCompletionPostInstall;
      }
      # Enable profiling on executables if the profiling argument is set.
      (lib.optionalAttrs profiling {
        enableLibraryProfiling = true;
        packages.metadata-server.components.exes.metadata-server.enableExecutableProfiling = true;
        packages.metadata-webhook.components.exes.metadata-webhook.enableExecutableProfiling = true;
        packages.metadata-validator-github.components.exes.metadata-validator-github.enableExecutableProfiling = true;
      })

      # Musl libc fully static build
      (lib.optionalAttrs stdenv.hostPlatform.isMusl (let
        staticLibs = with pkgs; [ zlib openssl libffi gmp6 libsodium ];

        # Module options which add GHC flags and libraries for a fully static build
        fullyStaticOptions = {
          enableShared = false;
          enableStatic = true;
          configureFlags = map (drv: "--ghc-option=-optl=-L${drv}/lib") staticLibs;
        };
      in {
        # Apply fully static options to our Haskell executables
        packages.metadata-server.components.benchmarks.restore = fullyStaticOptions;
        packages.metadata-webhook.components.exes.cardano-wallet = fullyStaticOptions;
        packages.metadata-validator-github.components.tests.integration = fullyStaticOptions;

        # systemd can't be statically linked - disable lobemo-scribe-journal
        packages.cardano-config.flags.systemd = false;
        packages.cardano-node.flags.systemd = false;

        # Haddock not working for cross builds and is not needed anyway
        doHaddock = false;
      }))

      # Allow installation of a newer version of Win32 than what is
      # included with GHC. The packages in this list are all those
      # installed with GHC, except for Win32.
      { nonReinstallablePkgs =
        [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
          "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
          # ghcjs custom packages
          "ghcjs-prim" "ghcjs-th"
          "ghc-boot"
          "ghc" "array" "binary" "bytestring" "containers"
          "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
          # "ghci" "haskeline"
          "hpc"
          "mtl" "parsec" "text" "transformers"
          "xhtml"
          # "stm" "terminfo"
        ];
      }
      {
        packages.metadata-store-postgres.components.tests.integration-tests.doCheck = false;
      }
    ];
  });

  # This exe component postInstall script adds shell completion
  # scripts. These completion
  # scripts will be picked up automatically if the resulting
  # derivation is installed, e.g. by `nix-env -i`.
  optparseCompletionPostInstall = lib.optionalString stdenv.hostPlatform.isUnix ''
    exeName=$(ls -1 $out/bin | head -n1)  # fixme add $exeName to Haskell.nix
    bashCompDir="$out/share/bash-completion/completions"
    zshCompDir="$out/share/zsh/vendor-completions"
    fishCompDir="$out/share/fish/vendor_completions.d"
    mkdir -p "$bashCompDir" "$zshCompDir" "$fishCompDir"
    "$out/bin/$exeName" --bash-completion-script "$out/bin/$exeName" >"$bashCompDir/$exeName"
    "$out/bin/$exeName" --zsh-completion-script "$out/bin/$exeName" >"$zshCompDir/_$exeName"
    "$out/bin/$exeName" --fish-completion-script "$out/bin/$exeName" >"$fishCompDir/$exeName.fish"
  '';

  # setGitRev is a postInstall script to stamp executables with
  # version info. It uses the "gitrev" argument, if set. Otherwise,
  # the revision is sourced from the local git work tree.
  setGitRev = ''${haskellBuildUtils}/bin/set-git-rev "${gitrev'}" $out/bin/*'';
  # package with libsodium:
  setLibSodium = "ln -s ${libsodium}/bin/libsodium-23.dll $out/bin/libsodium-23.dll";
  gitrev' = if (gitrev == null)
    then buildPackages.commonLib.commitIdFromGitRepoOrZero ../.git
    else gitrev;
  haskellBuildUtils = buildPackages.haskellBuildUtils.package;
in
  pkg-set
