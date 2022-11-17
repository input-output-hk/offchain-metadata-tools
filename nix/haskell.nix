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
, libsodium-vrf ? pkgs.libsodium-vrf
}:
let
  haskell = pkgs.haskell-nix;

  pkg-set = haskell-nix.cabalProject ({
    inherit src;
    compiler-nix-name = "ghc8107";
    inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = pkgs.commonLib.sources.CHaP; };
    modules = [
      # Enable release flag (optimization and -Werror) on all local packages
      {
        packages.metadata-lib.flags.release = true;
        packages.metadata-server.flags.release = true;
        packages.metadata-webhook.flags.release = true;
        packages.metadata-sync.flags.release = true;
        packages.metadata-store-postgres.flags.release = true;
        packages.metadata-validator-github.flags.release = true;
      }
      {
        packages.metadata-server.components.exes.metadata-server.postInstall = optparseCompletionPostInstall;
        packages.metadata-webhook.components.exes.metadata-webhook.postInstall = optparseCompletionPostInstall;
        packages.metadata-sync.components.exes.metadata-sync.postInstall = optparseCompletionPostInstall;
        packages.metadata-validator-github.components.exes.metadata-validator-github.postInstall = optparseCompletionPostInstall;
      }
      # Enable profiling on executables if the profiling argument is set.
      (lib.optionalAttrs profiling {
        enableLibraryProfiling = true;
        packages.metadata-server.components.exes.metadata-server.enableProfiling = true;
        packages.metadata-webhook.components.exes.metadata-webhook.enableProfiling = true;
        packages.metadata-sync.components.exes.metadata-sync.enableProfiling = true;
        packages.metadata-validator-github.components.exes.metadata-validator-github.enableProfiling = true;
        # Needed for profiled builds to fix an issue loading recursion-schemes part of makeBaseFunctor
        # that is missing from the `_p` output.  See https://gitlab.haskell.org/ghc/ghc/-/issues/18320
        packages.plutus-core.components.library.ghcOptions = [ "-fexternal-interpreter" ];
      })

      # Musl libc fully static build
      (lib.optionalAttrs stdenv.hostPlatform.isMusl (let
        staticLibs = with pkgs; [ zlib openssl libffi gmp6 libsodium-vrf ];

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
        packages.cardano-node.flags.systemd = false;

        # Haddock not working for cross builds and is not needed anyway
        doHaddock = false;
      }))

      ({ pkgs, ... }: {
        # Use the VRF fork of libsodium
        packages.cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
        packages.cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
      })

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
        packages.metadata-sync.components.tests.integration-tests.doCheck = false;
      }
      ({ pkgs, ... }: {
        packages = lib.genAttrs [ "cardano-config" ] (_: {
          components.library.build-tools = [ pkgs.buildPackages.buildPackages.gitMinimal ];
        });
      })
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
  setGitRev = ''${buildPackages.haskellBuildUtils}/bin/set-git-rev "${gitrev'}" $out/bin/*'';
  # package with libsodium:
  setLibSodium = "ln -s ${libsodium-vrf}/bin/libsodium-23.dll $out/bin/libsodium-23.dll";
  gitrev' = if (gitrev == null)
    then buildPackages.commonLib.commitIdFromGitRepoOrZero ../.git
    else gitrev;
in
  pkg-set
