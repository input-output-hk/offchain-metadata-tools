{
  description = "Tools for handling off-chain metadata for the Cardano blockchain";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    CHaP = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:input-output-hk/flake-compat/fixes";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, haskellNix, iohkNix, CHaP, flake-utils, ... }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      # Systems for which Hydra CI jobs are defined, matching the old release.nix.
      ciSystems = [ "x86_64-linux" "x86_64-darwin" ];
    in
    (flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [
          # iohkNix.overlays.crypto provides secp256k1, libsodium-vrf and libblst
          # and must come before haskellNix.overlay;
          # iohkNix.overlays.haskell-nix-crypto wires them into haskell.nix.
          iohkNix.overlays.crypto
          haskellNix.overlay
          iohkNix.overlays.haskell-nix-crypto
          iohkNix.overlays.utils
          (final: prev: {
            metadataProject = final.haskell-nix.cabalProject'
              (import ./nix/haskell.nix { inherit (inputs) CHaP; });
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        inherit (pkgs) lib;

        project = pkgs.metadataProject;
        profiledProject = project.appendModule {
          modules = [{ enableLibraryProfiling = true; }];
        };
        flake = project.flake { };

        # The executables provided by this repository, and the local package
        # each of them lives in.
        exes = [
          "metadata-server"
          "metadata-sync"
          "metadata-validator-github"
          "metadata-webhook"
          "token-metadata-creator"
        ];
        exePackages = lib.genAttrs exes (name: flake.packages."${name}:exe:${name}");

        # Statically linked (musl) binaries, distributed as tarballs from Hydra.
        muslProject = project.projectCross.musl64;
        staticTarball = name: pkgs.runCommand "${name}-tarball"
          { buildInputs = [ pkgs.gnutar pkgs.gzip ]; } ''
          cp ${muslProject.hsPkgs.${name}.components.exes.${name}}/bin/${name} ./
          mkdir -p $out/nix-support
          tar -czvf $out/${name}.tar.gz ${name}
          echo "file binary-dist $out/${name}.tar.gz" > $out/nix-support/hydra-build-products
        '';
        staticTarballs = lib.optionalAttrs (system == "x86_64-linux") {
          metadata-validator-github-tarball = staticTarball "metadata-validator-github";
          token-metadata-creator-tarball = staticTarball "token-metadata-creator";
        };

        docScripts = pkgs.callPackage ./docs/default.nix { };
        maintainerScripts = lib.optionalAttrs (system == "x86_64-linux") {
          update-docs = pkgs.callPackage ./scripts/update-docs.nix {
            inherit (pkgs.haskellPackages) ghcWithPackages;
            mkdocs = pkgs.mkdocs.overridePythonAttrs (_: {
              doCheck = false;
              dontUsePythonImportsCheck = true;
            });
          };
        };

        nixosTests = lib.optionalAttrs (system == "x86_64-linux")
          (import ./nix/nixos/tests {
            inherit pkgs;
            haskellPackages = project.hsPkgs;
          });

        packages = flake.packages // exePackages // staticTarballs // {
          default = exePackages.metadata-server;
          inherit docScripts;
        } // maintainerScripts;

        ciJobs = lib.optionalAttrs (lib.elem system ciSystems) (
          flake.hydraJobs
          // staticTarballs
          // lib.optionalAttrs (system == "x86_64-linux") {
            inherit nixosTests;
            inherit (maintainerScripts) update-docs;
          }
          // {
            required = pkgs.releaseTools.aggregate {
              name = "required";
              constituents =
                lib.attrValues exePackages
                ++ lib.attrValues (flake.checks or { })
                ++ lib.attrValues staticTarballs
                ++ lib.collect lib.isDerivation nixosTests
                ++ lib.attrValues maintainerScripts
                ++ [ flake.devShells.default ];
            };
          }
        );
      in
      lib.recursiveUpdate flake {
        legacyPackages = pkgs;
        inherit packages project;
        devShells = {
          profiled = profiledProject.shell;
        };
        hydraJobs = ciJobs;
        apps = lib.mapAttrs
          (name: exe: { type = "app"; program = "${exe}/bin/${name}"; })
          exePackages;
      }))
    // {
      nixosModules = {
        metadata-server = ./nix/nixos/metadata-server.nix;
        metadata-webhook = ./nix/nixos/metadata-webhook.nix;
        metadata-sync = ./nix/nixos/metadata-sync.nix;
        default.imports = import ./nix/nixos/module-list.nix;
      };
    };

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = true;
  };
}
